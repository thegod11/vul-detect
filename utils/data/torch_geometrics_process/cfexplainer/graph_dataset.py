import sys, json, os
import os.path as osp
from typing import Callable, Dict, Iterable, List, Optional, Tuple, Union
import pickle as pkl
from pathlib import Path
from glob import glob
from functools import partial
import numpy as np
import pandas as pd
import torch
from torch_geometric.data import Dataset, Data, Batch
from tqdm.std import trange
from transformers import (BertConfig, BertForMaskedLM, BertTokenizer,
                          GPT2Config, GPT2LMHeadModel, GPT2Tokenizer,
                          OpenAIGPTConfig, OpenAIGPTLMHeadModel, OpenAIGPTTokenizer,
                          RobertaConfig, RobertaForSequenceClassification, RobertaTokenizer,
                          DistilBertConfig, DistilBertForMaskedLM, DistilBertTokenizer,
                          T5Config, T5ForConditionalGeneration, T5Tokenizer)

from .helpers import utils
from .helpers import joern
from tqdm import tqdm
# from data_pre import bigvul
dataset_name = "CVEfixes"

class VulGraphDataset(Dataset):
    def __init__(self, root: Optional[str] = f"storage/processed/{dataset_name}", 
                 transform: Optional[Callable] = None, pre_transform: Optional[Callable] = None, pre_filter: Optional[Callable] = None, log: bool = True, 
                 encoder = None, tokenizer = None, partition = None,
                 vulonly = False, sample = -1, splits = "default",
                 filtered_dataset = None
                 ):
        os.makedirs(root, exist_ok=True)
        
        self.encoder = encoder
        self.word_embeddings = self.encoder.roberta.embeddings.word_embeddings.weight.detach().cpu().numpy() if self.encoder is not None else None
        self.root = root
        self.tokenizer = tokenizer
        self.partition = partition
        
        self.vulonly = vulonly
        self.sample = sample
        self.splits = splits
        self.df = filtered_dataset
        if os.path.exists(self.processed_paths[0]):
            os.remove(self.processed_paths[0])
        
        super().__init__(root, transform, pre_transform, pre_filter, log)
        
        self.data_list = torch.load(self.processed_paths[0])
        
    @property
    def processed_dir(self) -> str:
        return osp.join(self.root, f'{self.partition}_processed')
    
    @property
    def processed_file_names(self) -> Union[str, List[str], Tuple]:
        return 'data.pt'
    
    def data_build(self, row, data_list, edge_type):
        _id = self.idx2id[row.idx]
        lan = "c" if "programming_language" not in row.keys() or row.programming_language == "C" else "java" if row.programming_language == "Java" else "cpp"
        n, e = self.feature_extraction(VulGraphDataset.itempath(_id, lan), edge_type)
        x = np.array(list(n.subseq_feat.values))
        edge_index = np.array(e)
        code_graph = Data(x=torch.FloatTensor(x), edge_index=torch.LongTensor(edge_index))
            
        # n["vuln"] = n.id.map(self.get_vuln_indices(_id)).fillna(0)
        # code_graph.__setitem__("_VULN", torch.Tensor(n["vuln"].astype(int).to_numpy()))
        code_graph.__setitem__("_LINE", torch.Tensor(n["id"].astype(int).to_numpy()))
        code_graph.__setitem__("_SAMPLE", torch.Tensor([_id] * len(n)))
        code_graph.__setitem__("_GTYPE", edge_type)
        data_list.append(code_graph)
        return code_graph
    
    def process(self):
        # Get finished samples
        self.finished = [
            int(Path(i).name.split(".")[0])
            for i in glob(str(utils.processed_dir() / f"{dataset_name}/code/*nodes*"))
        ]
        # self.df = bigvul(splits=self.splits)
        # self.df = self.df[self.df.label == self.partition]
        self.df = self.df.rename({"idx": "id"} ,axis='columns')
        self.df = self.df[self.df.id.isin(self.finished)]

        # Balance set
        vul = self.df[self.df.target == 1]
        nonvul = self.df[self.df.target == 0].sample(len(vul), random_state=0) if len(self.df[self.df.target == 0]) >= len(vul) else self.df[self.df.target == 0]
        self.df = pd.concat([vul, nonvul])

        # Small sample (for debugging):
        if self.sample > 0:
            self.df = self.df.sample(self.sample, random_state=0)

        # Filter only vulnerable
        if self.vulonly:
            self.df = self.df[self.df.target == 1]

        # Filter out samples with no lineNumber from Joern output
        self.df["valid"] = utils.dfmp(
            self.df, VulGraphDataset.check_validity, ["id", "programming_language"], desc="Validate Samples: "
        )
        self.df = self.df[self.df.valid]

        # Get mapping from index to sample ID.
        self.df = self.df.reset_index(drop=True).reset_index()
        self.df = self.df.rename(columns={"index": "idx"})
        self.idx2id = pd.Series(self.df.id.values, index=self.df.idx).to_dict()

        data_list = []
        tqdm.pandas()
        self.df["torch_geometrics_data"] = self.df.progress_apply(lambda row: [self.data_build(row, data_list, e) for e in ["ast", "cfgcdg", "pdg"]], axis=1)

        print(f'Saving in {os.path.join(self.processed_dir, f"{dataset}_dataframe.pkl")} .....')
        self.df.to_pickle(os.path.join(self.processed_dir, f"{dataset}_dataframe.pkl"))
        torch.save(data_list, self.processed_paths[0])
        
    def len(self) -> int:
        return len(self.data_list)

    def get(self, idx: int) -> Data:
        return self.data_list[idx]
    
    def itempath(_id, lan = "c"):
        """Get itempath path from item id."""
        # _id = self.idx2id[row.idx]
        # lan = "c" if row.programming_language == "C" else "java" if row.programming_language == "Java" else "cpp"
        return utils.processed_dir() / f"{dataset_name}/code/{_id}.{lan}"
    
    def check_validity(item):
        """Check whether sample with id=_id has node/edges.

        Example:
        _id = 1320
        with open(str(utils.processed_dir() / f"bigvul/before/{_id}.c") + ".nodes.json", "r") as f:
            nodes = json.load(f)
        """
        valid = 0
        _id = item["id"]
        programming_language = item["programming_language"]

        lan = "c" if programming_language == "C" else "java" if programming_language == "Java" else "cpp"
        try:
            with open(str(VulGraphDataset.itempath(_id, lan)) + ".nodes.json", "r") as f:
                nodes = json.load(f)
                lineNums = set()
                for n in nodes:
                    if "lineNumber" in n.keys():
                        lineNums.add(n["lineNumber"])
                        if len(lineNums) > 1:
                            valid = 1
                            break
                if valid == 0:
                    return False
            with open(str(VulGraphDataset.itempath(_id, lan)) + ".edges.json", "r") as f:
                edges = json.load(f)
                edge_set = set([i[2] for i in edges])
                if "REACHING_DEF" not in edge_set and "CDG" not in edge_set:
                    return False
                return True
        except Exception as E:
            print(E, str(VulGraphDataset.itempath(_id, lan)))
            return False
        
    def get_vuln_indices(self, _id):
        """Obtain vulnerable lines from sample ID."""
        df = self.df[self.df.idx == _id]
        removed = df.removed.item()
        return dict([(i, 1) for i in removed])
    
    def feature_extraction(self, filepath, edge_type="all"):
            # 生成缓存文件路径
            cache_name = "_".join(str(filepath).split("/")[-3:])
            cachefp = utils.get_dir(utils.cache_dir() / "vul_graph_feat") / Path(cache_name).stem
            
            # 从Joern加载节点和边
            nodes, edges = joern.get_node_edges(filepath, edge_type=edge_type)
            
            # 处理节点以提取特征
            # 1. 按代码长度降序排序
            # 2. 按行号分组并取每组的第一个节点
            subseq = (
                nodes.sort_values(by="code", key=lambda x: x.str.len(), ascending=False)
                .groupby("lineNumber")
                .head(1)
            )
            # 3. 选择需要的列并组合local_type和code
            subseq = subseq[["lineNumber", "code", "local_type"]].copy()
            subseq.code = subseq.local_type + " " + subseq.code
            subseq = subseq.drop(columns="local_type")
            # 4. 删除空值和空字符串
            subseq = subseq[~subseq.eq("").any(axis='columns')]
            subseq = subseq[subseq.code != " "]
            subseq = subseq[subseq.code.notnull()]
            # 5. 按行号排序
            subseq.lineNumber = subseq.lineNumber.astype(int)
            subseq = subseq.sort_values("lineNumber")
            # 6. 处理代码字符串，添加token并转换为ID
            subseq.code = subseq.code.apply(lambda s: ' '.join(s.split()))
            subseq.code = subseq.code.apply(lambda s: [self.tokenizer.cls_token] + self.tokenizer.tokenize(s) + [self.tokenizer.sep_token])
            subseq["code_feat"] = subseq.code.apply(lambda tokens: self.tokenizer.convert_tokens_to_ids(tokens))
            subseq.code = subseq.code.apply(lambda tokens: ' '.join(tokens))
            # 7. 计算每个节点的特征向量
            subseq.code_feat = subseq.code_feat.apply(lambda token_ids: np.mean(self.word_embeddings[token_ids], axis=0))  # [22, 768] 22个节点，每个节点的embedding是768维
            subseq_feat = subseq.drop(columns="code")   
            subseq = subseq.drop(columns="code_feat")
            subseq = subseq.set_index("lineNumber").to_dict()["code"]
            subseq_feat = subseq_feat.set_index("lineNumber").to_dict()["code_feat"]
    
            # 处理节点和边以获取行号
            # 1. 过滤掉没有行号的节点
            nodesline = nodes[nodes.lineNumber != ""].copy()
            nodesline.lineNumber = nodesline.lineNumber.astype(int)
            # 2. 按代码长度降序排序，按行号分组并取每组的第一个节点
            nodesline = (
                nodesline.sort_values(by="code", key=lambda x: x.str.len(), ascending=False)
                .groupby("lineNumber")
                .head(1)
            )
            # 3. 复制边并设置节点ID
            edgesline = edges.copy()
            edgesline.innode = edgesline.line_in
            edgesline.outnode = edgesline.line_out
            nodesline.id = nodesline.lineNumber
            # 4. 生成关系依赖图并删除孤立节点
            edgesline = joern.rdg(edgesline, edge_type)
            nodesline = joern.drop_lone_nodes(nodesline, edgesline)
            
            # 删除重复的边并处理边类型
            # 1. 删除重复的边
            edgesline = edgesline.drop_duplicates(subset=["innode", "outnode", "etype"])
            # 2. 将REACHING_DEF类型的边转换为DDG
            edgesline["etype"] = edgesline.apply(
                lambda x: "DDG" if x.etype == "REACHING_DEF" else x.etype, axis=1
            )
            # 3. 过滤掉无效的边
            edgesline = edgesline[edgesline.innode.apply(lambda x: isinstance(x, float))]
            edgesline = edgesline[edgesline.outnode.apply(lambda x: isinstance(x, float))]
            # 4. 生成反向边并合并
            edgesline_reverse = edgesline[["innode", "outnode", "etype"]].copy()
            edgesline_reverse.columns = ["outnode", "innode", "etype"]
            uedge = pd.concat([edgesline, edgesline_reverse])
            uedge = uedge[uedge.innode != uedge.outnode]
            # 5. 按节点和边类型分组并聚合
            uedge = uedge.groupby(["innode", "etype"]).agg({"outnode": set})
            uedge = uedge.reset_index()
            
            # 处理控制和数据依赖
            if len(uedge) > 0:
                # 1. 透视表格，将边类型作为列
                uedge = uedge.pivot(index="innode", columns="etype", values="outnode")
                # 2. 添加缺失的列
                if "DDG" not in uedge.columns:
                    uedge["DDG"] = None
                if "CDG" not in uedge.columns:
                    uedge["CDG"] = None
                # 3. 重置索引并重命名列
                uedge = uedge.reset_index()[["innode", "CDG", "DDG"]]
                uedge.columns = ["lineNumber", "control", "data"]
                # 4. 将集合转换为列表
                uedge.control = uedge.control.apply(
                    lambda x: list(x) if isinstance(x, set) else []
                )
                uedge.data = uedge.data.apply(lambda x: list(x) if isinstance(x, set) else [])
                # 5. 转换为字典
                data = uedge.set_index("lineNumber").to_dict()["data"]
                control = uedge.set_index("lineNumber").to_dict()["control"]
            else:
                data = {}
                control = {}
    
            # 生成程序依赖图（PDG）在结点的 control 和 data 信息中
            # 1. 复制节点并排序
            pdg_nodes = nodesline.copy()
            pdg_nodes = pdg_nodes[["id"]].sort_values("id")
            # 2. 添加特征和依赖信息
            pdg_nodes["subseq"] = pdg_nodes.id.map(subseq).fillna("")
            pdg_nodes["subseq_feat"] = pdg_nodes.id.map(subseq_feat).fillna("")
            pdg_nodes["data"] = pdg_nodes.id.map(data)
            pdg_nodes["control"] = pdg_nodes.id.map(control)
            # 3. 复制边并重置索引
            pdg_edges = edgesline.copy()
            pdg_nodes = pdg_nodes.reset_index(drop=True).reset_index()
            # 4. 生成节点索引字典并映射边的节点
            pdg_dict = pd.Series(pdg_nodes.index.values, index=pdg_nodes.id).to_dict()
            pdg_edges.innode = pdg_edges.innode.map(pdg_dict)
            pdg_edges.outnode = pdg_edges.outnode.map(pdg_dict)
            # 5. 删除无效的边并转换为列表
            pdg_edges = pdg_edges.dropna()
            pdg_edges = (pdg_edges.outnode.tolist(), pdg_edges.innode.tolist())
    
            # 缓存PDG节点和边
            with open(cachefp, "wb") as f:
                pkl.dump([pdg_nodes, pdg_edges], f)
            
            # 返回PDG节点和边
            return pdg_nodes, pdg_edges


def collate(data_list):
    batch = Batch.from_data_list(data_list)
    return batch


if __name__ == '__main__':
    MODEL_CLASSES = {
        'gpt2': (GPT2Config, GPT2LMHeadModel, GPT2Tokenizer),
        'openai-gpt': (OpenAIGPTConfig, OpenAIGPTLMHeadModel, OpenAIGPTTokenizer),
        'bert': (BertConfig, BertForMaskedLM, BertTokenizer),
        'roberta': (RobertaConfig, RobertaForSequenceClassification, RobertaTokenizer),
        'distilbert': (DistilBertConfig, DistilBertForMaskedLM, DistilBertTokenizer),
        't5': (T5Config, T5ForConditionalGeneration, T5Tokenizer)
    }
    
    model_type = "roberta"
    model_name_or_path = "microsoft/graphcodebert-base"
    tokenizer_name = "microsoft/graphcodebert-base"
    
    partition = None
    
    config_class, model_class, tokenizer_class = MODEL_CLASSES[model_type]
    config = config_class.from_pretrained(model_name_or_path)
    tokenizer = tokenizer_class.from_pretrained(tokenizer_name)

    language_model = model_class.from_pretrained(model_name_or_path, from_tf=bool('.ckpt' in model_name_or_path), config=config)
    
    dataset = VulGraphDataset(root=str(utils.processed_dir() / "vul_graph_dataset"), encoder=language_model, tokenizer=tokenizer, partition=partition)
    print(dataset)
    print(dataset.data_list[0])
    print(dataset.data_list[0].x)
    print(dataset.data_list[0].edge_index)
    print(dataset.data_list[0]._SAMPLE)
