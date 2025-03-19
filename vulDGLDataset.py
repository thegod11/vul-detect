import dgl
from dgl.data import DGLDataset
import os
import torch
import pandas as pd
import numpy as np
from utils.data.torch_geometrics_process.cfexplainer.helpers.utils import dfmp
from functools import partial
import scipy.sparse as sp
from tqdm import tqdm
from torch.utils.data import Sampler
from collections import defaultdict
from dgl.dataloading import GraphCollator

import numpy as np
import torch
from torch.utils.data import Sampler
from collections import defaultdict

class BalancedBatchSampler(Sampler):
    def __init__(self, dataset, batch_size):
        self.dataset = dataset
        self.batch_size = batch_size

        # 按语言和漏洞类型组织索引
        self.lang_vul_map = defaultdict(lambda: defaultdict(list))
        for idx in range(len(dataset)):
            cwe = dataset.cve_ids[idx]
            lang = dataset.languages[idx]
            self.lang_vul_map[lang][cwe].append(idx)

        # 统计信息
        self.langs = list(self.lang_vul_map.keys())
        self.lang_ratio = 1 / len(self.langs)
        print(f"Language-cve distribution: { {k: len(v) for k,v in self.lang_vul_map.items()} }")

    def __iter__(self):
        num_batches = len(self.dataset) // self.batch_size

        for _ in range(num_batches):
            batch = []
            
            # **随机调整每个语言的比例**
            per_lang_base = int(self.batch_size * self.lang_ratio)
            per_lang = np.random.randint(int(per_lang_base * 0.8), int(per_lang_base * 1.2) + 1)
            
            # **打乱语言顺序**
            np.random.shuffle(self.langs)
            for lang in self.langs:
                available_cwes = list(self.lang_vul_map[lang].keys())

                # **随机打乱 CWE 漏洞类型**
                np.random.shuffle(available_cwes)

                # **动态选择 CWE 数量**
                num_cwe_to_sample = np.random.randint(10, min(250, len(available_cwes)) + 1)
                selected_cwes = available_cwes[:num_cwe_to_sample]

                # **从每个漏洞类型中采样**
                for cwe in selected_cwes:
                    candidates = self.lang_vul_map[lang][cwe]
                    if len(candidates) > 0:
                        num_samples_per_cwe = np.random.randint(1, max(2, per_lang // len(selected_cwes)))
                        batch.extend(np.random.choice(candidates, 
                                                      size=min(num_samples_per_cwe, len(candidates)), 
                                                      replace=False))

            # **确保 batch 大小**
            batch = batch[:self.batch_size]
            np.random.shuffle(batch)  # 最终再随机打乱 batch 内部的顺序
            yield batch

    def __len__(self):
        return (len(self.dataset) + self.batch_size - 1) // self.batch_size
    
def sparse_mx_to_torch_sparse_tensor(sparse_mx):
    """Convert a scipy sparse matrix to a torch sparse tensor."""
    sparse_mx = sparse_mx.tocoo().astype(np.float32)
    indices = torch.from_numpy(
        np.vstack((sparse_mx.row, sparse_mx.col)).astype(np.int64))
    values = torch.from_numpy(sparse_mx.data)
    shape = torch.Size(sparse_mx.shape)
    return torch.sparse.FloatTensor(indices, values, shape)

def normalize_adj(row, col, num_nodes):
    """Symmetrically normalize adjacency matrix."""
    data = torch.ones(row.size(0))

    adj = sp.coo_matrix((data, (row, col)), shape=(num_nodes, num_nodes,))
    rowsum = np.array(adj.sum(1))

    d_inv_sqrt = np.power(rowsum, -0.5).flatten()
    d_inv_sqrt[np.isinf(d_inv_sqrt)] = 0.
    d_mat_inv_sqrt = sp.diags(d_inv_sqrt)
    return adj.dot(d_mat_inv_sqrt).transpose().dot(d_mat_inv_sqrt).tocoo()

def re_featuresv2(adj, features, K, start_hops=0):
    nodes_features = torch.empty(features.shape[0], 1, K + 1 - start_hops, features.shape[1])

    for i in range(features.shape[0]):  # self
        nodes_features[i, 0, 0, :] = features[i]

    x = features + torch.zeros_like(features)

    for i in range(K):  # 0:1-hop，1：2:-hop....k-1:K-hop

        x = torch.matmul(adj, x)
        if i >= start_hops:
            for index in range(features.shape[0]):
                nodes_features[index, 0, i + 1 - start_hops, :] = x[index]

    nodes_features = nodes_features.squeeze()
    return nodes_features  # size= (N, 1, K+1-start_hops, d )

def process_to_dgl(data_list, row):  # 在 data list 中添加 dgl 数据
    graph_data = {
                ('node', 'ast', 'node'): (row["torch_geometrics_data"][0].edge_index[0], row["torch_geometrics_data"][0].edge_index[1]),
                ('node', 'cfgcdg', 'node'): (row["torch_geometrics_data"][1].edge_index[0], row["torch_geometrics_data"][1].edge_index[1]),
                ('node', 'pdg', 'node'): (row["torch_geometrics_data"][2].edge_index[0], row["torch_geometrics_data"][2].edge_index[1])
            }
    het_graph = dgl.heterograph(graph_data)

    if het_graph.num_nodes() != row["torch_geometrics_data"][0].x.shape[0]:
        return
    
    het_graph.ndata['feature'] = row["torch_geometrics_data"][0].x

    meta_path_adj_with_normalize_adj = {
                'ast': sparse_mx_to_torch_sparse_tensor(normalize_adj(row["torch_geometrics_data"][0].edge_index[0], row["torch_geometrics_data"][0].edge_index[1], het_graph.num_nodes())),
                'cfgcdg': sparse_mx_to_torch_sparse_tensor(normalize_adj(row["torch_geometrics_data"][1].edge_index[0], row["torch_geometrics_data"][1].edge_index[1], het_graph.num_nodes())),
                'pdg': sparse_mx_to_torch_sparse_tensor(normalize_adj(row["torch_geometrics_data"][2].edge_index[0], row["torch_geometrics_data"][2].edge_index[1], het_graph.num_nodes()))
            }

    multi_hop_features = None
    for max_hop in range(1, 10):
        multi_hop_features_with_process_feature_with_normalize_adj = [re_featuresv2(adj=meta_path_adj_with_normalize_adj[mp], features=row["torch_geometrics_data"][0].x, K=max_hop, start_hops=0) for mp in meta_path_adj_with_normalize_adj]

        multi_hop_features_with_process_feature_with_normalize_adj = torch.stack(multi_hop_features_with_process_feature_with_normalize_adj).permute(1, 0, 2, 3)
        if max_hop == 9:
            multi_hop_features = multi_hop_features_with_process_feature_with_normalize_adj

    het_graph.ndata['multi_hop_feature'] = multi_hop_features
    data_list.append(het_graph)
    locals().clear()

class vulDGLDataset_ds(DGLDataset):
    def __init__(self, name, raw_dataframe_path=None, url=None, raw_dir=None, save_dir=None, 
                 hash_key=..., force_reload=False, verbose=False, transform=None):
        if not os.path.exists(raw_dataframe_path):
            raise FileNotFoundError(f"Dataframe path {raw_dataframe_path} not exist!")
        
        self.raw_dataframe_path = raw_dataframe_path
        super().__init__(name, url, raw_dir, save_dir, hash_key, force_reload, verbose, transform)
        
        # 新增属性
        self.category = "node"
        self.vul_lang_map = defaultdict(lambda: defaultdict(list))  # {cve_id: {lang: [indices]}}
        self.languages = []  # 记录每个样本的语言
        self.cve_ids = []    # 记录每个样本的漏洞类型
        self.funcs = []      # 记录每个样本的函数
        self.ncode = []      # 记录每个样本结点的代码
        self.max_code_len = 200
        
        # 加载预处理数据
        if os.path.exists(self.save_dir):
            self._load_processed_data()

        self.etypes = self.data_list[0].etypes
        self.fea_dim = self.data_list[0].ndata["feature"].shape[-1]

    def _load_processed_data(self):
        """加载已处理数据并构建映射"""
        data = torch.load(self.save_dir)
        if isinstance(data, dict):
            self.data_list = data['graphs']
            self.ncode = data['ncode']
        else:
            self.data_list = data

        self.label = self.df['target'].tolist()
        self.languages = self.df['programming_language'].tolist()
        self.cve_ids = self.df['cve_id'].tolist()
        self.funcs = self.df['func'].tolist()
        
        # 构建漏洞-语言映射
        for idx, (cwe, lang) in enumerate(zip(self.cve_ids, self.languages)):
            self.vul_lang_map[cwe][lang].append(idx)

    def process(self):
        """处理原始数据并保存"""
        self.df = pd.read_pickle(self.raw_dataframe_path)
        if os.path.exists(self.save_dir):
            print(f"Load processed data from {self.save_dir}")
            return
        
        # 初始化数据结构
        data_list = []
        self.label = []
        self.languages = []
        self.cve_ids = []
        self.ncode = []
        
        # 并行处理数据
        tqdm.pandas(desc="Processing graphs")
        self.df.progress_apply(lambda row: self._process_row(row, data_list), axis=1)

        # 保存处理结果
        torch.save({
            'graphs': data_list,
            'labels': self.label,
            'languages': self.languages,
            'cve_ids': self.cve_ids,
            'ncode': self.ncode
        }, self.save_dir)
        print(f"Saved processed data to {self.save_dir}")

    def _process_row(self, row, data_list):
        """处理单行数据"""
        # 原有图处理逻辑
        graph = process_to_dgl(data_list, row)  
        
        # 记录元信息
        self.ncode.append(row['torch_geometrics_data'][0]._NCODE)
        self.label.append(row['target'])
        self.languages.append(row['programming_language'])
        self.cve_ids.append(row['cve_id'])

    def __getitem__(self, idx):
        """返回样本及跨语言关联信息"""
        return {
            'idx': idx,
            'graph': self.data_list[idx],
            'label': self.label[idx],
            'language': self.languages[idx],
            'cve_id': self.cve_ids[idx],
            "func": self.funcs[idx],
            "ncode": self.ncode[idx][:self.max_code_len] + [" "] * (self.max_code_len - len(self.ncode[idx]))
        }

    def __len__(self):
        return len(self.data_list)

    @property
    def num_vul_types(self):
        return len(set(self.cve_ids))
    
class vulDGLDataset(DGLDataset):
    def __init__(self, name, raw_dataframe_path=None, url=None, raw_dir=None, save_dir=None, hash_key=..., force_reload=False, verbose=False, transform=None):
        if not os.path.exists(raw_dataframe_path):
            print("dataframe path not exist!")
            return
        
        self.raw_dataframe_path = raw_dataframe_path
        super().__init__(name, url, raw_dir, save_dir, hash_key, force_reload, verbose, transform)
        self.data_list = torch.load(self.save_dir)
        self.etypes = self.data_list[0].etypes
        self.fea_dim = self.data_list[0].ndata["feature"].shape[-1]
        self.category = "node"

    def process(self):

        self.df = pd.read_pickle(self.raw_dataframe_path)
        self.label = self.df["target"].tolist()
        
        if os.path.exists(self.save_dir):
            print(f"load data list from {self.save_dir}")
            return 
        
        data_list = []
        # partial_process_to_dgl = partial(process_to_dgl, data_list)

        # NUM_JOBS = 5
        # splits = np.array_split(self.df, NUM_JOBS)
        # for JOB_ARRAY_NUMBER in range(NUM_JOBS):
        #     processed_list = dfmp(splits[JOB_ARRAY_NUMBER], partial_process_to_dgl, ordr=False, workers=1)
        # processed_list = dfmp(self.df, partial_process_to_dgl, ordr=False, workers=10)
        tqdm.pandas()
        self.df.progress_apply(lambda row: process_to_dgl(data_list, row), axis=1)
        torch.save(data_list, self.save_dir)
        print(f"Saved in { self.save_dir}")

    def __getitem__(self, idx):
        return idx, self.data_list[idx], self.label[idx]
    
    def __len__(self):
        return len(self.data_list)
    
    
if __name__ == "__main__":
    # dataframe_path = "/root/autodl-tmp/vul-detect/utils/data/torch_geometrics_process/cfexplainer/storage/processed/vul_graph_dataset/None_processed/devign_dataframe.pkl"
    dataframe_path = "/root/autodl-tmp/vul-detect/utils/data/torch_geometrics_process/cfexplainer/storage/processed/CVEfixes/None_processed/CVEfixes_dataframe_C#_ncode.pkl"
    save_dir = os.path.join(os.path.dirname(dataframe_path), "dgl_hetgraph_data_c#_ncode.pt")
    dataset = vulDGLDataset_ds(name="CVEfixes", raw_dataframe_path=dataframe_path, save_dir=save_dir)
