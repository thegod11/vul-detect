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
        return self.data_list[idx], self.label[idx]
    
    def __len__(self):
        return len(self.data_list)
    
    
if __name__ == "__main__":
    dataframe_path = "/root/autodl-tmp/vul-detect/utils/data/torch_geometrics_process/cfexplainer/storage/processed/vul_graph_dataset/None_processed/devign_dataframe.pkl"
    save_dir = os.path.join(os.path.dirname(dataframe_path), "data.pt")
    dataset = vulDGLDataset(name="devign", raw_dataframe_path=dataframe_path, save_dir=save_dir)
