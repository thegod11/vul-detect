import torch
import torch.nn as nn
import torch.nn.functional as F

from .contrast import Contrast, GraphLevelContrast
from .transformer_model import TransformerModel
from .gnn_encoder import GNN_encoder
import dgl
from sklearn.metrics import accuracy_score, precision_score, recall_score, f1_score

class LogReg(nn.Module):
    def __init__(self, ft_in, nb_classes):
        super(LogReg, self).__init__()
        self.fc = nn.Linear(ft_in, nb_classes)

        for m in self.modules():
            self.weights_init(m)

    def weights_init(self, m):
        if isinstance(m, nn.Linear):
            torch.nn.init.xavier_uniform_(m.weight.data)
            if m.bias is not None:
                m.bias.data.fill_(0.0)

    def forward(self, seq):
        ret = self.fc(seq)
        return ret

class GTC(nn.Module):

    def __init__(self, hidden_dim, feats_dim_list, feat_drop, P, tau, lam, t_hops, t_n_class, t_input_dim, t_pe_dim,
                 t_n_layers, t_num_heads, t_dropout_rate, t_attention_dropout_rate, rel_names, category=None,
                 gnn_branch_layer_num=2):

        """
        The GTC model class~
        :param hidden_dim: the dimension of the hidden layers
        :param feats_dim_list: the feature list of the input nodes,sometimes for many type nodes
        :param feat_drop: the drop ratio of ,can be 0
        :param attn_drop:
        :param P: meta paths
        :param sample_rate: /
        :param nei_num: /
        :param tau:
        :param lam:
        :param device:
        :param t_hops:
        :param t_n_class:
        :param t_input_dim:
        :param t_pe_dim:
        :param t_n_layers:
        :param t_num_heads:
        :param t_ffn_dim:
        :param t_dropout_rate:
        :param t_attention_dropout_rate:
        :param rel_names:
        :param category:
        """

        super(GTC, self).__init__()
        self.hidden_dim = hidden_dim
        self.category = category
        self.rel_names = rel_names
        self.gnn_branch_layer_num = gnn_branch_layer_num
        self.t_hops = t_hops
        self.feats_dim_list = feats_dim_list
        self.t_n_class = t_n_class
        self.t_input_dim = t_input_dim
        self.t_pe_dim = t_pe_dim
        self.t_n_layers = t_n_layers
        self.t_num_heads = t_num_heads
        self.t_dropout_rate = t_dropout_rate
        self.t_attention_dropout_rate = t_attention_dropout_rate
        self.feat_drop_rate = feat_drop
        self.tau = tau
        self.lam = lam

        self.category = category
        self.rel_names = rel_names
        self.fc_list = nn.ModuleList([nn.Linear(feats_dim, self.hidden_dim, bias=True)
                                      for feats_dim in feats_dim_list])
        for fc in self.fc_list:
            nn.init.xavier_normal_(fc.weight, gain=1.414)
        if feat_drop > 0:
            self.feat_drop = nn.Dropout(feat_drop)
        else:
            self.feat_drop = lambda x: x
        # hops view encoder
        # transformer blocks for different metapaths
        self.transformer_list = nn.ModuleList([TransformerModel(hops=t_hops,
                                                                n_class=t_n_class,
                                                                input_dim=t_input_dim,
                                                                pe_dim=t_pe_dim,
                                                                n_layers=t_num_heads,
                                                                num_heads=t_n_layers,
                                                                hidden_dim=self.hidden_dim,
                                                                ffn_dim=self.hidden_dim,
                                                                dropout_rate=t_dropout_rate,
                                                                attention_dropout_rate=t_attention_dropout_rate)
                                               for i in range(P)])

        self.att_embeddings_proj = nn.Linear(int(self.hidden_dim / 2), self.hidden_dim)
        nn.init.xavier_normal_(self.att_embeddings_proj.weight, gain=1.414)

        self.sematic_attention = Attention(hidden_dim=self.hidden_dim, attn_drop=t_attention_dropout_rate)
        # graph schema view encoder
        self.gnn_branch = GNN_encoder(in_feats=self.hidden_dim, hid_feats=self.hidden_dim * 2,
                                      out_feats=self.hidden_dim,
                                      rel_names=self.rel_names, layer_nums=gnn_branch_layer_num,
                                      category=self.category)
        
        self.logreg_model = LogReg(self.hidden_dim*2 , 2)
        # contrast task
        self.contrast = Contrast(self.hidden_dim, tau, lam)
        self.graph_contrast = GraphLevelContrast(hidden_dim=self.hidden_dim*2, tau=tau, alpha=0.7)

    def forward(self, g, feats, multi_hop_features, pos, cve_ids=None, languages=None, mini_batch_flag=False, mode="train"):
        h_all = {node_key: F.elu(self.feat_drop(self.fc_list[i](feats[node_key])))
                 for i, node_key in enumerate(feats.keys())}

        # 处理 Transformer 分支
        z_mp_list = [self.att_embeddings_proj(self.transformer_list[i](multi_hop_features[i]))
                     for i in range(len(multi_hop_features))]
        z_transformer = self.sematic_attention(z_mp_list)

        # 处理 GNN 分支
        z_gnn = self.gnn_branch(g=g, feat=h_all, mini_batch_flag=mini_batch_flag)
        loss = self.contrast(z_transformer, z_gnn, pos)

        # 计算最终的节点表示
        fused_features = torch.cat([z_transformer, z_gnn], dim=-1)
        g.ndata["fused_features"] = fused_features
        graph_embed = dgl.mean_nodes(g, "fused_features")
        logits = self.logreg_model(graph_embed)

        if mode == "train":
            if languages is not None and cve_ids is not None:
                # 计算对比损失
                contrast_loss = self.graph_contrast(graph_embed, cve_ids, languages)
                loss += contrast_loss

            return loss

        elif mode == "pred":
            return graph_embed


    def get_gnn_embeds(self, g, feat, mini_batch_flag):
        h_all = {}
        for i, node_key in enumerate(feat.keys()):
            h_all[node_key] = F.elu(self.feat_drop(self.fc_list[i](feat[node_key])))
        z_gnn = self.gnn_branch(g=g, feat=h_all, mini_batch_flag=mini_batch_flag)
        return z_gnn

    def get_embeds(self, multi_hop_features):   
        z_mp_list = []
        for i in range(len(multi_hop_features)):
            z_mp_list.append(self.att_embeddings_proj(self.transformer_list[i](multi_hop_features[i])))
        z_mp = self.sematic_attention(z_mp_list)
        return z_mp.detach()


class Attention(nn.Module):
    def __init__(self, hidden_dim, attn_drop):
        super(Attention, self).__init__()
        self.fc = nn.Linear(hidden_dim, hidden_dim, bias=True)
        nn.init.xavier_normal_(self.fc.weight, gain=1.414)

        self.tanh = nn.Tanh()
        self.att = nn.Parameter(torch.empty(size=(1, hidden_dim)), requires_grad=True)
        nn.init.xavier_normal_(self.att.data, gain=1.414)

        self.softmax = nn.Softmax()
        if attn_drop:
            self.attn_drop = nn.Dropout(attn_drop)
        else:
            self.attn_drop = lambda x: x

    def forward(self, embeds):
        beta = []
        attn_curr = self.attn_drop(self.att)
        for embed in embeds:
            sp = self.tanh(self.fc(embed)).mean(dim=0)
            beta.append(attn_curr.matmul(sp.t()))
        beta = torch.cat(beta, dim=-1).view(-1)
        beta = self.softmax(beta)
        # print("mp ", beta.data.cpu().numpy())  # semantic attention
        z_mp = 0
        for i in range(len(embeds)):
            z_mp += embeds[i] * beta[i]
        return z_mp
