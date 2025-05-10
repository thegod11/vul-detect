#!/usr/bin/env python 
# -*- coding: utf-8 -*- 
# @Time : 2023/3/21 15:20
# @Site :
# @File : main.py
# @Software: PyCharm
import os
import sys
# BASE_DIR = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
# # BASE_DIR = os.path.dirname(os.path.abspath(__file__))
# print(BASE_DIR)
# sys.path.append(BASE_DIR)
import sys
import os
curPath = os.path.abspath(os.path.dirname(__file__))
rootPath = os.path.split(curPath)[0]
sys.path.append(rootPath)
sys.path.append("/root/autodl-tmp/vul-detect")
from my_email import EmailSender
import numpy
import numpy as np
import torch
from dgl.dataloading import DataLoader
from dgl.dataloading import NeighborSampler
from module import GTC
import datetime
import random
from self_tools.data_tools import load_data, get_batch_pos
from self_tools.evaluate import evaluate_for_test, evaluate_for_train
from sklearn.metrics import f1_score, accuracy_score, recall_score, precision_score, roc_auc_score
from self_tools.params import set_params
from dgl.dataloading import GraphDataLoader
from torch.utils.data.sampler import SubsetRandomSampler
from dgl.data import DGLDataset
from vulDGLDataset import vulDGLDataset, vulDGLDataset_ds, BalancedBatchSampler
from sklearn.metrics import confusion_matrix
from classifier import ClassifierTrainer, extract_embeddings
import pandas as pd
import pickle

args = set_params()
if torch.cuda.is_available() and args.device > -1:
    device = torch.device("cuda:0")
    torch.cuda.set_device(args.device)
else:
    device = torch.device("cpu")

## name of intermediate document ##

own_str = args.dataset
exp_num = 10


def make(config, dgl_graph, feats_dim_list, P, h_dict, category, all_node_idx,
         num_classes, mini_batch_flag=True):
    """
    the fuction of building the model, train_loader and optimizer
    :param config:
    :param dgl_graph:
    :param feats_dim_list:
    :param P:
    :param meta_path_adj:
    :param h_dict:
    :param category:
    :param all_node_idx:
    :param num_classes:
    :param mini_batch_flag:
    :return: model, train_loader,optimizer
    """
    print("seed ", config.seed)
    print("Dataset: ", config.dataset)
    print("The number of gnn_branch_num: ", config.gnn_branch_layer_num)
    # build the GTC model
    model = GTC(config.hidden_dim, feats_dim_list, config.feat_drop, P, config.tau, config.lam,
                t_hops=config.t_hops, t_n_class=num_classes, t_input_dim=h_dict[category].shape[1],
                t_pe_dim=config.t_pe_dim, t_n_layers=config.t_n_layers, t_num_heads=config.t_n_heads,
                t_dropout_rate=config.t_dropout,
                t_attention_dropout_rate=config.t_attention_dropout, rel_names=dgl_graph.etypes, category=category,
                gnn_branch_layer_num=config.gnn_branch_layer_num)
    # build the optimizer for GTC
    optimizer = torch.optim.Adam(model.parameters(), lr=config.lr, weight_decay=config.l2_coef)

    # NeighborSampler and corresponding graph DataLoader for mini_batch training~
    # for more details for NeighborSampler and DataLoader, please see https://docs.dgl.ai/guide/minibatch.html#guide-minibatch
    fanouts = [20]  # first hop sample 20 neighbors for every node
    for i in range(1, config.gnn_branch_layer_num):
        fanouts.append(10)  # 2-gnn_branch_layer_num hop sample 10 neighbors for every node
    sampler = NeighborSampler(fanouts=fanouts)
    all_idx_dict = {category: all_node_idx}

    train_dataloader_4GTC = DataLoader(graph=dgl_graph, indices=all_idx_dict, graph_sampler=sampler,
                                       batch_size=config.batch_size,
                                       shuffle=True)

    return model, train_dataloader_4GTC, optimizer

def make4GraphClassification(config, dataset, mode="train"):
    """
    the fuction of building the model, train_loader and optimizer
    :param config:
    :param dgl_graph:
    :param feats_dim_list:
    :param P:
    :param meta_path_adj:
    :param h_dict:
    :param category:
    :param all_node_idx:
    :param mini_batch_flag:
    :return: model, train_loader, optimizer
    """
    print("seed ", config.seed)
    print("Dataset: ", config.dataset)
    print("The number of gnn_branch_num: ", config.gnn_branch_layer_num)
    # build the GTC model
    P = len(dataset.etypes)
    feats_dim_list = [dataset.fea_dim]
    
    model = GTC(config.hidden_dim, feats_dim_list, config.feat_drop, P, config.tau, config.lam,
                t_hops=config.t_hops, t_n_class=None, t_input_dim=dataset.fea_dim,
                t_pe_dim=config.t_pe_dim, t_n_layers=config.t_n_layers, t_num_heads=config.t_n_heads,
                t_dropout_rate=config.t_dropout,
                t_attention_dropout_rate=config.t_attention_dropout, rel_names=dataset.etypes, category=dataset.category,
                gnn_branch_layer_num=config.gnn_branch_layer_num)

    if False and os.path.exists('../data/checkpoint/GTC_' + config.dataset + '.pkl'):
        model.load_state_dict(torch.load('../data/checkpoint/GTC_' + config.dataset + '.pkl'))
        print(f'load pre-trained model from ../data/checkpoint/GTC_{config.dataset}.pkl !')
    # build the optimizer for GTC
    optimizer = torch.optim.Adam(model.parameters(), lr=config.lr, weight_decay=config.l2_coef)

    num_examples = len(dataset)
    num_train = int(num_examples * 0.8)

    if mode == "train":
        train_sampler = BalancedBatchSampler(dataset, batch_size=config.batch_size)
        dataloader = GraphDataLoader(dataset, batch_sampler=train_sampler, drop_last=False, num_workers=4)
    elif mode == "test":
        dataloader = GraphDataLoader(dataset, batch_size=128, shuffle=False, drop_last=False, num_workers=4)

    return model, dataloader, optimizer

def train_flow(model, train_loader, optimizer, config, category, pos, own_str, exp=0):
    cnt_wait = 0
    best = 1e9
    best_t = 0
    print('-' * 60)
    print('train_flow for exp-{}'.format(exp))
    starttime = datetime.datetime.now()
    for epoch in range(config.nb_epochs):
        model.train()
        loss_epoch = 0
        for batch_id, (input_nodes, output_nodes, blocks) in enumerate(train_loader):
            blocks = [block.to(config.device) for block in blocks]
            # for GNN_branch batch data
            if 'h' in blocks[0].srcdata:
                input_fea4GNN = blocks[0].srcdata['h']
            elif 'feature' in blocks[0].srcdata:
                input_fea4GNN = blocks[0].srcdata['feature']
            else:
                print('please specify the feature key!')
                return
            if not isinstance(input_fea4GNN, dict):
                input_fea4GNN = {category: input_fea4GNN}
            # deal with pos for mini-batch
            pos_batch = get_batch_pos(pos=pos, batch_node_id_x=output_nodes[category].numpy()).to(config.device)
            # [num_meta-paths,num_nodes,num_hops,feature_dim}
            multi_hop_features = blocks[-1].dstnodes[category].data['multi_hop_feature'].permute(1, 0, 2, 3)

            # blocks : <class 'dgl.heterograph.DGLBlock'>
            # input_fea4GNN  torch.Size([4057, 334]) 
            # multi_hop_features torch.Size([3, 512, 2, 334])
            # pos_batch = torch.Size([512, 512])
            # batch_size 512
            # dgl_graph.num_nodes() 4057
            loss = model(g=blocks, feats=input_fea4GNN, multi_hop_features=multi_hop_features, pos=pos_batch, mini_batch_flag=True)
            loss_epoch = loss_epoch + loss
            optimizer.zero_grad()
            loss.backward()
            optimizer.step()
            print("exp={}; epoch: {}; batch-{}; loss {}".format(exp, epoch, batch_id, loss.data.cpu()))

        print(" epoch: {}; epoch_loss {}".format(epoch, loss_epoch.data.cpu()))
        if loss_epoch < best:
            print('best loss: {}->{}'.format(best, loss_epoch))
            best = loss_epoch
            best_t = epoch
            cnt_wait = 0
            # save better checkpoint~
            torch.save(model.state_dict(), '../data/GTC_' + own_str + '.pkl')
        else:
            cnt_wait += 1
            print('lost not improved~ {}'.format(cnt_wait))
        if cnt_wait >= config.patience:
            print('Early stopping at {} epoch!'.format(epoch))
            break
    print('best epoch is {} !'.format(best_t))
    endtime = datetime.datetime.now()
    time = (endtime - starttime).seconds
    print('Total train time {} s'.format(time))
    print('-' * 40)
    return best_t

def test_flow_cvefixes(model, test_loader, config, category, exp=0):
    print('-' * 60)
    print('Testing for exp-{}'.format(exp))
    model.eval()
    
    # 初始化存储容器
    all_preds = []
    all_idxs = []
    all_probs = []
    all_labels = []
    
    with torch.no_grad():
        for batch_id, (idxs, graphs, labels, languages) in enumerate(test_loader):
            graphs = graphs.to(config.device)
            labels = labels.to(config.device)

            # 特征提取
            if 'h' in graphs.ndata:
                input_fea4GNN = graphs.ndata['h']
            elif 'feature' in graphs.ndata:
                input_fea4GNN = graphs.ndata['feature']
            else:
                raise ValueError("Feature key not found in graph data")
                
            if not isinstance(input_fea4GNN, dict):
                input_fea4GNN = {category: input_fea4GNN}
                
            multi_hop_features = graphs.ndata['multi_hop_feature'].permute(1, 0, 2, 3)

            # 模型预测
            preds, probs = model(
                g=graphs, 
                feats=input_fea4GNN, 
                multi_hop_features=multi_hop_features, 
                mini_batch_flag=False,
                mode="pred"
            )
            
            # 收集结果
            all_idxs.append(idxs)
            all_probs.append(probs[:, 1])
            all_preds.append(preds)
            all_labels.append(labels.cpu())

    # 合并所有结果
    all_idxs = np.concatenate(all_idxs)
    all_probs = np.concatenate(all_probs)
    all_preds = np.concatenate(all_preds)
    all_labels = np.concatenate(all_labels)

    # 计算混淆矩阵
    tn, fp, fn, tp = confusion_matrix(all_labels, all_preds).ravel()

    # 计算 FPR 和 FNR
    fpr = fp / (fp + tn) if (fp + tn) > 0 else 0
    fnr = fn / (fn + tp) if (fn + tp) > 0 else 0

    # 计算指标
    metrics = {
        "accuracy": accuracy_score(all_labels, all_preds),
        "f1": f1_score(all_labels, all_preds),
        "recall": recall_score(all_labels, all_preds),
        "precision": precision_score(all_labels, all_preds),
        "auc": roc_auc_score(all_labels, all_probs),
        "fpr": fpr,  # 假阳率
        "fnr": fnr   # 假阴率
    }

    # 打印结果
    print(f"\nTest Results (exp-{exp})")
    print(f"Accuracy: {metrics['accuracy']:.4f}")
    print(f"F1 Score: {metrics['f1']:.4f}")
    print(f"Recall:   {metrics['recall']:.4f}")
    print(f"Precision:{metrics['precision']:.4f}")
    print(f"AUC:      {metrics['auc']:.4f}")
    print(f"FPR:      {metrics['fpr']:.4f}")  # 假阳率
    print(f"FNR:      {metrics['fnr']:.4f}")  # 假阴率
    
    return metrics


def train_flow_cvefixes(model, train_loader, optimizer, config, category, own_str, exp=0):
    # 新增混合精度训练
    scaler = torch.cuda.amp.GradScaler(enabled=config.use_amp)  # 在config中添加use_amp参数
    
    # 新增内存监控函数
    def print_mem(msg):
        if config.debug_mem:  # 调试时开启
            print(f"[MEM]{msg}: alloc {torch.cuda.memory_allocated()/1e9:.2f}GB, "
                  f"reserved {torch.cuda.memory_reserved()/1e9:.2f}GB")
            
    if os.path.exists('../data/checkpoint/GTC_' + own_str + '.pkl'):
        model.load_state_dict(torch.load('../data/checkpoint/GTC_' + own_str + '.pkl'))
        print("load pretrained model from ../data/checkpoint/GTC_" + own_str + '.pkl !!!')

    cnt_wait = 0
    best = 1e9
    best_t = 0
    print('-' * 60)
    print('train_flow for exp-{}'.format(exp))
    starttime = datetime.datetime.now()
    for epoch in range(config.nb_epochs):
        model.train()
        loss_epoch = 0
        for batch_id, items in enumerate(train_loader):
            # 数据加载后立即释放CPU内存
            with torch.no_grad():
                idx = items['idx'].clone()
                graphs = items['graph'].to(config.device)
                labels = items['label'].to(config.device)
                cve_ids = items['cve_id']
                del items  # 主动释放原始数据
                torch.cuda.empty_cache()
            
            print_mem("After data loading")
            
            # 使用上下文管理器管理计算图
            with torch.cuda.amp.autocast(enabled=config.use_amp):

                pdg = graphs.adj(etype="pdg").to_dense()
                ast = graphs.adj(etype="ast").to_dense()
                cfg = graphs.adj(etype="cfgcdg").to_dense()
                pos_batch = ((pdg + ast + cfg) >= 3).float().fill_diagonal_(1).to_sparse().cuda()
                
                del pdg, cfg, ast
                print_mem("After pos_batch")
                
                # 优化特征处理
                if 'h' in graphs.ndata:
                    input_fea4GNN = graphs.ndata['h']
                elif 'feature' in graphs.ndata:
                    input_fea4GNN = graphs.ndata['feature']
                else:
                    raise KeyError("Feature key not found in graph data")
                if not isinstance(input_fea4GNN, dict):
                    input_fea4GNN = {category: input_fea4GNN}
                # 使用原地操作处理多跳特征
                multi_hop_features = graphs.ndata['multi_hop_feature']
                multi_hop_features = multi_hop_features.permute(1, 0, 2, 3).contiguous()
                
                print_mem("Before model forward")
                
                # 前向传播
                loss = model(
                    g=graphs,
                    feats=input_fea4GNN,
                    multi_hop_features=multi_hop_features,
                    pos=pos_batch,
                    languages=None,
                    cve_ids=cve_ids,
                    mini_batch_flag=False
                )
            
            # 梯度累积（新增）
            if config.grad_accum_steps > 1:
                loss = loss / config.grad_accum_steps
            
            # 反向传播优化
            scaler.scale(loss).backward()
            
            # 梯度累积策略
            if (batch_id + 1) % config.grad_accum_steps == 0:
                scaler.step(optimizer)
                scaler.update()
                optimizer.zero_grad()
                print_mem("After backward")
            
            # 及时释放中间变量
            del graphs, pos_batch, multi_hop_features
            torch.cuda.empty_cache()
            
            loss_epoch += loss.detach()
            
            print("exp={}; epoch: {};batch-{}; loss {}; ".format(exp, epoch, batch_id, loss.data.cpu()))

        print(" epoch: {}; epoch_loss {}".format(epoch, loss_epoch.data.cpu()))
        if loss_epoch < best :
            print('best loss: {}->{}'.format(best, loss_epoch))
            best = loss_epoch
            best_t = epoch
            cnt_wait = 0
            # save better checkpoint~
            os.makedirs('../data/checkpoint', exist_ok=True)
            torch.save(model.state_dict(), '../data/checkpoint/GTC_' + own_str + f'.pkl')
            print("save model in ../data/checkpoint/GTC_" + own_str + f'.pkl !!!')
        elif epoch % 100 == 0:
            os.makedirs('../data/other-checkpoints', exist_ok=True)
            torch.save(model.state_dict(), '../data/other-checkpoints/GTC_' + own_str + f'_epoch_{epoch}.pkl')
            print("save model in ../data/other-checkpoints/GTC_" + own_str + f'_epoch_{epoch}.pkl !!!')
        else:
            cnt_wait += 1
            print('lost not improved~ {}'.format(cnt_wait))
        if cnt_wait >= config.patience:
            print('Early stopping at {} epoch!'.format(epoch))
            break
    print('best epoch is {} !'.format(best_t))
    endtime = datetime.datetime.now()
    time = (endtime - starttime).seconds
    print('Total train time {} s'.format(time))
    print('-' * 40)
    return best_t


def train_flow_devign(model, train_loader, optimizer, config, category, own_str, exp=0):
    if os.path.exists('../data/checkpoint/GTC_' + own_str + '.pkl'):
        model.load_state_dict(torch.load('../data/checkpoint/GTC_' + own_str + '.pkl'))
        print("load pretrained model from ../data/checkpoint/GTC_" + own_str + '.pkl !!!')

    cnt_wait = 0
    best = 1e9
    best_t = 0
    print('-' * 60)
    print('train_flow for exp-{}'.format(exp))
    starttime = datetime.datetime.now()
    for epoch in range(config.nb_epochs):
        model.train()
        loss_epoch = 0
        for batch_id, items in enumerate(train_loader):
            idx, graphs, labels, languages, cve_ids = items['idx'], items['graph'], items['label'], items['language'], items['cve_id']
            graphs = graphs.to(config.device)
            labels = labels.to(config.device)
            # blocks = [block.to(config.device) for block in blocks]
            # for GNN_branch batch data
            if 'h' in graphs.ndata:
                input_fea4GNN = graphs.ndata['h']
            elif 'feature' in graphs.ndata:
                input_fea4GNN = graphs.ndata['feature']
            else:
                print('please specify the feature key!')
                return
            if not isinstance(input_fea4GNN, dict):
                input_fea4GNN = {category: input_fea4GNN}
            # deal with pos for mini-batch
            pdg = graphs.adj(etype="pdg").to_dense()
            ast = graphs.adj(etype="ast").to_dense()
            cfg = graphs.adj(etype="cfgcdg").to_dense()
            pos_batch = ((pdg + ast + cfg) >= 3).float().fill_diagonal_(1).to_sparse().cuda()
            del pdg, ast, cfg
            # pos_batch = get_batch_pos(pos=pos, batch_node_id_x=output_nodes[category].numpy()).to(config.device)
            # [num_meta-paths,num_nodes,num_hops,feature_dim}
            multi_hop_features = graphs.ndata['multi_hop_feature'].permute(1, 0, 2, 3)

            loss = model(g=graphs, feats=input_fea4GNN, multi_hop_features=multi_hop_features, pos=pos_batch,languages=languages, cve_ids=cve_ids, mini_batch_flag=False)
            loss_epoch = loss_epoch + loss
            optimizer.zero_grad()
            loss.backward()
            optimizer.step()
            
            print("exp={}; epoch: {};batch-{}; loss {}; ".format(exp, epoch, batch_id, loss.data.cpu()))

        print(" epoch: {}; epoch_loss {}".format(epoch, loss_epoch.data.cpu()))
        if loss_epoch < best :
            print('best loss: {}->{}'.format(best, loss_epoch))
            best = loss_epoch
            best_t = epoch
            cnt_wait = 0
            # save better checkpoint~
            os.makedirs('../data/checkpoint', exist_ok=True)
            torch.save(model.state_dict(), '../data/checkpoint/GTC_' + own_str + f'.pkl')
            print("save model in ../data/checkpoint/GTC_" + own_str + f'.pkl !!!')
        elif epoch % 100 == 0:
            os.makedirs('../data/other-checkpoints', exist_ok=True)
            torch.save(model.state_dict(), '../data/other-checkpoints/GTC_' + own_str + f'_epoch_{epoch}.pkl')
            print("save model in ../data/other-checkpoints/GTC_" + own_str + f'_epoch_{epoch}.pkl !!!')
        else:
            cnt_wait += 1
            print('lost not improved~ {}'.format(cnt_wait))
        if cnt_wait >= config.patience:
            print('Early stopping at {} epoch!'.format(epoch))
            break
    print('best epoch is {} !'.format(best_t))
    endtime = datetime.datetime.now()
    time = (endtime - starttime).seconds
    print('Total train time {} s'.format(time))
    print('-' * 40)
    return best_t

def test(model, config, train_idx_list, val_idx_list, test_idx_list, labels, num_classes, fea_evalue, ma_dic_list,
         mi_dic_list, auc_dic_list):
    starttime = datetime.datetime.now()
    model.eval()
    emb = model.get_embeds(multi_hop_features=fea_evalue.permute(1, 0, 2, 3))
    for i in range(len(train_idx_list)):  # for different data splits for testing~
        ma, mi, auc = evaluate_for_train(config.hidden_dim, train_idx_list[i], val_idx_list[i], test_idx_list[i],
                                         labels, num_classes, config.device, config.dataset, config.eva_lr,
                                         config.eva_wd, batch_size=500, patience=config.patience, emb=emb)
        # record the result of this exp
        ma_dic_list['ma_{}'.format(config.ratio[i])].append(ma)
        mi_dic_list['mi_{}'.format(config.ratio[i])].append(mi)
        auc_dic_list['auc_{}'.format(config.ratio[i])].append(auc)
    endtime = datetime.datetime.now()
    time = (endtime - starttime).seconds
    print("Total evaluate time: ", time, "s")
    print('-' * 40)


def model_train(args):
    # record the result of each exp
    ma_dic_list = dict.fromkeys(['ma_20', 'ma_40', 'ma_60'])
    for key in ma_dic_list.keys():
        ma_dic_list[key] = []
    mi_dic_list = dict.fromkeys(['mi_20', 'mi_40', 'mi_60'])
    for key in mi_dic_list.keys():
        mi_dic_list[key] = []
    auc_dic_list = dict.fromkeys(['auc_20', 'auc_40', 'auc_60'])
    for key in auc_dic_list.keys():
        auc_dic_list[key] = []
    for exp in range(exp_num):  # every exp
        print('-' * 60)
        print('exp:{}'.format(exp))
        print('-' * 60)
        starttime = datetime.datetime.now()
        if torch.cuda.is_available() and args.device > -1:
            device = torch.device("cuda:0")
            torch.cuda.set_device(args.device)
        else:
            device = torch.device("cpu")

        # name of intermediate document
        own_str = args.dataset + '_' + str(exp)

        # random seed
        seed = args.seed
        numpy.random.seed(seed)
        random.seed(seed)
        torch.manual_seed(seed)
        torch.cuda.manual_seed(seed)

        # load data~
        dgl_graph, category, all_node_idx, train_idx_list, val_idx_list, test_idx_list, \
        h_dict, labels, P, num_classes, pos = load_data(
            data_name=args.dataset, data_dir='../data/', t_hops=args.t_hops,
            cache_sub_dir='cache-opensource')

        feats_dim_list = [h_dict[key].shape[-1] for key in h_dict.keys()]

        # build the model, train_loader and optimizer
        model, train_loader, optimizer = make(args, dgl_graph, feats_dim_list, P, h_dict,
                                              category, all_node_idx, dgl_graph.etypes, num_classes)
        print(model)

        if torch.cuda.is_available() and args.device > -1:
            print('Using CUDA~')
            model.to(device)
            labels = labels.cuda()
            for index in range(len(train_idx_list)):
                train_idx_list[index] = train_idx_list[index].long().cuda()
                val_idx_list[index] = val_idx_list[index].long().cuda()
                test_idx_list[index] = test_idx_list[index].long().cuda()

        # train the model~
        best_t = train_flow(model, train_loader, optimizer, args, category, pos, own_str, exp=exp)
        # test the model~
        print('-' * 40)
        print('test paradigm~')
        print('Loading {}th epoch'.format(best_t))
        # load checkpoint
        model.load_state_dict(torch.load('../data/GTC_' + own_str + '.pkl'))
        fea_evalue = dgl_graph.nodes[category].data['multi_hop_feature'].to(device)
        # test flow
        test(model, args, train_idx_list, val_idx_list, test_idx_list, labels, num_classes, fea_evalue, ma_dic_list,
             mi_dic_list, auc_dic_list)

        endtime = datetime.datetime.now()
        time = (endtime - starttime).seconds
        print("Total time: ", time, "s")

    # print the result
    for key in ma_dic_list.keys():
        lst = ma_dic_list[key]
        print('{}_mean:{},{}_var:{}'.format(key, np.mean(lst), key, np.std(lst)))
        # print('{}:{}'.format(key, lst))

    for key in mi_dic_list.keys():
        lst = mi_dic_list[key]
        print('{}_mean:{},{}_var:{}'.format(key, np.mean(lst), key, np.std(lst)))
        # print('{}:{}'.format(key, lst))

    for key in auc_dic_list.keys():
        lst = auc_dic_list[key]
        print('{}_mean:{},{}_var:{}'.format(key, np.mean(lst), key, np.std(lst)))
        # print('{}:{}'.format(key, lst))

def model_train_CVEfixes(args):
    def watch_log():
        file_path = '/root/autodl-tmp/output-gtc-train.log'
        with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
            lines = f.readlines()
        tail_lines = lines[-5:]
        return '\n'.join(tail_lines)

    emailsender = EmailSender(heartbeat_callback=watch_log, heartbeat_interval=1800)
    for exp in range(exp_num):  # every exp
        print('-' * 60)
        print('exp:{}'.format(exp))
        print('-' * 60)
        starttime = datetime.datetime.now()
        if torch.cuda.is_available() and args.device > -1:
            device = torch.device("cuda:0")
            torch.cuda.set_device(args.device)
        else:
            device = torch.device("cpu")

        # name of intermediate document
        own_str = args.dataset + f'_{args.train_mode}_exp_' + str(exp)

        # random seed
        seed = args.seed
        numpy.random.seed(seed)
        random.seed(seed)
        torch.manual_seed(seed)
        torch.cuda.manual_seed(seed)

        dataset = vulDGLDataset_ds("CVEfixes", raw_dataframe_path=args.dataframe_path, save_dir=args.save_dir)
        # build the model, train_loader and optimizer
        model, train_loader, optimizer = make4GraphClassification(args, dataset)
        # print(model)

        if torch.cuda.is_available() and args.device > -1:
            print('Using CUDA~')
            model.to(device)

        # train the model~
        best_t = train_flow_devign(model, train_loader, optimizer, args, dataset.category, own_str, exp=exp)

        endtime = datetime.datetime.now()
        time = (endtime - starttime).seconds
        print("Total time: ", time, "s")

def model_test_CVEfixes(args):
    if torch.cuda.is_available() and args.device > -1:
        device = torch.device("cuda:0")
        torch.cuda.set_device(args.device)
    else:
        device = torch.device("cpu")

    # name of intermediate document
    own_str = args.dataset + '_test'

    # random seed
    seed = args.seed
    numpy.random.seed(seed)
    random.seed(seed)
    torch.manual_seed(seed)
    torch.cuda.manual_seed(seed)

    dataset = vulDGLDataset("CVEfixes", raw_dataframe_path=args.test_dataframe_path, save_dir=args.test_data_save_dir)
    # build the model, test_loader and optimizer
    model, test_loader, optimizer = make4GraphClassification(args, dataset)
    print(model)

    if torch.cuda.is_available() and args.device > -1:
        print('Using CUDA~')
        model.to(device)

    # test the model~
    metircs = test_flow_cvefixes(model, test_loader, args, dataset.category)

def test_pre_trained_model(args):
    model = torch.load('../data/{}_model.pkl'.format(args.dataset))
    ## random seed ##
    seed = model.seed
    numpy.random.seed(seed)
    random.seed(seed)
    torch.manual_seed(seed)
    torch.cuda.manual_seed(seed)
    # load data
    dgl_graph, category, all_node_idx, train_idx_list, val_idx_list, test_idx_list, \
    h_dict, labels, P, num_classes, pos = load_data(
        data_name=args.dataset, data_dir='../data/', t_hops=model.t_hops,
        cache_sub_dir='cache-opensource')
    if torch.cuda.is_available() and args.device > -1:
        print('Using CUDA')
        model.to(device)
        labels = labels.cuda()
        for index in range(len(train_idx_list)):
            train_idx_list[index] = train_idx_list[index].long().cuda()
            val_idx_list[index] = val_idx_list[index].long().cuda()
            test_idx_list[index] = test_idx_list[index].long().cuda()

    starttime = datetime.datetime.now()
    model.eval()
    fea_evalue = dgl_graph.nodes[category].data['multi_hop_feature'].to(device)
    for i in range(len(train_idx_list)):
        evaluate_for_test(model.hidden_dim, train_idx_list[i], val_idx_list[i], test_idx_list[i],
                          labels,
                          num_classes, device,
                          args.dataset,
                          args.eva_lr, args.eva_wd, model=model, fea_evalue=fea_evalue,
                          patience=args.patience, batch_size=500)
    endtime = datetime.datetime.now()
    time = (endtime - starttime).seconds
    print("Total time: ", time, "s")

def main_train_classifier(args):
    # 固定随机种子保证可重复性
    seed = args.seed
    numpy.random.seed(seed)
    random.seed(seed)
    torch.manual_seed(seed)
    torch.cuda.manual_seed(seed)

    # 设备设置
    device = torch.device(f"cuda:{args.device}" if torch.cuda.is_available() and args.device > -1 else "cpu")
    print(f"Using device: {device}")

    # 加载数据集（需与预训练时相同）
    dataset = vulDGLDataset_ds("CVEfixes", 
                          raw_dataframe_path=args.dataframe_path,
                          save_dir=args.save_dir)
    test_dataset = vulDGLDataset_ds("CVEfixes", 
                          raw_dataframe_path=args.test_dataframe_path, 
                          save_dir=args.test_data_save_dir)
    
    # 构建模型结构（需与预训练模型完全一致）
    model, train_loader, _ = make4GraphClassification(args, dataset, mode="test")
    _, test_loader, _ = make4GraphClassification(args, test_dataset, mode="test")
    model = model.to(device)

    # 加载预训练权重
    pretrained_path = args.pretrained_path
    model.load_state_dict(torch.load(pretrained_path, map_location=device))
    print(f"Loaded pretrained weights from {pretrained_path}")

    # 冻结模型参数
    for param in model.parameters():
        param.requires_grad = False
    model.eval()

    # 提取图嵌入特征
    print("Extracting graph embeddings...")
    train_data = extract_embeddings(model, train_loader, args, dataset.category)
    print("train_data shape: ", train_data["embeddings"].shape)
    print("Extracting test dataset 's graph embeddings...")
    test_data = extract_embeddings(model, test_loader, args, dataset.category)

    with open(f"{args.result_dir}/test_{args.train_mode}_betas.pkl", "wb") as f:
        pickle.dump(test_data, f)

    # 训练分类器
    X_train, y_train, X_test, y_test, idxes, test_funcs = train_data["embeddings"], train_data["labels"], test_data["embeddings"], test_data["labels"], test_data["indexes"], test_data["funcs"]
    print(f"Training classifier on {X_train.shape[0]} samples...")
    trainer = ClassifierTrainer(device=device)
    trainer._plot_distribution(args, train_data, test_data)
    print("！！！！！！！！！！！！所有图像绘制完毕 ！！！！！！！！！！！")
    
    # 训练所有分类器
    print("\nTraining MLP:")
    mlp_model = trainer.train_mlp(X_train, y_train, X_train.shape[1], args)
    
    print("\nTesting MLP:")
    mlp_metrics = trainer.test(args, mlp_model, test_data, 'mlp', idxes)
    print(f"MLP测试结果: {mlp_metrics}")

    print("\nTraining SVM:")
    svm_model = trainer.train_sklearn_model(X_train, y_train, 'svm')
    print("\nTesting SVM:")
    svm_metrics = trainer.test(args, svm_model, test_data, 'svm', idxes)
    print(f"SVM测试结果: {svm_metrics}")

    print("\nTraining Random Forest:")
    rf_model = trainer.train_sklearn_model(X_train, y_train, 'rf')
    print("\nTesting Random Forest:")
    rf_metrics = trainer.test(args, rf_model, test_data, 'rf', idxes)
    print(f"RF测试结果: {rf_metrics}")

    print("\nTraining XGBoost:")
    xgb_model = trainer.train_sklearn_model(X_train, y_train, 'xgb')
    print("\nTesting XGBoost:")
    xgb_metrics = trainer.test(args, xgb_model, test_data, 'xgb', idxes)
    print(f"XGB测试结果: {xgb_metrics}")

    return {
        'mlp': (mlp_model, mlp_metrics),
        'svm': (svm_model, svm_metrics),
        'rf': (rf_model, rf_metrics),
        'xgb': (xgb_model, xgb_metrics)
    }

if __name__ == "__main__":
    if args.task == "test":  # test the pretrained model
        model_test_CVEfixes(args)
    elif args.task == "train":  # train new model
        model_train_CVEfixes(args)
    elif args.task == "train_classifer":  # train classifier
        main_train_classifier(args)