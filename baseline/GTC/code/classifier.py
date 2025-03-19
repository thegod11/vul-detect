from sklearn.ensemble import RandomForestClassifier
from sklearn.linear_model import LogisticRegression
from sklearn.svm import SVC
import torch
import torch.nn as nn
from sklearn.model_selection import train_test_split
from sklearn.metrics import accuracy_score, f1_score, roc_auc_score
from sklearn.preprocessing import StandardScaler
import numpy as np
from sklearn.metrics import confusion_matrix
from xgboost import XGBClassifier
import matplotlib.pyplot as plt
import seaborn as sns
import pandas as pd
from sklearn.decomposition import PCA
import networkx as nx
import os

def extract_embeddings(model, data_loader, config, category):
    """提取图嵌入特征"""
    model.eval()
    all_embeddings = []
    all_labels = []
    all_indexes = []
    all_languages = []
    all_cve_ids = []
    all_betas = []
    all_funcs = []
    
    with torch.no_grad():
        for batch_id, items in enumerate(data_loader):
            idx, graphs, labels, languages, cve_ids, funcs = items['idx'], items['graph'], items['label'], items['language'], items['cve_id'], items['func']
            # 数据转移到设备
            graphs = graphs.to(config.device)
            labels = labels.to(config.device)
            
            # 处理输入特征
            if 'h' in graphs.ndata:
                feats = graphs.ndata['h']
            elif 'feature' in graphs.ndata:
                feats = graphs.ndata['feature']
            else:
                raise ValueError("Missing node features")
                
            if not isinstance(feats, dict):
                feats = {category: feats}
            
            # 生成位置编码（与原代码一致）
            pdg = graphs.adj(etype="pdg").to_dense()
            ast = graphs.adj(etype="ast").to_dense()
            cfg = graphs.adj(etype="cfgcdg").to_dense()
            pos = ((pdg + ast + cfg) >= 3).float().fill_diagonal_(1).to_sparse().cuda()
            
            # 多跳特征处理 [metapath, node, hop, feature]
            multi_hop = graphs.ndata['multi_hop_feature'].permute(1, 0, 2, 3)
            # multi_hop = multi_hop[0].unsqueeze(0)  # 只取第cfg图的多跳特征
            # 获取图嵌入
            embeddings = model(
                g=graphs, 
                feats=feats, 
                multi_hop_features=multi_hop,
                pos=pos,
                mode='pred'  # 预训练模式返回嵌入
            )
            # beta = model.sematic_attention.beta_values
            beta = model.type_attention.alpha
            all_betas.append(np.mean(beta.T, axis=0))
            
            all_embeddings.append(embeddings.cpu())
            all_labels.append(labels.cpu())
            all_indexes.append(idx)
            all_cve_ids.append(cve_ids)
            all_languages.append(languages)
            all_funcs.append(funcs)
    
    return {
        'embeddings': torch.cat(all_embeddings).numpy(),
        'labels': torch.cat(all_labels).numpy(),
        'indexes': torch.cat(all_indexes).numpy(),
        'cve_ids': [item for sublist in all_cve_ids for item in sublist],
        'languages': [item for sublist in all_languages for item in sublist],
        "betas": np.stack(all_betas),
        "funcs": [item for sublist in all_funcs for item in sublist]
    }

class ClassifierTrainer:
    """分类器训练器"""
    def __init__(self, device='cuda'):
        self.device = device
        self.best_metrics = {}
        self.scalers = {}


    def _plot_distribution(self, args, train_data, test_data):
        """数据分布可视化（分开展示）"""
        # 设置学术图表样式
        plt.style.use('seaborn-v0_8-whitegrid')
        plt.rcParams.update({
            'font.family': 'Times New Roman',
            'axes.labelsize': 12,
            'xtick.labelsize': 10,
            'ytick.labelsize': 10,
            'savefig.dpi': 300,
            'savefig.format': 'pdf',
            'savefig.bbox': 'tight',
            'savefig.pad_inches': 0.1
        })

        # 语言分布图
        self._plot_lang_distribution(args, train_data, test_data)
        
        # 特征空间分布图
        self._plot_feature_distribution(args, train_data, test_data)

        # 语言-数据集类型-漏洞类型热力图
        self._plot_cve_language_heatmap(args, train_data, test_data)

        # sematic attention分布
        self._visualize_attention(args, train_data['betas'], test_data['betas'])
        

    def _visualize_attention(self, args, train_betas, test_betas, meta_path_names=["ast", "cfgcdg", "pdg"]):
        plt.figure(figsize=(10, 6))
        # 绘制整体分布
        plt.subplot(1, 2, 1)
        sns.violinplot(data=train_betas)
        plt.title("Attention Distribution")
        plt.xlabel("Meta Path Index")
        # 绘制平均权重
        plt.subplot(1, 2, 2)
        mean_betas = np.mean(train_betas, axis=0)
        plt.bar(range(len(meta_path_names)), mean_betas)
        plt.xticks(range(len(meta_path_names)), meta_path_names, rotation=45)
        plt.title("Average Attention Weights")
        plt.tight_layout()
        plt.savefig(f'{args.result_dir}/train_{args.train_mode}_attention_distribution.svg')
        plt.close()

        plt.figure(figsize=(10, 6))
        # 绘制整体分布
        plt.subplot(1, 2, 1)
        sns.violinplot(data=test_betas)
        plt.title("Attention Distribution")
        plt.xlabel("Meta Path Index")
        # 绘制平均权重
        plt.subplot(1, 2, 2)
        mean_betas = np.mean(test_betas, axis=0)
        plt.bar(range(len(meta_path_names)), mean_betas)
        plt.xticks(range(len(meta_path_names)), meta_path_names, rotation=45)
        plt.title("Average Attention Weights")
        
        plt.tight_layout()
        plt.savefig(f'{args.result_dir}/test_{args.train_mode}_attention_distribution.svg')
        plt.close()

    def _plot_lang_distribution(self, args, train_data, test_data):
        """绘制语言分布柱状图"""
        plt.figure(figsize=(8, 5))
        
        # 准备数据
        lang_counts = pd.concat([
            pd.Series(train_data['languages']).value_counts().rename('Train'),
            pd.Series(test_data['languages']).value_counts().rename('Test')
        ], axis=1).fillna(0)
        
        # 绘制横向柱状图避免标签重叠
        ax = lang_counts.plot(kind='barh', width=0.8, alpha=0.8)
        
        # 样式调整
        ax.set_title('Programming Language Distribution', pad=20, fontsize=14)
        ax.set_xlabel('Sample Count', labelpad=10)
        ax.set_ylabel('Language', labelpad=10)
        ax.xaxis.grid(True, linestyle='--', alpha=0.6)
        ax.yaxis.grid(False)
        
        # 添加数值标签
        for container in ax.containers:
            ax.bar_label(container, fmt='%d', padding=3, fontsize=9)
        
        plt.savefig(f'{args.result_dir}/{args.train_mode}_language_distribution.svg')
        plt.close()

    def _plot_cve_language_heatmap(self, args, train_data, test_data):
        """绘制CVE-语言热力图"""
        cross_table_train = pd.crosstab(train_data['cve_ids'], train_data['languages'])
        plt.figure(figsize=(10, 6))
        ax = sns.heatmap(cross_table_train.T, cmap='YlGnBu')
        plt.xlabel("CVE ID", fontsize=10)
        plt.ylabel("Languages", fontsize=10)
        plt.savefig(f'{args.result_dir}/train_cve_language_heatmap.svg')

        cross_table_test = pd.crosstab(test_data['cve_ids'], test_data['languages'])
        plt.figure(figsize=(10, 6))
        ax = sns.heatmap(cross_table_test.T, cmap='YlGnBu')
        plt.xlabel("CVE ID", fontsize=10)
        plt.ylabel("Languages", fontsize=10)
        plt.savefig(f'{args.result_dir}/{args.train_mode}_test_cve_language_heatmap.svg')

        

    def _plot_feature_distribution(self, args, train_data, test_data):
        """绘制特征空间分布图（2D和3D）"""
        # 合并语言标签并创建颜色映射
        all_languages = np.concatenate([train_data['languages'], test_data['languages']])
        unique_langs = np.unique(all_languages)
        colors = plt.cm.tab20(np.linspace(0, 1, len(unique_langs)))  # 使用tab20色系
        lang_color_map = {lang: colors[i] for i, lang in enumerate(unique_langs)}

        # 2D绘图
        plt.figure(figsize=(10, 8))
        ax = plt.gca()
        
        # PCA降维
        pca = PCA(n_components=2)
        combined = np.vstack([train_data['embeddings'], test_data['embeddings']])
        pca.fit(combined)
        
        # 绘制训练集和测试集
        for lang in unique_langs:
            # 训练数据
            train_mask = np.array([lang == languages for languages in train_data['languages']])
            if train_mask.any():
                points = pca.transform(train_data['embeddings'][train_mask])
                ax.scatter(points[:, 0], points[:, 1], 
                        color=lang_color_map[lang], marker='o', 
                        alpha=0.7, s=40, edgecolor='w', linewidth=0.5,
                        label=f'Train {lang}')
            
            # 测试数据
            test_mask = np.array([lang == languages for languages in test_data['languages']])
            if test_mask.any():
                points = pca.transform(test_data['embeddings'][test_mask])
                ax.scatter(points[:, 0], points[:, 1], 
                        color=lang_color_map[lang], marker='^', 
                        alpha=0.7, s=40, edgecolor='w', linewidth=0.5,
                        label=f'Test {lang}')

        # 图例和标签
        ax.set_title('2D Feature Space Distribution', pad=15, fontsize=14)
        ax.set_xlabel('Principal Component 1', labelpad=10)
        ax.set_ylabel('Principal Component 2', labelpad=10)
        
        # 创建分块图例
        handles, labels = ax.get_legend_handles_labels()
        unique_labels = dict(zip(labels, handles))
        ax.legend(unique_labels.values(), unique_labels.keys(), 
                loc='upper left', bbox_to_anchor=(1, 1))
        
        plt.savefig(f'{args.result_dir}/{args.train_mode}_feature_2d.svg', bbox_inches='tight')
        plt.close()

        # 3D绘图
        fig = plt.figure(figsize=(10, 8))
        ax = fig.add_subplot(111, projection='3d')
        
        # 3D PCA降维
        pca_3d = PCA(n_components=3)
        pca_3d.fit(combined)
        
        # 绘制三维散点
        for lang in unique_langs:
            # 训练数据
            train_mask = np.array([lang == languages for languages in train_data['languages']])
            if train_mask.any():
                points = pca_3d.transform(train_data['embeddings'][train_mask])
                ax.scatter(points[:, 0], points[:, 1], points[:, 2],
                        color=lang_color_map[lang], marker='o',
                        alpha=0.7, s=40, edgecolor='w', linewidth=0.5)
            
            # 测试数据
            test_mask = np.array([lang == languages for languages in test_data['languages']])
            if test_mask.any():
                points = pca_3d.transform(test_data['embeddings'][test_mask])
                ax.scatter(points[:, 0], points[:, 1], points[:, 2],
                        color=lang_color_map[lang], marker='^',
                        alpha=0.7, s=40, edgecolor='w', linewidth=0.5)

        # 3D图设置
        ax.set_title('3D Feature Space Distribution', pad=15, fontsize=14)
        ax.set_xlabel('PC1', labelpad=10)
        ax.set_ylabel('PC2', labelpad=10)
        ax.set_zlabel('PC3', labelpad=10)
        
        # 创建颜色图例
        legend_elements = [plt.Line2D([0], [0], marker='o', color='w', label=lang,
                                    markerfacecolor=lang_color_map[lang], markersize=8)
                        for lang in unique_langs]
        ax.legend(handles=legend_elements, loc='upper left', 
                bbox_to_anchor=(1.1, 1), title="Languages")

        plt.savefig(f'{args.result_dir}/{args.train_mode}_feature_3d.svg', bbox_inches='tight')
        plt.close()

    def _prepare_data(self, X, y):
        """数据预处理"""
        # 标准化特征
        self.scaler = StandardScaler()
        X = self.scaler.fit_transform(X)
        
        # 划分数据集
        X_train, X_val, y_train, y_val = train_test_split(
            X, y, test_size=0.2, random_state=42, stratify=y
        )
        
        # 转换为Tensor
        return (
            torch.FloatTensor(X_train).to(self.device),
            torch.FloatTensor(X_val).to(self.device),
            torch.FloatTensor(y_train).unsqueeze(1).to(self.device),
            torch.FloatTensor(y_val).unsqueeze(1).to(self.device)
        )

    def _calculate_metrics(self, y_true, y_pred, probas):
        """统一计算评估指标"""
        if isinstance(y_true, torch.Tensor):
            y_true = y_true.cpu().numpy()
        if isinstance(y_pred, torch.Tensor):
            y_pred = y_pred.cpu().numpy()

        acc = accuracy_score(y_true, y_pred)
        f1 = f1_score(y_true, y_pred)
        auc = roc_auc_score(y_true, probas)
        
        # 计算假阳率
        tn, fp, fn, tp = confusion_matrix(y_true, y_pred).ravel()
        fpr = fp / (fp + tn) if (fp + tn) > 0 else 0.0
        fnr = fn / (fn + tp) if (fn + tp) > 0 else 0.0
        
        return {
            'acc': round(acc, 4),
            'f1': round(f1, 4),
            'auc': round(auc, 4),
            'fpr': round(fpr, 4),
            'fnr': round(fnr, 4)
        }
    
    import os

    def test(self, args, model, test_data, model_type, indexes=None):
        X_test = test_data["embeddings"]
        y_test = test_data["labels"]
        functions = test_data["funcs"]

        assert model_type in ['mlp', 'svm', 'rf', 'lr', 'xgb'], "Invalid model type"

        # 参数检查
        if indexes is None:
            raise ValueError("indexes must be provided")

        # 获取对应模型的标准化器
        scaler = self.scalers.get(model_type)
        if scaler is None:
            raise ValueError(f"No scaler found for {model_type}")

        # 标准化测试数据
        X_test_scaled = scaler.transform(X_test)

        # 模型预测
        if model_type == 'mlp':
            X_test_tensor = torch.FloatTensor(X_test_scaled).to(self.device)
            with torch.no_grad():
                model.eval()
                logits = model(X_test_tensor)
                probas = torch.sigmoid(logits).cpu().numpy().flatten()
                preds = (probas > 0.5).astype(int)
        else:
            if model_type == 'svm':
                probas = model.decision_function(X_test_scaled)
                probas = 1 / (1 + np.exp(-probas))  # SVM 的 decision_function 转换为概率
            else:
                probas = model.predict_proba(X_test_scaled)[:, 1]
            preds = model.predict(X_test_scaled)

        # 计算置信度
        confidence = np.where(preds == 1, probas, 1 - probas)

        # 构建结果 DataFrame
        results_df = pd.DataFrame({
            'idx': indexes,
            'true_label': y_test,
            'pred_label': preds,
            'confidence': confidence,
            'function': functions
        })

        # 选择最佳案例和最差案例
        best_cases = results_df[results_df.true_label == results_df.pred_label].nlargest(5, 'confidence')
        worst_cases = results_df[results_df.true_label != results_df.pred_label].nlargest(5, 'confidence')

        # 创建临时文件夹（如果不存在）
        os.makedirs(args.tmp_folder, exist_ok=True)

        # 保存 C# 代码函数到文件
        def save_csharp_functions(cases, prefix):
            for idx, row in cases.iterrows():
                # 生成文件名（例如：best_case_123.cs）
                filename = f"{prefix}_case_{row['idx']}.cs"
                filepath = os.path.join(args.tmp_folder, filename)
                # 写入 C# 代码
                with open(filepath, 'w', encoding='utf-8') as f:
                    f.write(row['function'])

        # 保存最佳和最差案例
        save_csharp_functions(best_cases, "best")
        save_csharp_functions(worst_cases, "worst")

        return self._calculate_metrics(y_test, preds, probas)

    def train_mlp(self, X, y, input_dim, args):
        """训练神经网络分类器"""
        class MLP(nn.Module):
            def __init__(self, input_dim, hidden_dim=256):
                super().__init__()
                self.layers = nn.Sequential(
                    nn.Linear(input_dim, hidden_dim),
                    nn.ReLU(),
                    nn.Dropout(0.5),
                    nn.Linear(hidden_dim, 1)
                )
            
            def forward(self, x):
                return self.layers(x)

        # 数据准备
        X_train, X_val, y_train, y_val = self._prepare_data(X, y)
        self.scalers['mlp'] = self.scaler
        
        model = MLP(input_dim).to(self.device)
        num_pos = torch.sum(y_train).item()
        num_neg = len(y_train) - num_pos
        pos_weight = torch.tensor([num_neg / num_pos]).to(self.device)
        criterion = nn.BCEWithLogitsLoss(pos_weight=pos_weight)
        
        optimizer = torch.optim.Adam(model.parameters(), lr=1e-3, weight_decay=1e-4)  # 添加权重衰减
        scheduler = torch.optim.lr_scheduler.ReduceLROnPlateau(optimizer, mode='max', patience=10, factor=0.1) # 动态学习率调整 

        best_metrics = {'auc': 0}
        epochs_no_improve = 0
        early_stop_patience = 20
        for epoch in range(20000):
            model.train()
            optimizer.zero_grad()
            outputs = model(X_train)
            loss = criterion(outputs, y_train)
            loss.backward()
            optimizer.step()

            # 验证
            with torch.no_grad():
                model.eval()
                probas = torch.sigmoid(model(X_val))
                preds = (probas > 0.5).float()
                metrics = self._calculate_metrics(
                    y_val.cpu(), 
                    preds.cpu(), 
                    probas.cpu()
                )
                # 更新学习率
                scheduler.step(metrics['auc'])

                print(f"Epoch {epoch+1} | Loss: {loss.item():.4f} | "
                      f"Acc: {metrics['acc']} | F1: {metrics['f1']} | "
                      f"AUC: {metrics['auc']} | FPR: {metrics['fpr']}")

                if metrics['auc'] > best_metrics['auc']:
                    best_metrics = metrics
                    epochs_no_improve = 0
                    torch.save(model.state_dict(), f'{args.train_mode}_best_mlp.pth')
                else:
                    epochs_no_improve += 1
                    if epochs_no_improve >= early_stop_patience:
                        print(f"Early stopping at epoch {epoch+1}")
                        break

        print(f"Best AUC Metrics : Acc: {best_metrics['acc']} | F1: {best_metrics['f1']} | "
              f"AUC: {best_metrics['auc']} | FPR: {best_metrics['fpr']}")
        model.load_state_dict(torch.load(f'{args.train_mode}_best_mlp.pth'))

        with torch.no_grad():
            probas_val = torch.sigmoid(model(X_val)).cpu().numpy()
            y_val_np = y_val.cpu().numpy()
            
            # 寻找最佳阈值（基于FPR最小化）
            thresholds = np.linspace(0, 1, 100)
            best_threshold = 0.5
            best_fpr = 1.0
            for th in thresholds:
                preds = (probas_val > th).astype(float)
                fpr = np.sum((preds == 1) & (y_val_np == 0)) / np.sum(y_val_np == 0)
                if fpr < best_fpr:
                    best_fpr = fpr
                    best_threshold = th

            print(f"Optimal Threshold: {best_threshold:.4f} (FPR={best_fpr:.4f})")        
            # 更新最终模型的预测阈值
            self.mlp_threshold = best_threshold

        return model

    def train_sklearn_model(self, X, y, model_type='svm'):
        """训练传统机器学习模型"""
        X_train, X_val, y_train, y_val = train_test_split(
            X, y, test_size=0.2, random_state=42, stratify=y
        )

        # 标准化处理
        self.scaler = StandardScaler()
        X_train = self.scaler.fit_transform(X_train)
        X_val = self.scaler.transform(X_val)
        self.scalers[model_type] = self.scaler

        model = None
        if model_type == 'svm':
            model = SVC(kernel='rbf', probability=True, class_weight='balanced')
        elif model_type == 'rf':
            model = RandomForestClassifier(n_estimators=300, class_weight='balanced')
        elif model_type == 'lr':
            model = LogisticRegression(class_weight='balanced')
        elif model_type == 'xgb':
            scale_pos_weight = sum(y == 0) / sum(y == 1)  # 处理类别不平衡
            model = XGBClassifier(
                n_estimators=200,
                max_depth=5,
                learning_rate=0.1,
                subsample=0.8,
                colsample_bytree=0.8,
                scale_pos_weight=scale_pos_weight,
                use_label_encoder=False,
                eval_metric='logloss'
            )
        else:
            raise ValueError("Unsupported model type")

        model.fit(X_train, y_train)
        
        # 预测和评估
        probas = model.predict_proba(X_val)[:,1] if model_type != 'svm' else model.decision_function(X_val)
        preds = model.predict(X_val)
        
        metrics = self._calculate_metrics(y_val, preds, probas)
        
        print(f"{model_type.upper()} Results:")
        print(f"Acc: {metrics['acc']} | F1: {metrics['f1']} | "
              f"AUC: {metrics['auc']} | FPR: {metrics['fpr']}")
        return model
