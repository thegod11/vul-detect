import torch
import torch.nn as nn
import torch.nn.functional as F
import numpy as np

class GraphLevelContrast(nn.Module):
    def __init__(self, hidden_dim, tau=0.8, alpha=0.7):
        super().__init__()
        self.tau = tau
        self.alpha = alpha  # 跨语言对比权重
        self.eps = 1e-8
        self.proj = nn.Sequential(
            nn.Linear(hidden_dim, hidden_dim),
            nn.ReLU(),
            nn.Linear(hidden_dim, hidden_dim)
        )
        
    def forward(self, graph_emb, cve_ids, languages):
        """
        graph_emb: 图嵌入矩阵 [N, D]
        labels: 漏洞标签 [N]
        languages: 语言标识 [N]
        """
        # 标准化投影
        z = F.normalize(self.proj(graph_emb), dim=1)
        
        # 语言内对比损失
        intra_loss = self._contrastive_loss(z, cve_ids, languages, same_lang=True)
        
        # 跨语言对比损失
        cross_loss = self._contrastive_loss(z, cve_ids, languages, same_lang=False)
        
        return (1-self.alpha)*intra_loss + self.alpha*cross_loss
    
    def _contrastive_loss(self, z, cve_ids, languages, same_lang=True):
        sim = torch.exp(torch.mm(z, z.t()) / self.tau).cpu()
        mask = self._create_mask(cve_ids, same_lang, languages)
        pos = (sim * mask).sum(dim=1)
        neg = (sim * ~mask).sum(dim=1)
        
        # **防止 log(0) 和 0/0 问题**
        pos = torch.clamp(pos, min=self.eps)  # 确保 pos 至少是 eps
        denominator = torch.clamp(pos + neg, min=self.eps)  # 确保分母不为 0
        return -torch.log(pos / denominator).mean()
    
    def _create_mask(self, cve_ids, same_lang, languages):
        # 构造唯一值到索引的映射
        cve_id_map = {cve: idx for idx, cve in enumerate(set(cve_ids))}
        lang_map = {lang: idx for idx, lang in enumerate(set(languages))}

        # 转换为索引
        cve_ids_tensor = torch.tensor([cve_id_map[cve] for cve in cve_ids], dtype=torch.long)
        languages_tensor = torch.tensor([lang_map[lang] for lang in languages], dtype=torch.long)

        # 计算掩码
        cve_ids_mask = cve_ids_tensor.unsqueeze(1) == cve_ids_tensor.unsqueeze(0)
        lang_mask = languages_tensor.unsqueeze(1) == languages_tensor.unsqueeze(0)

        return cve_ids_mask & (lang_mask if same_lang else ~lang_mask)

class CVEContrastiveLoss(nn.Module):
    def __init__(self, hidden_dim, tau=0.8, alpha=0):
        super().__init__()
        self.tau = tau
        self.eps = 1e-8
        self.proj = nn.Sequential(
            nn.Linear(hidden_dim, hidden_dim),
            nn.ReLU(),
            nn.Linear(hidden_dim, hidden_dim)
        )

    def forward(self, graph_emb, labels, lan):
        """
        Args:
            graph_emb: 图嵌入向量 [batch_size, hidden_dim]
            labels: CVE ID列表 [batch_size]
        Returns:
            contrastive_loss: 对比损失值
        """
        # 构造CVE ID到索引的映射
        unique_cves = list(set(labels))
        cve_id_map = {cve: idx for idx, cve in enumerate(unique_cves)}
        
        # 转换为索引张量
        cve_indices = torch.tensor([cve_id_map[cve] for cve in labels], 
                                 dtype=torch.long, 
                                 device=graph_emb.device)

        # 投影并归一化
        z = F.normalize(self.proj(graph_emb), dim=1)
        
        # 计算相似度矩阵
        sim_matrix = torch.exp(torch.mm(z, z.t()) / self.tau)
        
        # 构建正负样本掩码
        pos_mask = (cve_indices.unsqueeze(1) == cve_indices.unsqueeze(0))  # 正样本对
        neg_mask = ~pos_mask  # 负样本对
        
        # 计算正负样本相似度
        pos_sim = (sim_matrix * pos_mask).sum(dim=1)  # 每个样本的正对相似度和
        neg_sim = (sim_matrix * neg_mask).sum(dim=1)  # 每个样本的负对相似度和
        
        # 数值稳定性处理
        pos_sim = torch.clamp(pos_sim, min=self.eps)
        neg_sim = torch.clamp(neg_sim, min=self.eps)
        
        # 计算对比损失
        loss = -torch.log(pos_sim / (pos_sim + neg_sim)).mean()
        
        return loss
    
class GraphLevelContrast_new(nn.Module):
    def __init__(self, hidden_dim, tau=0.8, alpha=0.7):
        super().__init__()
        self.tau = tau
        self.alpha = alpha
        self.eps = 1e-8
        self.proj = nn.Sequential(
            nn.Linear(hidden_dim, hidden_dim),
            nn.ReLU(),
            nn.Linear(hidden_dim, hidden_dim)
        )
        
    def forward(self, graph_emb, labels, languages):

        # 构造唯一值到索引的映射
        cve_id_map = {cve: idx for idx, cve in enumerate(set(labels))}
        lang_map = {lang: idx for idx, lang in enumerate(set(languages))}

        # 转换为索引
        cve_ids_tensor = torch.tensor([cve_id_map[cve] for cve in labels], dtype=torch.long)
        languages_tensor = torch.tensor([lang_map[lang] for lang in languages], dtype=torch.long)

        z = F.normalize(self.proj(graph_emb), dim=1)
        sim = torch.exp(torch.mm(z, z.t()) / self.tau).cpu()
        label_mask = cve_ids_tensor.unsqueeze(1) == cve_ids_tensor.unsqueeze(0)
        lang_mask = languages_tensor.unsqueeze(1) == languages_tensor.unsqueeze(0)
        intra_pos_mask = label_mask & lang_mask
        cross_pos_mask = label_mask & ~lang_mask
        neg_mask = cve_ids_tensor.unsqueeze(1) != cve_ids_tensor.unsqueeze(0)

        intra_pos = (sim * intra_pos_mask).sum(dim=1)
        cross_pos = (sim * cross_pos_mask).sum(dim=1)
        neg = (sim * neg_mask).sum(dim=1)

        intra_pos = torch.clamp(intra_pos, min=self.eps)
        cross_pos = torch.clamp(cross_pos, min=self.eps)
        denominator = torch.clamp(neg, min=self.eps)
        # intra_denominator = torch.clamp(intra_pos + neg, min=self.eps)
        # cross_denominator = torch.clamp(cross_pos + neg, min=self.eps)

        intra_loss = -torch.log(intra_pos / denominator).mean()
        cross_loss = -torch.log(cross_pos / denominator).mean()

        return (1-self.alpha)*intra_loss + self.alpha*cross_loss
    

class Contrast(nn.Module):
    def __init__(self, hidden_dim, tau, lam):
        super(Contrast, self).__init__()
        self.proj = nn.Sequential(
            nn.Linear(hidden_dim, hidden_dim),
            nn.ELU(),
            nn.Linear(hidden_dim, hidden_dim)
        )
        self.tau = tau
        self.lam = lam
        for model in self.proj:
            if isinstance(model, nn.Linear):
                nn.init.xavier_normal_(model.weight, gain=1.414)

    def sim(self, z1, z2):
        z1_norm = torch.norm(z1, dim=-1, keepdim=True)
        z2_norm = torch.norm(z2, dim=-1, keepdim=True)
        dot_numerator = torch.mm(z1, z2.t())
        dot_denominator = torch.mm(z1_norm, z2_norm.t())
        sim_matrix = torch.exp(dot_numerator / dot_denominator / self.tau)
        return sim_matrix

    def forward(self, z_mp, z_sc, pos):
        z_proj_mp = self.proj(z_mp)
        z_proj_sc = self.proj(z_sc)
        matrix_mp2sc = self.sim(z_proj_mp, z_proj_sc)
        matrix_sc2mp = matrix_mp2sc.t()
        
        matrix_mp2sc = matrix_mp2sc / (torch.sum(matrix_mp2sc, dim=1).view(-1, 1) + 1e-8)
        lori_mp = -torch.log(matrix_mp2sc.mul(pos.to_dense()).sum(dim=-1)).mean()

        matrix_sc2mp = matrix_sc2mp / (torch.sum(matrix_sc2mp, dim=1).view(-1, 1) + 1e-8)
        lori_sc = -torch.log(matrix_sc2mp.mul(pos.to_dense()).sum(dim=-1)).mean()
        return self.lam * lori_mp + (1 - self.lam) * lori_sc
