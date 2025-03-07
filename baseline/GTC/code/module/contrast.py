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
