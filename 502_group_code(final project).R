# -----------------------------
# Step 0: 加载所需包
# -----------------------------
library(ggplot2)
library(lme4)
library(lmerTest)     # 用于 Satterthwaite's df 和显著性
library(emmeans)      # 可选：用于估计边际均值
library(dplyr)
library(car)
library(ExpDes)

# -----------------------------
# Step 1: 读取与预处理数据
# -----------------------------
url <- "https://users.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Appendix%20C%20Data%20Sets/APPENC06.txt"
x <- read.table(url, header = FALSE)

colnames(x) <- c("ID", "WebsitesDelivered", "Backlog", "Team", "Experience", "ProcessChange", "Year", "Quarter")

# -----------------------------
# Step 2: 构造分类变量（因子化）
# -----------------------------
x <- subset(x, !(Team %in% c(12))) # 删除 Team 12：仅包含低经验组，缺乏组内比较，违背 split-plot 思路

x$ExpGroup <- ifelse(x$Experience <= 9, "Low", "High")
x$ExpGroup <- factor(x$ExpGroup, levels = c("Low", "High"))

x$BackGroup <- ifelse(x$Backlog <= 28, "Low", "High") # backlog 中位数为 28
x$BackGroup <- factor(x$BackGroup, levels = c("Low", "High"))

x$Team <- factor(x$Team)
x$ProcessChange <- factor(x$ProcessChange)

# -----------------------------
# PART 1: BackGroup × ExpGroup 模型
# -----------------------------

# 可视化：分组散点图
ggplot(x, aes(x = Backlog, y = WebsitesDelivered, color = Team)) +
  geom_point() +
  facet_wrap(~ ExpGroup) +
  theme_bw() +
  labs(title = "Backlog vs Websites Delivered by Team (Faceted by Experience Group)")

# 模型1：包含交互项
model_exp_inter <- lmer(WebsitesDelivered ~ BackGroup * ExpGroup + (1 | Team), data = x, REML = TRUE)

# 模型2：仅包含主效应
model_exp_add <- lmer(WebsitesDelivered ~ BackGroup + ExpGroup + (1 | Team), data = x, REML = TRUE)

# 模型比较（使用 ML 而非 REML）
model_exp_inter_ML <- update(model_exp_inter, REML = FALSE)
model_exp_add_ML <- update(model_exp_add, REML = FALSE)

anova(model_exp_add_ML, model_exp_inter_ML)  # likelihood ratio test

# 主效应显著性分析
summary(model_exp_add)
anova(model_exp_add)

## 最后选择只包含主效应

# 模型诊断
qqnorm(resid(model_exp_add)); qqline(resid(model_exp_add), col = 'red')

# -----------------------------
# PART 2: 考虑 ProcessChange（三因子交互）
# -----------------------------

# 数据结构检查
table(x$BackGroup, x$ExpGroup, x$ProcessChange)  # 检查是否存在不平衡或缺失组合

# 可视化
ggplot(x, aes(x = Backlog, y = WebsitesDelivered, color = Team, shape = ProcessChange)) +
  geom_point() +
  facet_wrap(~ ExpGroup) +
  theme_bw() +
  labs(title = "Backlog vs Websites Delivered (Faceted by Experience and Process Change)")

# 模型1：三因子交互模型（有 rank deficiency 警告）
model_proc_inter <- lmer(WebsitesDelivered ~ BackGroup * ExpGroup * ProcessChange + (1 | Team), data = x)

# 警告说明：
# → 由于某些组组合缺失（如某经验组中未出现流程变更）
# → R 会自动剔除无法估计的列，交互项结果有缺失

# 模型2：仅包含主效应
model_proc_add <- lmer(WebsitesDelivered ~ BackGroup + ExpGroup + ProcessChange + (1 | Team), data = x)

summary(model_proc_add)
anova(model_proc_add)

# 模型诊断
qqnorm(resid(model_proc_add)); qqline(resid(model_proc_add), col = 'red')

# -----------------------------
# ✅ 总结建议
# -----------------------------
# - 分析表明：经验组和 backlog 都对交付有显著主效应，但无显著交互；
# - 流程变更对交付具有显著影响；
# - 三因子交互模型因数据结构不完整而 rank deficient，结果不可解释；
# - 最终推荐使用简化主效应模型（BackGroup + ExpGroup + ProcessChange + (1|Team)）

