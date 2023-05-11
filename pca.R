library(ca)
data(iris)
# log transform 
log.ir <- log(iris[, 1:4])
ir.species <- iris[, 5]
# apply PCA - scale. = TRUE is highly advisable, but default is FALSE. 
ir.pca <- prcomp(log.ir,center = TRUE, scale. = TRUE)
library(ggbiplot)
g <- ggbiplot(ir.pca, choices = c(2,4), obs.scale = 1, var.scale = 1, groups = ir.species)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', legend.position = 'top')
print(g)
ggbiplot(ir.pca, choices = c(1,2), obs.scale = 1, var.scale = 1, groups = ir.species) + scale_color_discrete(name = '')g + theme(legend.direction = 'horizontal', legend.position = 'top')
# Test

library(MASS)
library(ggbiplot)
data(iris)

# 將 iris 資料集的類別變數和數值變數分開
iris_cat <- iris[,5]
iris_num <- iris[,1:4]
# correlation matrix
corr_matrix <- round(cor(iris_num), 2)
p.mat <- cor_pmat(corr_matrix)
ggcorrplot(corr_matrix, hc.order = TRUE, lab = TRUE, p.mat = p.mat, colors = c("#6D9EC1", "white", "#E46726"),method = "square")
# 進行對應分析
ca_res <- corresp(iris_num)

# Pareto plot
vars <- (ir.pca$sdev)^2
props <- vars / sum(vars)
cumulative.props <- cumsum(props)
fviz_eig(ir.pca, addlabels = TRUE)

# 提取行得分和列得分
row_scores <- ca_res$row.scores
col_scores <- ca_res$col.scores

# 繪製 biplot
ggbiplot(ca_res, obs.scale = 1, var.scale = 1) +
  scale_color_discrete(name = '') +
  scale_shape_manual(values = c(15, 17, 16)) +
  theme(legend.direction = 'horizontal', legend.position = 'top')
