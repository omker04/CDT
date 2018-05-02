library(data.table)
library(openxlsx)
library(dplyr)
library(RColorBrewer)
library(dendextend)
library(ape)
library(bit64)

extended_basket_data <- read.xlsx(xlsxFile = "/Users/omahala/Desktop/GM Insights/graph partition/item cluster/CDT_GM/GM_CDT_matrices.xlsx", sheet = 1, startRow = 1, rowNames = TRUE, colNames = TRUE)
multi_attribute_data <- read.xlsx(xlsxFile = "/Users/omahala/Desktop/GM Insights/graph partition/item cluster/CDT_GM/GM_CDT_matrices.xlsx", sheet = 2, startRow = 1, rowNames = TRUE, colNames = TRUE)
text_clustering_data <- read.xlsx(xlsxFile = "/Users/omahala/Desktop/GM Insights/graph partition/item cluster/CDT_GM/GM_CDT_matrices.xlsx", sheet = 3, startRow = 1, rowNames = TRUE, colNames = TRUE)

label_df <- data.frame(fread("/Users/omahala/Desktop/GM Insights/graph partition/item cluster/CDT_GM/Assortment Description.csv"))
label_df$Rollup_id <- as.numeric(label_df$Rollup_id)

adj.mtx <- as.matrix(extended_basket_data)
diag(adj.mtx) <- 0
R_item <- as.matrix(extended_basket_data)[1:908,1:908]
R_dist <- as.matrix(extended_basket_data)

adj.mtx <- adj.mtx/max(adj.mtx)
R_item <- R_item/max(R_item)
R_dist <- R_dist/max(R_dist)

agg.mtx <- adj.mtx * R_item * R_dist
agg.mtx <- agg.mtx/max(agg.mtx)

match <- which(rownames(agg.mtx) %in% label_df$Rollup_id)
agg.mtx <- agg.mtx[match, match]
row.names(agg.mtx) <- inner_join(data.frame(a=as.numeric(row.names(agg.mtx))), label_df, by = c("a"="Rollup_id"))$Leaf_labels

hclust <- hclust(as.dist(agg.mtx))



clusters <- cutree(hclust, 30)
cols <- c(brewer.pal(10,"Paired"),brewer.pal(9,"Set1")[-(1:6)],brewer.pal(8,"Dark2")[-c(2,3,5)],brewer.pal(8,"Accent")[5:7],brewer.pal(8,"Set2"),brewer.pal(10,"Spectral"))
par(mar = c(0,0,0,1))
pdf("Text_CDT_generic.pdf", height = 200, width = 150)
plot(compute.brlen(as.phylo(hclust), power = 0.9), tip.color = cols[clusters], label.offset = 0.005, cex = 0.2)
title(main = "Text based CDT for project_id generic", font.main = 2, cex.main = 3)
dev.off()

item_cluster <- data.frame(item=hclust$labels[hclust$order], cluster=clusters[hclust$order])
write.csv(item_cluster[nrow(item_cluster):1,], "text_item_cluster_assignment_generic.csv", row.names = FALSE)

