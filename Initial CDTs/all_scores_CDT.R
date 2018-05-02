library(XLConnect)
library(igraph)
library(data.table)
library(bit64)
library(dplyr)
library(plyr)
library(arules)
library(RColorBrewer)
library(dendextend)
library(ape)
library(wordcloud)

setwd("/Users/omahala/Desktop/GM Insights/GeC data/CDT")
all_scores <- loadWorkbook("all_scores_2.xlsx")
all_scores_list = readWorksheet(all_scores, sheet = getSheets(all_scores))

gec_data <- all_scores_list$allscores.csv
gec_data <- gec_data[,c("New.Text_Target", "New.Text_Compliment", "Score")]
graph_score <- graph_from_data_frame(gec_data, directed = TRUE)
E(graph_score)$weights <- gec_data$Score

score_walktrap <- cluster_walktrap(graph_score, weights = gec_data$Score)

hc <- as.dendrogram(score_walktrap)
clusters <- as.vector(cutree(hc, 58))
cols <- c(brewer.pal(10,"Paired"),brewer.pal(9,"Set1")[-(1:6)],brewer.pal(8,"Dark2")[-c(2,3,5)],brewer.pal(8,"Accent")[5:7],brewer.pal(8,"Set2"),brewer.pal(8,"Accent")[1:3],brewer.pal(10,"Spectral"), brewer.pal(10,"BrBG"), brewer.pal(10,"RdBu"))
dend <- branches_attr_by_clusters(hc, clusters[order.dendrogram(hc)], values=cols) %>% color_labels(col=cols[clusters])
pdf("dendrogram_score_text_leaf.pdf",height = 80, width = 35)
par(mar=c(0,0,5,1))
plot(compute.brlen(as.phylo(flatten.dendrogram(dend)),power=0.9),tip.color = cols[clusters],label.offset = 0.02, cex=1)
title(main = "Graph Partition based CDT for scores", font.main=2, cex.main=5)
dev.off()
