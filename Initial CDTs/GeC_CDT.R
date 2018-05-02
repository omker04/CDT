library(igraph)
library(Matrix)
library(data.table)
library(bit64)
library(dplyr)
library(arules)
library(RColorBrewer)
library(dendextend)
library(ape)
library(wordcloud)

setwd("/Users/omahala/Desktop/GM Insights/GeC data/CDT")
data_GeC <- data.frame(fread("allscores.csv"))

data_GeC$Target_item_join <- paste(data_GeC$Target_item, data_GeC$Target_item_text, sep = "--")
data_GeC$Complimentary_item_join <- paste(data_GeC$Complimentary_item, data_GeC$Complimentary_item_text, sep = "--")
data_GeC <- data_GeC[,c(9,10,6:8)]

graph_score <- graph_from_data_frame(data_GeC[,1:2], directed = TRUE)
E(graph_score)$weights <- data_GeC$Score
c_score <- cluster_walktrap(graph_score, weights = E(graph_score)$weights)
hc <- as.dendrogram(c_score)
clusters <- as.vector(cutree(hc, 58))
cols <- c(brewer.pal(10,"Paired"),brewer.pal(9,"Set1")[-(1:6)],brewer.pal(8,"Dark2")[-c(2,3,5)],brewer.pal(8,"Accent")[5:7],brewer.pal(8,"Set2"),brewer.pal(8,"Accent")[1:3],brewer.pal(10,"Spectral"), brewer.pal(10,"BrBG"), brewer.pal(10,"RdBu"))
dend <- branches_attr_by_clusters(hc, clusters[order.dendrogram(hc)], values=cols) %>% color_labels(col=cols[clusters])
pdf("dendrogram_score.pdf",height = 70, width = 35)
par(mar=c(0,0,5,1))
plot(compute.brlen(as.phylo(flatten.dendrogram(dend)),power=0.9),tip.color = cols[clusters],label.offset = 0.02, cex=1)
title(main = "Graph Partition based CDT for scores", font.main=2, cex.main=5)
dev.off()

item_cluster <- data.frame("sl_no" = 1:c_score$vcount, "item" = c_score$names, "cluster" = clusters)
item_cluster <- inner_join(data.frame(a = order.dendrogram(hc)), item_cluster, by = c("a" = "sl_no"))
write.csv(item_cluster[c_score$vcount:1,-1], "item_cluster_assignment_score.csv", row.names = FALSE)

graph_cobought <- graph_from_data_frame(data_GeC[,1:2], directed = TRUE)
E(graph_cobought)$weights <- data_GeC$Cobought
c_cobought <- cluster_walktrap(graph_cobought, weights = E(graph_cobought)$weights)
hc <- as.dendrogram(c_cobought)
clusters <- as.vector(cutree(hc, 16))
cols <- c(brewer.pal(10,"Paired"),brewer.pal(9,"Set1")[-(1:6)],brewer.pal(8,"Dark2")[-c(2,3,5)],brewer.pal(8,"Accent")[5:7],brewer.pal(8,"Set2"),brewer.pal(8,"Accent")[1:3],brewer.pal(10,"Spectral"), brewer.pal(10,"BrBG"), brewer.pal(10,"RdBu"))
dend <- branches_attr_by_clusters(hc, clusters[order.dendrogram(hc)], values=cols) %>% color_labels(col=cols[clusters])
pdf("dendrogram_cobought.pdf",height = 70, width = 35)
par(mar=c(0,0,5,1))
plot(compute.brlen(as.phylo(flatten.dendrogram(dend)),power=0.9),tip.color = cols[clusters],label.offset = 0.02, cex=1)
title(main = "Graph Partition based CDT for coboughts", font.main=2, cex.main=5)
dev.off()

item_cluster <- data.frame("sl_no" = 1:c_score$vcount, "item" = c_score$names, "cluster" = clusters)
item_cluster <- inner_join(data.frame(a = order.dendrogram(hc)), item_cluster, by = c("a" = "sl_no"))
write.csv(item_cluster[c_score$vcount:1,-1], "item_cluster_assignment_cobought.csv", row.names = FALSE)

graph_totalview <- graph_from_data_frame(data_GeC[,1:2], directed = TRUE)
E(graph_totalview)$weights <- data_GeC$totalview
c_totalview <- cluster_walktrap(graph_totalview, weights = E(graph_totalview)$weights)
hc <- as.dendrogram(c_totalview)
clusters <- as.vector(cutree(hc, 48))
cols <- c(brewer.pal(10,"Paired"),brewer.pal(9,"Set1")[-(1:6)],brewer.pal(8,"Dark2")[-c(2,3,5)],brewer.pal(8,"Accent")[5:7],brewer.pal(8,"Set2"),brewer.pal(8,"Accent")[1:3],brewer.pal(10,"Spectral"), brewer.pal(10,"BrBG"), brewer.pal(10,"RdBu"))
dend <- branches_attr_by_clusters(hc, clusters[order.dendrogram(hc)], values=cols) %>% color_labels(col=cols[clusters])
pdf("dendrogram_totalview.pdf",height = 70, width = 35)
par(mar=c(0,0,5,1))
plot(compute.brlen(as.phylo(flatten.dendrogram(dend)),power=0.9),tip.color = cols[clusters],label.offset = 0.02, cex=1)
title(main = "Graph Partition based CDT for totalviews", font.main=2, cex.main=5)
dev.off()

item_cluster <- data.frame("sl_no" = 1:c_score$vcount, "item" = c_score$names, "cluster" = clusters)
item_cluster <- inner_join(data.frame(a = order.dendrogram(hc)), item_cluster, by = c("a" = "sl_no"))
write.csv(item_cluster[c_score$vcount:1,-1], "item_cluster_assignment_totalview.csv", row.names = FALSE)



data_GeC$Score <- data_GeC$Score/max(data_GeC$Score)
data_GeC$Cobought <- data_GeC$Cobought/max(data_GeC$Cobought)
data_GeC$totalview <- data_GeC$totalview/max(data_GeC$totalview)

simulation <- function(i){
  alpha <- runif(3,0,1)
  alpha <- alpha/sum(alpha)
  graph_simulation <- graph_from_data_frame(data_GeC[,1:2], directed = TRUE)
  E(graph_simulation)$weights <- rowSums(alpha*data_GeC[,3:5])
  c_simulation <- cluster_walktrap(graph_simulation, weights = E(graph_simulation)$weights)
  simulation <- vector("list", 3)
  simulation[[1]] <- alpha
  simulation[[2]] <- max(c_simulation$modularity)
  simulation[[3]] <- max(c_simulation$membership)
  return(simulation)
}
simulation_result <- list()

for(i in 1:100000){
  sim_i <- simulation(i)
  simulation_result$alpha[[i]] <- sim_i[[1]]
  simulation_result$modularity[i] <- sim_i[[2]]
  simulation_result$membership[i] <- sim_i[[3]]
}

simulation_result1 <- list()
multi_weight <- data.frame("w1"=rep(1:50, each=2500), "w2"=rep(1:50, each=50), w3=1:50)
multi_weight <- multi_weight/rowSums(multi_weight)
sim <- function(i){
  graph_simulation <- graph_from_data_frame(data_GeC[,1:2], directed = TRUE)
  E(graph_simulation)$weights <- rowSums(t(multi_weight[i,])*data_GeC[,3:5])
  c_simulation <- cluster_walktrap(graph_simulation, weights = E(graph_simulation)$weights)
  simulation <- vector("list", 3)
  simulation[[1]] <- max(c_simulation$modularity)
  simulation[[2]] <- max(c_simulation$membership)
  return(simulation)
}

for(i in 1:125000){
  sim_i <- sim(i)
  simulation_result1$modularity[i] <- sim_i[[1]]
  simulation_result1$membership[i] <- sim_i[[2]]
}
