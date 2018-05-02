library(dendextend)

hc1=as.dendrogram(hclust(dist(mtcars)))
clus1 <- cutree(hc1, 7)
remove_labels <- c("Maserati Bora", "Fiat X1-9", "AMC Javelin")    #label for the items to be removed or pruned
hc0 <- prune(hc1, remove_labels)
clus0 <- clus1
clus0[which(names(clus1) %in% remove_labels)] <- 0
cols <- c(brewer.pal(10,"Paired"),brewer.pal(9,"Set1")[-(1:6)],brewer.pal(8,"Dark2")[-c(2,3,5)],brewer.pal(8,"Accent")[5:7],brewer.pal(8,"Set2"),brewer.pal(8,"Accent")[1:3],brewer.pal(10,"Spectral"), brewer.pal(10,"BrBG"), brewer.pal(10,"RdBu"))
dend1 <- branches_attr_by_clusters(hc1, clus1[order.dendrogram(hc1)], values=cols) %>% color_labels(col=cols[clus1])
dend0 <- branches_attr_by_clusters(hc0, clus0[order.dendrogram(hc0)], values=cols) %>% color_labels(col=cols[clus0])
order.dendrogram(dend0) <- rank(order.dendrogram(dend0))

pdf("Dendrogram_pruning.pdf")
plot(compute.brlen(as.phylo(dend1), power = 0.9), tip.color = cols[clus1], label.offset = 0.005, cex = 1)
title(main = "mtcars dendrogram", cex.main=1)
plot(compute.brlen(as.phylo(dend0), power = 0.9), tip.color = cols[clus0], label.offset = 0.005, cex = 1)
title(main = "mtcars dendrogram after pruning Maserati Bora, Fiat X1-9, AMC Javelin", cex.main=1)
dev.off()
