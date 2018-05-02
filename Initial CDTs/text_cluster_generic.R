library(data.table)
library(plyr)
library(dplyr)
library(ape)
library(dendextend)
library(RColorBrewer)
library(bit64)

setwd("/Users/omahala/Desktop/GM Insights/graph partition/item cluster")

item_data_generic <- data.frame(fread("item_price_generic.csv"))
colnames(item_data_generic) <- c("rollup_id", "rollup_desc", "price")
item_data_generic$rollup_id <- as.numeric(item_data_generic$rollup_id)
item_data_generic$item <- paste(item_data_generic$rollup_id, item_data_generic$rollup_desc, sep = "--")
select_data <- data.frame(fread("select_item_generic.csv"))
item_data_generic <- inner_join(data_frame(rollup_id = as.numeric(select_data[,2])), item_data_generic, by = c("rollup_id"="rollup_id"))

attribute_generic <- data.frame(fread("attribute_generic.csv"))
attribute_generic <- unique(attribute_generic[,c(1,2,3)])
colnames(attribute_generic) <- c("rollup_id", "rollup_desc", "brand")
attribute_generic$rollup_id <- as.numeric(attribute_generic$rollup_id)
attribute_generic <- inner_join(data_frame(rollup_id = as.numeric(select_data[,2])), attribute_generic, by = c("rollup_id"="rollup_id"))

attribute_generic$check <- 1
item_cnt <- data.frame(attribute_generic %>% group_by(rollup_id) %>% summarise(nbr=n()))
item_cnt <- item_cnt[which(item_cnt$nbr > 1),]
for(i in 1:nrow(attribute_generic)){
  if(attribute_generic$rollup_id[i] %in% item_cnt$rollup_id &
     !(as.vector(unlist(strsplit(attribute_generic$brand[i], split = " "))) %in% as.vector(unlist(strsplit(attribute_generic$rollup_desc[i], split=" ")))))
    attribute_generic$check[i] <- 0
}
rm(item_cnt)
attribute_generic <- attribute_generic[which(attribute_generic$check==1), 1:3]
attribute_generic$item <- paste(attribute_generic$rollup_id, attribute_generic$rollup_desc, sep = "--")

item_data_generic <- inner_join(attribute_generic[,3:4], item_data_generic, by = c("item" = "item"))
for(i in 1:nrow(item_data_generic))
  item_data_generic$item_desc[i] <- gsub(paste(item_data_generic$brand[i],""),"",item_data_generic$rollup_desc[i])

text_dist <- adist(item_data_generic$item_desc)
rownames(text_dist) <- paste(item_data_generic$item, paste0("$", round(item_data_generic$price, 2)), sep = "--")
text_dist <- as.dist(text_dist)
hclust <- hclust(text_dist)

clusters <- cutree(hclust, 18)
cols <- c(brewer.pal(10,"Paired"),brewer.pal(9,"Set1")[-(1:6)],brewer.pal(8,"Dark2")[-c(2,3,5)],brewer.pal(8,"Accent")[5:7],brewer.pal(8,"Set2"),brewer.pal(10,"Spectral"))
par(mar = c(0,0,0,1))
pdf("Text_CDT_generic.pdf", height = 45, width = 35)
plot(compute.brlen(as.phylo(hclust), power = 0.9), tip.color = cols[clusters], label.offset = 0.05, cex = 1.2)
title(main = "Text based CDT for project_id generic", font.main = 2, cex.main = 3)
dev.off()

item_cluster <- data.frame(item=hclust$labels[hclust$order], cluster=clusters[hclust$order])
write.csv(item_cluster[nrow(item_cluster):1,], "text_item_cluster_assignment_generic.csv", row.names = FALSE)

