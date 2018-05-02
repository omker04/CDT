library(data.table)
library(plyr)
library(dplyr)
library(ape)
library(dendextend)
library(RColorBrewer)
library(bit64)
library(wordcloud)
library(SnowballC)
library(tm)

assort_desc <- data.frame(fread("/Users/omahala/Desktop/GM Insights/EDA data/Data from Ryan/Assortment DescriptionGolf_Project1765.csv"))
sub_data <- data.frame(fread("/Users/omahala/Desktop/GM Insights/EDA data/Data from Ryan/cleanSubData.csv"))
item_metric <- data.frame(fread("/Users/omahala/Desktop/GM Insights/EDA data/Data from Ryan/item_metrics_file_GOLF_051016.csv"))
attribute <- data.frame(fread("/Users/omahala/Desktop/GM Insights/graph partition/item cluster/GOLF_723/attribute_723.csv"))

selected_upc <- item_metric[which(item_metric$stores.top.90 == 1),]$a.upc
selected_upc <- inner_join(data.frame("Rollup_id"=selected_upc), assort_desc, by = c("Rollup_id"="Rollup_id"))
selected_upc$Category <- paste("Subcat", selected_upc$SubCategory, sep = "--")

docs <- Corpus(VectorSource(selected_upc$Rollup_Description))
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "-")
docs <- tm_map(docs, content_transformer(toupper))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)

wordcloud(d$word, d$freq, min.freq = 5, random.order = FALSE, colors = brewer.pal(8, "Dark2"))
slct_wds <- c("golf","glove","cap","umbrella","bag","towel","hat", "ball")
slct_wds2 <- paste0(slct_wds, "s")

all_wds <- toupper(c(slct_wds, slct_wds2))

rollup_wds <- strsplit(toupper(selected_upc$Rollup_Description), split = " ")
rollup_tags <- lapply(rollup_wds, function(x) x[x %in% all_wds])
selected_upc$tag <- NULL
for(i in 1:nrow(selected_upc)){
  selected_upc$tag[i] <- paste(rollup_tags[[i]], collapse = " ")
  selected_upc$tag[i] <- gsub("GOLF ","",selected_upc$tag[i])
}
selected_upc$tag[which(selected_upc$tag == "")] <- "MISCELLANEOUS"
tag_dist <- selected_upc %>% group_by(tag) %>% summarise(n())

# balls <- selected_upc[which(selected_upc$tag=="BALLS"),]
# text_dist_balls <- adist(toupper(balls$Rollup_Description))
# rownames(text_dist_balls) <- balls$Leaf_labels
# hclust_balls <- hclust(as.dist(text_dist_balls))
# cluster_balls <- cutree(hclust_balls, 3)
# plot(compute.brlen(as.phylo(hclust_balls), power = 0.9), tip.color = cols[cluster_balls], label.offset = 0.005, cex = 0.5)
# balls_hclust <- data.frame("label"=hclust_balls$labels, "order"=hclust_balls$order, "cluster"=cluster_balls)
# balls <- inner_join(balls, balls_hclust, by = c("Leaf_labels"="label"))
# balls$cluster <- balls$cluster + 20
# 
# misc <- selected_upc[which(selected_upc$tag=="MISCELLANEOUS"),]
# text_dist_misc <- adist(toupper(misc$Rollup_Description))
# rownames(text_dist_misc) <- misc$Leaf_labels
# hclust_misc <- hclust(as.dist(text_dist_misc))
# cluster_misc <- cutree(hclust_misc, 6)
# plot(compute.brlen(as.phylo(hclust_misc), power = 0.9), tip.color = cols[cluster_misc], label.offset = 0.005, cex = 0.5)
# misc_hclust <- data.frame("label"=hclust_misc$labels, "order"=hclust_misc$order, "cluster"=cluster_misc)
# misc <- inner_join(misc, misc_hclust, by = c("Leaf_labels"="label"))
# misc$cluster <- misc$cluster + 10


# text_dist <- adist(selected_upc$tag[!(selected_upc$tag %in% c("MISCELLANEOUS","BALLS"))])
# rownames(text_dist) <- selected_upc$Leaf_labels[!(selected_upc$tag %in% c("MISCELLANEOUS","BALLS"))]
# hclust <- hclust(as.dist(text_dist))
# clusters <- cutree(hclust, 8)
# item_cluster <- data.frame("label"=hclust$labels, "order"=hclust$order, "cluster"=clusters)
# selected_upc <- inner_join(selected_upc, item_cluster, by = c("Leaf_labels" = "label"))
# #selected_upc <- left_join(selected_upc, others[,c("Leaf_labels","cluster")],  by=c("Leaf_labels"="Leaf_labels"))
# 
# misc$order <- misc$order + max(selected_upc$order)
# balls$order <- balls$order + max(misc$order)
# 
# dend <- merge(as.dendrogram(hclust), as.dendrogram(hclust_misc), as.dendrogram(hclust_balls))



text_dist <- adist(selected_upc$tag)
rownames(text_dist) <- selected_upc$Leaf_labels
hclust <- hclust(as.dist(text_dist))
clusters <- cutree(hclust, 11)
cols <- c(brewer.pal(10,"Paired"),brewer.pal(9,"Set1")[-(1:6)],brewer.pal(8,"Dark2")[-c(2,3,5)],brewer.pal(8,"Accent")[5:7],brewer.pal(8,"Set2"),brewer.pal(10,"Spectral"))
#dend <- branches_attr_by_clusters(dend, clusters[order.dendrogram(dend)], values=cols) %>% color_labels(col=cols[clusters])
par(mar = c(0,0,0,1))
pdf("Text_CDT_selected.pdf", height = 45, width = 35)
plot(compute.brlen(as.phylo(hclust), power = 0.9), tip.color = cols[clusters], label.offset = 0.005, cex = 0.5)
title(main = "Text based CDT for project_id generic", font.main = 2, cex.main = 3)
dev.off()



