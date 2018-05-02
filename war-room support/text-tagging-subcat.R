library(data.table)
library(plyr)
library(dplyr)
library(ape)
library(dendextend)
library(RColorBrewer)
library(bit64)
library(wordcloud)

setwd("/Users/omahala/Desktop/GM Insights/graph partition/item cluster/GOLF_723")

item_subcat <- data.frame(fread("item_subcat_723.csv"))
assort_desc <- data.frame(fread("/Users/omahala/Desktop/GM Insights/EDA data/Data from Ryan/Assortment DescriptionGolf_Project1765.csv"))
item_subcat$V1 <- as.numeric(item_subcat$V1)
item_subcat <- inner_join(item_subcat, assort_desc, by = c("V1"="Rollup_id"))
item_subcat$Leaf_labels <- as.character(item_subcat$Leaf_labels)
item_subcat$Rollup_Description <- as.character(item_subcat$Rollup_Description)
item_subcat_list <- split(item_subcat, item_subcat$V4)
lapply(item_subcat_list, function(x) wordcloud(x$V2))
lapply(item_subcat_list, function(x) wordcloud(x$Rollup_Description))
attribute <- data.frame(fread("/Users/omahala/golf_brand.csv"))
attribute <- data.frame(fread("attribute_723.csv"))

## only for 4796
docs <- Corpus(VectorSource(item_subcat_list$`4796`$Rollup_Description))
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

docs <- tm_map(docs, content_transformer(toupper))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 10,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

slct_wds <- as.character(d$word[d$freq > summary(d$freq[d$freq>3])[5]])
slct_wds <- slct_wds[!(slct_wds %in% attribute$V4)]
for(x in slct_wds){
  d$word[d$word %in% substring(x, 1, 2:nchar(x))] <- x
}
d <- d %>% group_by(word) %>% summarise("freq"=sum(freq))
d <- arrange(d, desc(freq))
slct_wds <- as.character(d$word[d$freq > summary(d$freq[d$freq>3])[3]])
slct_wds <- toupper(slct_wds)
slct_wds <- slct_wds[!(slct_wds %in% attribute$V4)]
slct_wds <- slct_wds[!(slct_wds %in% rmv_wrds)]


slct_wds <- toupper(slct_wds)

rollup_wds <- strsplit(item_subcat_list$`4796`$Rollup_Description, split = " ")
rollup_tags <- lapply(rollup_wds, function(x) x[x %in% slct_wds])
item_subcat_list$`4796`$tag <- NULL
for(i in 1:nrow(item_subcat_list$`4796`))
  item_subcat_list$`4796`$tag[i] <- paste(rollup_tags[[i]], collapse = " ")
# for(i in 1:nrow(item_subcat_list$`4796`)){
#   if(item_subcat_list$`4796`$tag[i]=="")
#     item_subcat_list$`4796`$tag[i]  <- item_subcat_list$`4796`$Rollup_Description[i]
# }
item_subcat_list$`4796`$tag[which(item_subcat_list$`4796`$tag == "")] <- "MISCELLANEOUS"

item_subcat_list$`4796`$tag[which(item_subcat_list$`4796`$tag %in% toupper(nu_wds_plural))]
item_subcat_list$`4796`$tag[which(item_subcat_list$`4796`$tag == "BALLS")] <- "BALL"
item_subcat_list$`4796`$tag[which(item_subcat_list$`4796`$tag == "TEES")] <- "TEE"
item_subcat_list$`4796`$tag[which(item_subcat_list$`4796`$tag == "GLOVES")] <- "GLOVE"
item_subcat_list$`4796`$tag[which(item_subcat_list$`4796`$tag == "TOWELS")] <- "TOWEL"
item_subcat_list$`4796`$tag[which(item_subcat_list$`4796`$tag == "HEADCOVERS")] <- "HEADCOVER"

#tagged_item_subcat_list <- split(item_subcat_list$`4796`, item_subcat_list$`4796`$tag)
cluster <- array(0,nrow(item_subcat_list$`4796`))
al_tag <- unique(item_subcat_list$`4796`$tag)
for(i in 1:length(al_tag))
  cluster[which(item_subcat_list$`4796`$tag == al_tag[i])] <- i


text_dist <- adist(item_subcat_list$`4796`$tag)
rownames(text_dist) <- item_subcat_list$`4796`$Leaf_labels
hclust <- hclust(as.dist(text_dist))

clusters <- cutree(hclust, 15)

cols <- c(brewer.pal(10,"Paired"),brewer.pal(9,"Set1")[-(1:6)],brewer.pal(8,"Dark2")[-c(2,3,5)],brewer.pal(8,"Accent")[5:7],brewer.pal(8,"Set2"),brewer.pal(10,"Spectral"))
par(mar = c(0,0,0,1))
pdf("Text_CDT_generic.pdf", height = 45, width = 35)
plot(compute.brlen(as.phylo(hclust), power = 0.9), tip.color = cols[clusters], label.offset = 0.005, cex = 0.5)
title(main = "Text based CDT for project_id generic", font.main = 2, cex.main = 3)
dev.off()

item_cluster <- data.frame(item=hclust$labels[hclust$order], cluster=clusters[hclust$order])
item_cluster <- inner_join(item_cluster, item_subcat_list$`4796`, by = c("item" = "Leaf_labels"))


#only for 4797
docs <- Corpus(VectorSource(item_subcat_list$`4797`$Rollup_Description))
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

docs <- tm_map(docs, content_transformer(toupper))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 10,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

slct_wds <- as.character(d$word[d$freq > summary(d$freq[d$freq>3])[5]])
slct_wds <- slct_wds[!(slct_wds %in% attribute$V4)]
for(x in slct_wds){
  d$word[d$word %in% substring(x, 1, 2:nchar(x))] <- x
}
d <- d %>% group_by(word) %>% summarise("freq"=sum(freq))
d <- arrange(d, desc(freq))
slct_wds <- as.character(d$word[d$freq > summary(d$freq[d$freq>3])[3]])
slct_wds <- toupper(slct_wds)
slct_wds <- slct_wds[!(slct_wds %in% attribute$V4)]
slct_wds <- slct_wds[!(slct_wds %in% rmv_wrds)]


slct_wds <- toupper(slct_wds)

rollup_wds <- strsplit(item_subcat_list$`4797`$Rollup_Description, split = " ")
rollup_tags <- lapply(rollup_wds, function(x) x[x %in% slct_wds])
item_subcat_list$`4797`$tags <- NULL
for(i in 1:nrow(item_subcat_list$`4797`))
  item_subcat_list$`4797`$tags[i] <- paste(rollup_tags[[i]], collapse = " ")
# for(i in 1:nrow(item_subcat_list$`4797`)){
#   if(item_subcat_list$`4797`$tag[i]=="")
#     item_subcat_list$`4797`$tag[i]  <- item_subcat_list$`4797`$Rollup_Description[i]
# }
item_subcat_list$`4797`$tags[which(item_subcat_list$`4797`$tags == "")] <- "MISCELLANEOUS"

#tagged_item_subcat_list <- split(item_subcat_list$`4797`, item_subcat_list$`4797`$tag)
cluster <- array(0,nrow(item_subcat_list$`4797`))
View(item_subcat_list$`4797` %>% group_by(tag) %>% summarise(n()))
item_subcat_list$`4797`$tag[which(item_subcat_list$`4797`$tag == "BALLS")] <- "BALL"
item_subcat_list$`4797`$tag[which(item_subcat_list$`4797`$tag %in% c("BALLS BALL PACK","PACK","PACK BALL","PACK BALLS"))] <- "BALL PACK"
item_subcat_list$`4797`$tag[which(item_subcat_list$`4797`$tag %in% c("LONG","LONG BALL","LONG BALLS", "LONG SOFT BALL"))] <- "LONG BALL"
item_subcat_list$`4797`$tag[which(item_subcat_list$`4797`$tag %in% c("LONG SOFT","LONG SOFT", "LONG SOFT BALL PACK", "PACK LONG SOFT BALL"))] <- "LONG SOFT BALL"
item_subcat_list$`4797`$tag[which(item_subcat_list$`4797`$tag %in% c("NITRO TOUR BALLS","NITRO TOUR","NITRO"))] <- "NITRO"
item_subcat_list$`4797`$tag[which(item_subcat_list$`4797`$tag %in% c("SOFT","SOFT BALL","SOFT BALLS"))] <- "SOFT BALL"
item_subcat_list$`4797`$tag[which(item_subcat_list$`4797`$tag %in% c("STRAIGHT", "STRAIGHT BALL", "STRAIGHT BALLS"))] <- "STRAIGHT BALL"
item_subcat_list$`4797`$tag[which(item_subcat_list$`4797`$tag %in% c("TOP FLITE","TOP FLITE BALLS","TOP FLITE LONG BALLS","TOP FLITE SOFT BALLS","TOP FLITE STRAIGHT","TOP FLITE STRAIGHT BALLS"))] <- "TOP FLITE STRAIGHT BALL"
item_subcat_list$`4797`$tag[which(item_subcat_list$`4797`$tag %in% c("TOUR","TOUR BALLS","TOUR STRAIGHT"))] <- "TOUR BALL"




al_tag <- unique(item_subcat_list$`4797`$tag)

for(i in 1:length(al_tag))
  cluster[which(item_subcat_list$`4797`$tag == al_tag[i])] <- i


text_dist <- adist(item_subcat_list$`4797`$tag)
rownames(text_dist) <- item_subcat_list$`4797`$Leaf_labels
hclust <- hclust(as.dist(text_dist))

clusters <- cutree(hclust, 8)

cols <- c(brewer.pal(10,"Paired"),brewer.pal(9,"Set1")[-(1:6)],brewer.pal(8,"Dark2")[-c(2,3,5)],brewer.pal(8,"Accent")[5:7],brewer.pal(8,"Set2"),brewer.pal(10,"Spectral"))
par(mar = c(0,0,0,1))
pdf("Text_CDT_generic2.pdf", height = 45, width = 35)
plot(compute.brlen(as.phylo(hclust), power = 0.9), tip.color = cols[clusters], label.offset = 0.005, cex = 0.5)
title(main = "Text based CDT for project_id generic", font.main = 2, cex.main = 3)
dev.off()

item_cluster_4797 <- data.frame(item=hclust$labels[hclust$order], cluster_tag=clusters[hclust$order])
item_cluster_4797 <- inner_join(item_cluster_4797, item_subcat_list$`4797`, by = c("item" = "Leaf_labels"))


#only for 4798
docs <- Corpus(VectorSource(item_subcat_list$`4798`$Rollup_Description))
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

docs <- tm_map(docs, content_transformer(toupper))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 10,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

slct_wds <- as.character(d$word[d$freq > summary(d$freq[d$freq>3])[5]])
slct_wds <- slct_wds[!(slct_wds %in% attribute$V4)]
for(x in slct_wds){
  d$word[d$word %in% substring(x, 1, 2:nchar(x))] <- x
}
d <- d %>% group_by(word) %>% summarise("freq"=sum(freq))
d <- arrange(d, desc(freq))
slct_wds <- as.character(d$word[d$freq > summary(d$freq[d$freq>3])[3]])
slct_wds <- toupper(slct_wds)
slct_wds <- slct_wds[!(slct_wds %in% attribute$V4)]
#define rmv_wrd first
slct_wds <- slct_wds[!(slct_wds %in% rmv_wrds)]


slct_wds <- toupper(slct_wds)

rollup_wds <- strsplit(item_subcat_list$`4798`$Rollup_Description, split = " ")
rollup_tags <- lapply(rollup_wds, function(x) x[x %in% slct_wds])
item_subcat_list$`4798`$tags <- NULL
for(i in 1:nrow(item_subcat_list$`4798`))
  item_subcat_list$`4798`$tags[i] <- paste(rollup_tags[[i]], collapse = " ")
# for(i in 1:nrow(item_subcat_list$`4798`)){
#   if(item_subcat_list$`4798`$tag[i]=="")
#     item_subcat_list$`4798`$tag[i]  <- item_subcat_list$`4798`$Rollup_Description[i]
# }
item_subcat_list$`4798`$tags[which(item_subcat_list$`4798`$tags == "")] <- "MISCELLANEOUS"

#tagged_item_subcat_list <- split(item_subcat_list$`4798`, item_subcat_list$`4798`$tag)
cluster <- array(0,nrow(item_subcat_list$`4798`))
View(item_subcat_list$`4798` %>% group_by(tag) %>% summarise(n()))
item_subcat_list$`4798`$tag[which(item_subcat_list$`4798`$tag == "BALLS")] <- "BALL"
item_subcat_list$`4798`$tag[which(item_subcat_list$`4798`$tag %in% c("BALLS BALL PACK","PACK","PACK BALL","PACK BALLS"))] <- "BALL PACK"
item_subcat_list$`4798`$tag[which(item_subcat_list$`4798`$tag %in% c("LONG","LONG BALL","LONG BALLS", "LONG SOFT BALL"))] <- "LONG BALL"
item_subcat_list$`4798`$tag[which(item_subcat_list$`4798`$tag %in% c("LONG SOFT","LONG SOFT", "LONG SOFT BALL PACK", "PACK LONG SOFT BALL"))] <- "LONG SOFT BALL"
item_subcat_list$`4798`$tag[which(item_subcat_list$`4798`$tag %in% c("NITRO TOUR BALLS","NITRO TOUR","NITRO"))] <- "NITRO"
item_subcat_list$`4798`$tag[which(item_subcat_list$`4798`$tag %in% c("SOFT","SOFT BALL","SOFT BALLS"))] <- "SOFT BALL"
item_subcat_list$`4798`$tag[which(item_subcat_list$`4798`$tag %in% c("STRAIGHT", "STRAIGHT BALL", "STRAIGHT BALLS"))] <- "STRAIGHT BALL"
item_subcat_list$`4798`$tag[which(item_subcat_list$`4798`$tag %in% c("TOP FLITE","TOP FLITE BALLS","TOP FLITE LONG BALLS","TOP FLITE SOFT BALLS","TOP FLITE STRAIGHT","TOP FLITE STRAIGHT BALLS"))] <- "TOP FLITE STRAIGHT BALL"
item_subcat_list$`4798`$tag[which(item_subcat_list$`4798`$tag %in% c("TOUR","TOUR BALLS","TOUR STRAIGHT"))] <- "TOUR BALL"




al_tag <- unique(item_subcat_list$`4798`$tag)

for(i in 1:length(al_tag))
  cluster[which(item_subcat_list$`4798`$tag == al_tag[i])] <- i


text_dist <- adist(item_subcat_list$`4798`$tag)
rownames(text_dist) <- item_subcat_list$`4798`$Leaf_labels
hclust <- hclust(as.dist(text_dist))

clusters <- cutree(hclust, 2)

cols <- c(brewer.pal(10,"Paired"),brewer.pal(9,"Set1")[-(1:6)],brewer.pal(8,"Dark2")[-c(2,3,5)],brewer.pal(8,"Accent")[5:7],brewer.pal(8,"Set2"),brewer.pal(10,"Spectral"))
par(mar = c(0,0,0,1))
pdf("Text_CDT_generic3.pdf", height = 45, width = 35)
plot(compute.brlen(as.phylo(hclust), power = 0.9), tip.color = cols[clusters], label.offset = 0.005, cex = 0.5)
title(main = "Text based CDT for project_id generic", font.main = 2, cex.main = 3)
dev.off()

item_cluster_4798 <- data.frame(item=hclust$labels[hclust$order], cluster_tag=clusters[hclust$order])
item_cluster_4798 <- inner_join(item_cluster_4798, item_subcat_list$`4798`, by = c("item" = "Leaf_labels"))


text_dist <- adist(all_item_cluster$tags)
rownames(text_dist) <- all_item_cluster$item
hclust <- hclust(as.dist(text_dist))

clusters <- cutree(hclust, 23)

cols <- c(brewer.pal(10,"Paired"),brewer.pal(9,"Set1")[-(1:6)],brewer.pal(8,"Dark2")[-c(2,3,5)],brewer.pal(8,"Accent")[5:7],brewer.pal(8,"Set2"),brewer.pal(10,"Spectral"))
par(mar = c(0,0,0,1))
pdf("Text_CDT_generic_all.pdf", height = 200, width = 150)
plot(compute.brlen(as.phylo(hclust), power = 0.9), tip.color = cols[clusters], label.offset = 0.001, cex = 1)
title(main = "Text based CDT for project_id 723", font.main = 2, cex.main = 3)
dev.off()






