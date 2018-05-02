library(igraph)
library(Matrix)
library(data.table)
library(bit64)
library(plyr)
library(dplyr)
library(arules)
library(RColorBrewer)
library(dendextend)
library(ape)
library(cluster)
library(NbClust)
library(fpc)
library(rpart)
library(caret)

setwd("/Users/omahala/Desktop/GM Insights/graph partition/item cluster")

## reading the hh-item data

data_generic <- unique(as.data.frame(fread("item_hh_generic.csv")))
colnames(data_generic) <- c("household_id","rollup_id","rollup_desc")
select_data <- data.frame(fread("select_item_generic.csv"))
data_generic$rollup_id <- as.numeric(data_generic$rollup_id)
data_generic <- inner_join(data_generic, data.frame(a=as.numeric(select_data[,2])), by = c("rollup_id" = "a"))
hh_count <- data.frame(data_generic %>% group_by(household_id) %>% summarise(nbr=n()))
hh_count_max <- hh_count$nbr[which.max(hh_count$nbr)]
hh_not1 <- hh_count[which(hh_count$nbr > 1),]$household_id
data_generic <- inner_join(data_generic, as.data.frame(hh_not1), by=c("household_id"="hh_not1"))
data_generic <- arrange(data_generic, household_id)
data_generic$item <- paste(data_generic$rollup_id, data_generic$rollup_desc, sep = "--")
item <- uniqueN(data_generic$rollup_id)
rm(hh_count); rm(hh_not1)


## reading the attribute data

attribute_generic <- data.frame(fread("attribute_generic.csv"))
colnames(attribute_generic) <- c("rollup_id", "rollup_desc", "fineline_nbr", "brand", "price", "sales")
attribute_generic$rollup_id <- as.numeric(attribute_generic$rollup_id)

attribute_generic <- inner_join(attribute_generic, data.frame(a=as.numeric(select_data[,2])), by=c("rollup_id"="a"))
attribute_generic <- inner_join(attribute_generic, data.frame(a=unique(data_generic$rollup_id)), by=c("rollup_id"="a"))
attribute_generic$check <- 1
item_cnt <- data.frame(attribute_generic %>% group_by(rollup_id) %>% summarise(nbr=n()))
item_cnt <- item_cnt[which(item_cnt$nbr > 1),]
#brand_cnt <- data.frame(attribute_generic %>% group_by(brand) %>% summarise(nbr=n()))
for(i in 1:nrow(attribute_generic)){
  if(attribute_generic$rollup_id[i] %in% item_cnt$rollup_id &
     !(attribute_generic$brand[i] %in% as.vector(unlist(strsplit(attribute_generic$rollup_desc[i], split=" ")))))
    attribute_generic$check[i] <- 0
}

setDT(attribute_generic)[order(desc(sales)), max := c("max", rep(0, .N - 1)), by = rollup_id]

item_cnt <- data.frame(attribute_generic[which(attribute_generic$check==0),] %>% group_by(rollup_id) %>% summarise(nbr=n()))
item_cnt <- item_cnt[which(item_cnt$nbr>1),]
for(i in 1:nrow(attribute_generic)){
  if(attribute_generic$rollup_id[i] %in% item_cnt$rollup_id & attribute_generic$check[i]==0 & attribute_generic$max[i]=="max")
    attribute_generic$check[i] <- 1
}
attribute_generic <- as.data.frame(attribute_generic[which(attribute_generic$check==1),])
attribute_generic <- attribute_generic[,colnames(attribute_generic)[1:6]]

attribute_generic$price_cat <- "high"
attribute_generic <- split(attribute_generic, attribute_generic$fineline_nbr)
price_cat <- function(x){
  q = as.vector(quantile(x$price, 0.5))
  for(i in 1:nrow(x))
    if(x$price[i]<=q) x$price_cat[i] <- "low"
  x
}
attribute_generic <- lapply(attribute_generic, function(x) price_cat(x))
attribute_generic <- ldply(attribute_generic, data.frame)
attribute_generic <- attribute_generic[,-1]
attribute_generic$item <- paste(attribute_generic$rollup_id, attribute_generic$rollup_desc, sep="--")
rm(item_cnt); #rm(brand_cnt)


## CART preparation:

attribute_generic$product <- paste(attribute_generic$fineline_nbr, attribute_generic$price_cat, sep="--")
q <- quantile(attribute_generic$sales, 0.75)
for(i in 1: nrow(attribute_generic))
  attribute_generic$high_sales[i] <- ifelse(attribute_generic$sales[i] < q, 0, 1)
set.seed(112356)
train <- sample(1:nrow(attribute_generic),round(0.7*nrow(attribute_generic)),FALSE)
train_sample <- attribute_generic[train,]
test_sample <- attribute_generic[-train,]
cart_fit <- rpart(high_sales~., method="class", data=train_sample[,which(names(train_sample)
                                                                         %in% c("high_sales","brand","product"))])
importance <- varImp(cart_fit)
importance


## graph preparation:

data_generic_trans <- as(split(data_generic[,4], data_generic[,1]), "transactions")
adjacency.mtx <- crossTable(data_generic_trans)[-(item+1), -(item+1)]
rm(data_generic_trans)
diag(adjacency.mtx) <- 0
rm(data_generic)
item_graph_generic <- graph.adjacency(adjacency.mtx, mode="undirected", weighted=TRUE)
structured_vertex <- get.data.frame(item_graph_generic, "vertices")
rownames(structured_vertex) <- NULL
structured_vertex <- inner_join(structured_vertex, attribute_generic[,c(4,8,9)], by = c("name"="item"))
item_graph_generic <- add_vertices(item_graph_generic, sum(uniqueN(structured_vertex$brand), uniqueN(structured_vertex$product)),
                               name=c(unique(structured_vertex$brand), unique(structured_vertex$product)))

item_graph_generic <- add_edges(item_graph_generic, as.vector(unlist(strsplit(paste(structured_vertex[,1], structured_vertex[,2], sep="---"), split="---"))))
item_graph_generic <- add_edges(item_graph_generic, as.vector(unlist(strsplit(paste(structured_vertex[,1], structured_vertex[,3], sep="---"), split="---"))))
all_edges <- get.data.frame(item_graph_generic, "edges")
all_edges$edge <- paste(all_edges$from, all_edges$to, sep="---")
struc_edge_count <- data.frame("nbr"=apply(adjacency.mtx, 2, sum))
struc_edge_count$item <- rownames(struc_edge_count)
rownames(struc_edge_count) <- NULL
attr_edge_count <- data.frame(tail(all_edges, item*2) %>% group_by(to) %>% summarise(nbr=n()))


## transition matrix

P <- matrix(0, nrow= length(V(item_graph_generic)), ncol=length(V(item_graph_generic)))
colnames(P) <- V(item_graph_generic)$name
rownames(P) <- V(item_graph_generic)$name
struc_edge_count <- inner_join(data.frame("item"=colnames(P)), struc_edge_count, by=c("item"="item"))
attr_edge_count <- inner_join(data.frame("item"=colnames(P)), attr_edge_count, by=c("item"="to"))

for(i in 1:item){
  for(j in 1:item)
    P[i,j] <- adjacency.mtx[i,j]/(struc_edge_count$nbr[i]+sum(importance$Overall))
}

for(i in 1:item){
  for(j in (item+1):ncol(P)){
    if(colnames(P)[j] %in% unique(structured_vertex$brand))
      P[i,j] <- ifelse(paste(colnames(P)[i],colnames(P)[j],sep="---") %in% all_edges$edge, importance$Overall[1]/(struc_edge_count$nbr[i]+sum(importance$Overall)), 0)
    if(colnames(P)[j] %in% unique(structured_vertex$product))
      P[i,j] <- ifelse(paste(colnames(P)[i],colnames(P)[j],sep="---") %in% all_edges$edge, importance$Overall[2]/(struc_edge_count$nbr[i]+sum(importance$Overall)), 0)
  }
}

for(i in (item+1):ncol(P)){
  for(j in 1:item)
    P[i,j] <- ifelse(paste(colnames(P)[j],colnames(P)[i],sep="---") %in% all_edges$edge, 1/(attr_edge_count$nbr[i-item]), 0)
}

pow <- function(A,k){
  X = A
  for(i in 1:k-1)
    X = X %*% A
  return(X)
}

R = (0.5)*(0.5)*P
for(i in 2:5)
  R = R + (0.5)*((0.5)^i)*pow(P,i)


## clustering

R_item <- R#[1:item, 1:item]
par(mar=c(2,3,1,1))
wss <- (nrow(R_item)-1)*sum(apply(R_item,2,var))
for (i in 2:50) wss[i] <- sum(kmeans(R_item, centers=i)$withinss)
plot(1:50, wss, type="b")

R_item <- R[1:item, 1:item]
R_dist <- as.dist(R_item)
fit <- hclust(R_dist, "ward.D2")
fit <- as.dendrogram(fit)
clusters <- as.vector(cutree(fit, 16))
cols <- c(brewer.pal(10,"Paired"),brewer.pal(9,"Set1")[-(1:6)],brewer.pal(8,"Dark2")[-c(2,3,5)],brewer.pal(8,"Accent")[5:7],brewer.pal(8,"Set2"), brewer.pal(10,"Spectral"))

dend <- branches_attr_by_clusters(fit, clusters[order.dendrogram(fit)], values=cols) %>% color_labels(col=cols[clusters])

par(mar=c(0,0,0,1))
pdf("dendrogram_generic_attribute.pdf", height = 65, width = 45)
plot(compute.brlen(as.phylo(flatten.dendrogram(dend)),power=0.9),tip.color = cols[clusters],label.offset = 0.02, cex=1.2)
#plot(dend, horiz = TRUE)
title(main = "Multi Attribute based Graph Partitioned CDT for project-id generic", font.main=2, cex.main=5)
dev.off()

item_cluster <- data.frame("item"=colnames(P)[order.dendrogram(dend)], "cluster"=clusters[order.dendrogram(dend)])
item_cluster <- inner_join(item_cluster, structured_vertex, by=c("item"="name"))
write.csv(item_cluster[nrow(item_cluster):1,], "attributes_item_cluster_assignment_generic.csv", row.names = FALSE)



