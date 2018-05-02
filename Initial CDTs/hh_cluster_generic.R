library(igraph)
library(Matrix)
library(data.table)
library(dplyr)
library(arules)
library(xgboost)
library(caret)
library(DMwR)
setwd("/Users/omahala/Desktop/GM Insights/graph partition/household cluster/Fragrance_generic")
set.seed(700075)

##--------------------reading the data and data prep-----------------------------------##

data <- data.frame(fread("hh_graph_partition_generic.csv"))
colnames(data) <- c("household_id","item_nbr")
hh_count <- data.frame(data %>% group_by(household_id) %>% summarise(nbr=n()))
hh_not1 <- hh_count[which(hh_count$nbr > 1),]$household_id
data <- inner_join(data, as.data.frame(hh_not1), by=c("household_id"="hh_not1"))
data <- arrange(data,item_nbr)[,2:1]
hh_metric <- c(dim(hh_count)[1], length(which(hh_count$nbr==1)), length(hh_not1))
rm(hh_count)
rm(hh_not1)

hh <- sort(unique(data$household_id))
prob <- data.frame(data %>% group_by(household_id) %>% summarise(nbr=n()))$nbr
n <- round(uniqueN(data$household_id)*0.01)
n
sample_hh <- sample(hh, n, replace = F, prob = prob)
data <- inner_join(data, as.data.frame(sample_hh), by = c("household_id" = "sample_hh"))
rm(sample_hh)
rm(prob)

write.csv(unique(data), "more_generic.csv", row.names = FALSE)
rm(data)

##-------------------data prep for graph preparation-----------------------------------##

data_hh <- read.transactions("more_generic.csv", format="single", sep=",", cols=c(1,2))
adjacency.mtx_hh <- crossTable(data_hh)[-(n+1), -(n+1)]
rm(data_hh)
graph_hh <- graph.adjacency(adjacency.mtx_hh, mode="undirected", weighted=TRUE)
rm(adjacency.mtx_hh)
c_louv_hh <- cluster_louvain(graph_hh)
rm(golf_graph_hh)
hh_cluster <- data.frame("household_id"=c_louv_hh$names,"cluster"=c_louv_hh$membership)
hh_cluster$household_id <- as.integer(as.character(hh_cluster$household_id))
hh_cluster %>% group_by(cluster) %>% summarise(nbr=n(),pct=100*n()/n)
c(c_louv_hh$modularity, max(hh_cluster$cluster))

##------------------household variable data reading for classification----------------##

var_cust <- as.data.frame(fread("household_aggregate_generic_other_vars_2.csv"))
colnames(var_cust) <- as.vector(unlist(strsplit(colnames(var_cust),split="_2."))[2*1:dim(var_cust)[2]])

null <- which(var_cust$lifetime_sales==0 | var_cust$avg_item_val=="NULL")
var_cust <- var_cust[-null,]

var_cust[var_cust=="NULL"]<-0

var_cust$avg_item_val <- as.numeric(var_cust$avg_item_val)
var_cust <- inner_join(var_cust,as.data.frame(hh),by=c("household_id"="hh"))
var_cust_cluster <- inner_join(var_cust,hh_cluster,by=c("household_id"="household_id"))
var_cust_cluster <- unique(var_cust_cluster)
var_cust <- unique(var_cust)
var_cust <- anti_join(var_cust,hh_cluster,by=c("household_id","household_id"))

common_vars<-intersect(colnames(var_cust),colnames(var_cust_cluster)) 

for (f in common_vars[-1]) {
  if (class(var_cust_cluster[[f]]) == "character"){
    levels <- unique(c(var_cust_cluster[[f]], var_cust[[f]]))
    var_cust_cluster[[f]] <- as.integer(factor(var_cust_cluster[[f]], levels=levels))
    var_cust[[f]]  <- as.integer(factor(var_cust[[f]],  levels=levels))
  }
}

intrain <- createDataPartition(var_cust_cluster$cluster, p=0.8, list=FALSE)
#var_cust_cluster$cluster <- as.numeric(var_cust_cluster$cluster)
training_set <- var_cust_cluster[intrain,]
val_set <- var_cust_cluster[-intrain,]

dtrain <- xgb.DMatrix(data=data.matrix(training_set[,-c(1,dim(training_set)[2])]), label=training_set[,dim(training_set)[2]]-1)
dval <- xgb.DMatrix(data=data.matrix(val_set[,-c(1,dim(training_set)[2])]), label=val_set[,dim(training_set)[2]]-1)

##------------------ XGBoost for classification----------------------------------##

watchlist<-list(val=dval,train=dtrain)

param <- list(objective           = "multi:softmax",
              num_class           = max(hh_cluster$cluster),
              booster             = "gbtree",
              eta                 = 0.01, 
              max_depth           = 50, 
              subsample           = 1,
              colsample_bytree    = 1
)

clf <- xgb.train(params              = param, 
                 data                = dtrain, 
                 nrounds             = 3000,
                 verbose             = 1,
                 early.stop.round    = 50,
                 watchlist           = watchlist,
                 eval_metric         = "merror"
                 
)

m <- as.matrix(var_cust[,-1])
var_cust$cluster <- predict(clf,m) +1
all_hh <- rbind(var_cust,var_cust_cluster)

##----------------------------profile----------------------------------##

cluster_profile <- data.frame(t(all_hh[,-1] %>% group_by(cluster) %>% summarise_each(funs(mean)))[-1,])
write.csv(cluster_profile,"hh_cluster_profile_generic.csv")
write.csv(all_hh,"hh_cluster_generic.csv")



