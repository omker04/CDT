    
    require(igraph) || install.packages("igraph")    #for creating graph and graph partitioning
    require(Matrix) || install.packages("Matrix")    #for usual matrix operations
    require(data.table) || install.packages("data.table")    #for reading huge datasets really fast and other important functions
    require(bit64) || install.packages("bit64")    #for allowing rollup_id as integer64
    require(dplyr) || install.packages("dplyr")    #for data-prep
    require(arules) || install.packages("arules")    #for creating the adjacency matrix
    require(RColorBrewer) || install.packages("RColorBrewer")    #for colored dendrogram leaf
    require(dendextend) || install.packages("dendextend")    #for dendrogram operations
    require(ape) || install.packages("ape")    #for fitting the dendrogram into the required page size
    require(wordcloud) || install.packages("wordcloud")   #for drawing the wordclouds
    
    
    setwd("/Users/omahala/Desktop/GM Insights/graph partition/item cluster/AUTO_generic")
    
    
    
    ########################################################################################################################################
    ####################################          reading the data and required data prep       ############################################
    ########################################################################################################################################
    
    
    
    data_generic <- unique(as.data.frame(fread("item_hh_generic.csv")))
    colnames(data_generic) <- c("household_id","rollup_id","rollup_desc")
    
    #converting rollup_id from integer_64 to numeric
    data_generic$rollup_id <- as.numeric(data_generic$rollup_id) 
    
    #reading the data having the selected list of items (sent by BTV)
    select_data <- data.frame(fread("select_item_637.csv"))
    
    #inner joining with data_generic to have info only abt the selected items
    data_generic <- inner_join(data_generic, data.frame(a=as.numeric(select_data[,2])), by = c("rollup_id" = "a"))
    
    #counting the no. of unique products purchased by a hh, and ignoring those who bought only 1 type of product
    hh_count <- data.frame(data_generic %>% group_by(household_id) %>% summarise(nbr=n()))
    hh_not1 <- hh_count[which(hh_count$nbr > 1),]$household_id
    data_generic <- inner_join(data_generic, as.data.frame(hh_not1), by=c("household_id"="hh_not1"))
    
    #arranging the data as per household_id
    data_generic <- arrange(data_generic, household_id)
    
    #no. of unique items under consideration
    item <- uniqueN(data_generic$rollup_id) #can be replaced by length(unique()) if uniqueN() doesnt work
    rm(hh_count); rm(hh_not1)
    
    
    
    ###################################################################################################################################################
    ####################################          creating graph and running graph partitioning algo       ############################################
    ###################################################################################################################################################
    
    
    
    #read as transaction data for item cluster and run graph based partitioning----------------##
    data_generic_trans <- as(split(data_generic[,2], data_generic[,1]), "transactions")
    
    #calculating the adjacency matrix from transaction data
    adjacency.mtx <- crossTable(data_generic_trans)[-(item+1), -(item+1)]
    rm(data_generic_trans)
    
    #creating a graph from the adjacency matrix
    item_graph_generic <- graph.adjacency(adjacency.mtx, mode="undirected", weighted=TRUE)
    rm(adjacency.mtx)
    
    #running fast greedy algorithm for graph partitioning
    c_fast_greedy <- cluster_fast_greedy(item_graph_generic)
    
    #creating the item-cluster assignment as obtained from fast greedy partitioning
    item_cluster <- data.frame("rollup_id" = c_fast_greedy$names, "clus_fast_greedy" = c_fast_greedy$membership)
    
    #item distribution over different clusters
    fast_greedy_distn <- data.frame(item_cluster %>% group_by(clus_fast_greedy) %>% summarise(nbr=n()))
    
    
    
    #####################################################################################################################################
    ####################################          creating and coloring the dendrogram       ############################################
    #####################################################################################################################################
    
    
    
    #creating the leaf names of the form `rollup_id--rollup_desc--price`
    rollup_id_desc <- inner_join(data.frame(a = as.numeric(c_fast_greedy$names)), unique(data_generic[,-1]), by = c("a" = "rollup_id"))
    c_fast_greedy$names <- paste(rollup_id_desc$a, rollup_id_desc$rollup_desc, sep = "--")
    item_price_generic <- data.frame(fread("item_price_generic.csv"))
    item_price_generic$item <- paste(item_price_generic[,1],item_price_generic[,2], sep="--")
    item_price_generic$price <- paste("$",round(item_price_generic[,3],2), sep = "")

    #joining as per order of the items in the clusters
    item_price_generic <- inner_join(data.frame(a=c_fast_greedy$names), item_price_generic[,4:5], by=c("a"="item"))
    c_fast_greedy$names <- paste(item_price_generic[,1], item_price_generic[,2], sep="--")
    
    #obtaining a dendrogram as per the hierarchial fast greedy algo
    hc <- as.dendrogram(c_fast_greedy)
    
    #cutting the dendrogram obtained as per required no. of clusters
        #default: optimal no. of clusters as obtained
        #can change it as per requirement; replace max(c_fast_greedy$membership) by `no.of clusters`
    clusters <- as.vector(cutree(hc, max(c_fast_greedy$membership))) 
    
    #defining a set of colors for the colored branches of the dendrogram
    cols <- c(brewer.pal(10,"Paired"),brewer.pal(9,"Set1")[-(1:6)],brewer.pal(8,"Dark2")[-c(2,3,5)],brewer.pal(8,"Accent")[5:7],brewer.pal(8,"Set2"),brewer.pal(8,"Accent")[1:3])
    
    #create the final ordered dendrogram with different colors for each clusters
    dend <- branches_attr_by_clusters(hc, clusters[order.dendrogram(hc)], values=cols) %>% color_labels(col=cols[clusters])
    
    #print the dendrogram as a pdf
    par(mar=c(0,0,0,1))
      #change the height and width of the pdf as per item size
    pdf("dendrogram_select_generic.pdf", height = 45, width = 35)
      #change cex value to alter the leaf font size 
    plot(as.phylo(flatten.dendrogram(dend)),tip.color = cols[clusters], label.offset = 2, cex = 2)
    title(main = "Graph Partition based CDT for project-id generic", font.main=2, cex.main=5)
    dev.off()
    
    
    
    #####################################################################################################################################
    ####################################          creating the item cluster assignment       ############################################
    #####################################################################################################################################
    
    
    
    #creating item cluster assignment as per the no. of clusters input
    item_cluster <- data.frame("sl_no"=1:item, "item"=c_fast_greedy$names, "cluster"=clusters)
    
    #arranging the item-cluster assignment as per order of items in the dendrogram
    item_cluster <- inner_join(data.frame(a=order.dendrogram(hc)), item_cluster, by=c("a" = "sl_no"))
    
    #writing the item-cluster assignment as a .csv file
    write.csv(item_cluster[,-1], "select_item_cluster_assignment_generic.csv", row.names = FALSE)

    
    
    #####################################################################################################################################
    ####################################          creating the text clusters       ############################################
    #####################################################################################################################################
    
    
    item_names <- unlist(strsplit(c_fast_greedy$names,split="--"))[seq(2,3*item,3)]
    text_dist <- adist(item_names)
    rownames(text_dist) <- item_names
    hclust <- hclust(as.dist(text_dist))
    rollup <- unique(inner_join(data.frame(a=as.character(hclust$labels)),rollup_id_desc,by=c("a"="rollup_desc")))
    hclust$labels <- paste(rollup$a.y,rollup$a,sep="--")
    
    hclust <- as.dendrogram(hclust)
    cls <- as.vector(cutree(hclust,10))    #****#
    hclust <- hclust %>% color_labels(col=cols[cls[order.dendrogram(hclust)]])
    
    
    
    #####################################################################################################################################
    ####################################          creating legend description       ############################################
    #####################################################################################################################################
    
    
    
    text_clusters <- data.frame("item"=item_names, "cluster"=cls)
    for(i in 10:1)     #****# same as the number in cutree
      wordcloud(text_clusters[text_clusters$cluster==i,1],min.freq = 1)

    #names vector is created looking at the wordclouds and selecting the most popular word from there
    names = c("evenflo booster & other miscellaneous","umbrella stroller","graco","rocker-booster",
              "bright starts","baby einstein jumper activity","ingenuity","fisher-price","safety 1st","baby trend")


        
    #####################################################################################################################################
    ####################################          creating the bi-dendrogram       ############################################
    #####################################################################################################################################
    
    
    
    pdf("bi_dendrogram_generic.pdf",height = 45, width = 35)
    par(mar=c(0,0,5,1))
    plot(compute.brlen(as.phylo(flatten.dendrogram(dend)),power=0.9),tip.color = cols[clusters],label.offset = 0.05, cex=1)
    #plot(flatten.dendrogram(hclust), horiz=TRUE, cex=0.2)
    colored_bars(cols[cls[order.dendrogram(as.dendrogram(dend))]],dend,horiz = TRUE,sort_by_labels_order = FALSE,y_shift = -1,y_scale = 0.05)
    legend("topleft", legend = names, fill = cols[1:14], border = "black", bty = "n", cex = 3, 
           title = "LEGEND for TEXT CLUSTERING")
    title(main = "Graph Partition based CDT with text based bar for project-id generic", font.main=2, cex.main=3)
    dev.off()
    
    
    