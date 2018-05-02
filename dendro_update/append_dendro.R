## omahala

library(dendextend)
library(phytools)

#dend0 <- dend object for 1st subcat
#dend1 <- dend object for 2nd subcat
#dend2 <- dend object for 3rd subcat

#k0 <- no. of clusters for 1st subcat
#k1 <- no. of clusters for 2nd subcat
#k2 <- no. of clusters for 3rd subcat

#clusters0 <- cutree(dend0, k0)  ## DO NOT USE as.vector() OVER cutree()    cluster assignments for 1st subcat  
#clusters1 <- cutree(dend1, k1)  ## DO NOT USE as.vector() OVER cutree()    cluster assignments for 2nd subcat
#clusters2 <- cutree(dend2, k2)  ## DO NOT USE as.vector() OVER cutree()    cluster assignments for 3rd subcat

all_dend <- merge(dend0, dend1)
all_dend <- merge(all_dend, dend2)
all_clus <- c(clusters0, k0+clusters1, k0+k1+clusters2)
all_dend <- branches_attr_by_clusters(all_dend, all_clus[order.dendrogram(all_dend)], values=colors(distinct = TRUE)) %>% color_labels(col=colors(distinct = TRUE)[all_clus])

dend_phylo <- compute.brlen(as.phylo(all_dend), power = 0.9)
clade0 <- labels(dend0)
clade1 <- labels(dend1)
clade2 <- labels(dend2)
edge_clade.list <- list(clade1, clade2, clade3)
tip_clade.list <- split(names(all_clus), as.vector(all_clus))


tip.color <- function(phy, tips, regex = FALSE, col, bgcol){
  if (!inherits(phy, "phylo"))
    stop("'phy' is not of class 'phylo'")
  if (missing(bgcol)) bgcol <- "black"
  tiplab <- phy$tip.label
  nbtips <- length(tiplab)
  tipcol <- rep(bgcol, nbtips)
  
  if (!is.list(tips)) tips <- list(tips)
  
  for (i in seq(along = tips)){
    pattern <- paste(tips[[i]], collapse = "|")
    tipcol[grep(pattern, tiplab, fixed = FALSE)] <- col[i]
  }
  tipcol
}

edge.color <- function(phy, groups, what = "crown", col, bgcol){
  if (missing(bgcol)) bgcol <- "black"
  if (missing(col)) col <- "red"
  if (is.character(groups)) groups <- list(groups)
  n <- ifelse(is.matrix(groups), nrow(groups), length(groups))
  col <- rep(col, length.out = n)
  
  what <- match.arg(what, c("crowngroup", "stemgroup"))
  stem <- ifelse(what == "stemgroup", TRUE, FALSE)
  
  ecol <- rep(bgcol, nrow(phy$edge))
  mrcas <- mrca(phy)
  tab2mrca <- function(x, mrcas, phy){
    if (is.monophyletic(phy, x)){
      mrcas[rownames(mrcas) == x[1], colnames(mrcas) == x[2]]
    }
    else {
      MRCA <- noi(phy, groups)
      tip.id <- which(phy$tip.label %in% groups)
      
      edge.id <- vector()
      for (i in seq_along(tip.id)){
        outer <- inner <- tip.id[i]
        while (inner != MRCA){
          inner <- phy$edge[phy$edge[, 2] == outer[1], 1]
          outer <- c(inner, outer)
        }
        edge.id <- c(edge.id, which(phy$edge[, 2] %in% outer))
      }
      unique(edge.id)
    }
  }
  if (is.matrix(groups)) {
    groups <- apply(groups, 1, tab2mrca, mrcas = mrcas, phy = phy)
  }
  if (is.numeric(groups)) {
    id <- lapply(groups, descendants, phy = phy, type = "t")
  }					
  else {
    id <- groups
  }
  
  for (i in seq_along(id)){	
    ind <- which.edge(phy, id[[i]])
    if (stem && length(ind) > 1) ind <- c(ind, min(ind) - 1)
    ecol[ind] <- col[i]
  }
  ecol
}

cols <- c(brewer.pal(10,"Paired"))

edge_color <- sample(cols, length(edge_clade.list))
distinct_color <- colors()[c(7,8,12,17,23,24,30,31,32,33,42,45,47,51,52,56,62,68,74,76,81,84,95,100,101,107,108,114,115,116,120,121,125,128,132,142,149,188,258,259,254,367,371,372,375,376,382,386,392,399,403,404,417,419,423,424,428,429,435,441,450,454,455,459,460,461,463,468,472,474,475,476,477,483,488,490,493,495,499,502,503,507,509,514,518,523,524,535,547,552,556,558,565,570,575,586,592,596,611,614,616,622,632,640,641,645,654,657)]
tip_color <- sample(distinct_color, length(tip_clade.list))

tcol <- tip.color(dend_phylo, tip_clade.list, col = tip_color)
ecol <- edge.color(dend_phylo, edge_clade.list, col = edge_color)

pdf(file="whatever.pdf", height = 200, width = 150)
par(mar=c(0,0,10,2))
plot(dend_phylo, tip.color = tcol, edge.color = ecol, label.offset = 0.005, cex = 3)
title(main = "Appended Dendrogram for Writing", cex.main=15)
dev.off()
