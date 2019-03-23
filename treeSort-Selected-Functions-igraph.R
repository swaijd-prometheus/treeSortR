# Author: sana wajid (2013-2014)
###############################################################################
####iGRAPH Functions####
library(igraph)

uniqeify<-function(x, sep=" "){
  paste(x, ave(x, x, FUN=seq_along), sep=sep)
}

toGraph <- function(cur_Tree){
  cur_Tree$node.label[1] <<- " "
  print("phylo => igraph")
}

findNames <- function(names, list, value = FALSE){
  #  if(value == TRUE){
  print(grep(names, list, value = value))
}

centralNode <- function(myGraph){
  which(degree(myGraph) %in% c(max(degree(myGraph))))
}

leafNodes <- function(myGraph){
  which(degree(myGraph) %in% c(1))
}

getNodeNames <- function(node, myGraph){
  V(graph)[neighbors(myGraph, node)]
}

toNode <- function(name, myGraph){
  grep(name, V(myGraph)$name)
}

toName <- function(node, myGraph){
  V(myGraph)[node]
}

