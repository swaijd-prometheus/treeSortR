# Author: sana wajid (2013-2014)
###############################################################################
library(ape)
source("stackR.R")
numTargrp2 <- 0
numTargrp1 <- 0 
tax <<- ""

set.taxA <- function(taxA){
  targrp1 <<- as.character(taxA)
}

set.taxB <- function(taxB){
  targrp2 <<- as.character(taxB)
}

set.MinBoostrapThreshold <- function(val){
  val <- as.numeric(val)
  if(val >= 0 && val <= 100){
    MinBoostrapThreshold <<- val
  }
}

set.RelativeQueryPropThreshold <- function(val){
  val <- as.numeric(val)
  if(val >= 0 && val <= 100){
    RelativeQueryPropThreshold <<- val/100
  }
}

set.AbsoluteQueryPropThreshold <- function(val){
  val <- as.numeric(val)
  if(val >= 0 && val <= 100){
    AbsoluteQueryPropThreshold <<- val/100
  }
}

set.TargetDirectory <- function(loc){
  loc <- as.character(loc)
  #makeFol(loc)
  setwd(loc)
}

set.Regex <- function(val){
  nameRe <- as.character(val)
}


set.DistinctPhylaThreshold <- function(val){
  val <- as.numeric(val)
  if(val >= 0){
    DistinctPhylaThreshold <<- val
  }
}

set.TerminalTaxaThreshold <- function(val){
  val <- as.numeric(val)
  if(val >= 0){
    TerminalTaxaThreshold <<- val
  }
}	

set.NonExclusiveThreshold <- function(val){
  val <- as.numeric(val)
  if(val >= 0){
    NonExclusiveThreshold <<- val
  }
}


autoPlot <- function(cur_Tree){  
  if(cur_Tree$Nnode >= 80){
    plot(cur_Tree, cex = 0.5)
    nodelabels(cex = 0.6)
  }else if(cur_Tree$Nnode >= 50){
    plot(cur_Tree, cex = 0.7)
    nodelabels(cex = 0.7)
  }else{
    plot(cur_Tree)
    nodelabels()
  }
}

getEdges <- function(cur_Node, cur_Tree){
  cur_Node <- as.numeric(cur_Node)
  
  cur_Tree_Edges <- cur_Tree$edge
  mat_Neighbors <- cur_Tree_Edges[which(cur_Tree_Edges[,1] == cur_Node),2]
  mat_Neighbors <- mat_Neighbors[mat_Neighbors < cur_Node]
  return(mat_Neighbors)
}

getNeighbors <- function(cur_Node, cur_Tree) {
  cur_Node <- as.numeric(cur_Node)
  mat_Ancestor <- 0
  mat_Neighbors <- 0
  
  if(cur_Node == getRoot(cur_Tree)){
    mat_Ancestor <- getAncestor(cur_Node, cur_Tree)
  }
  
  if(mat_Ancestor == 0){
    cur_Tree_Edges <- cur_Tree$edge
    mat_Neighbors <- cur_Tree_Edges[which(cur_Tree_Edges[,1] == cur_Node),2]
    mat_Neighbors <- mat_Neighbors[mat_Neighbors > cur_Node]
  }else{
    mat_Neighbors <- mat_Ancestor
  }
  
  if(length(mat_Neighbors) == 0){
    mat_Neighbors <- 0	
  }
  
  return(mat_Neighbors)
}

#So check including ancestor
areNeighbors <- function(node_A, node_B, cur_Tree){
  #	mat_Ancestor <- 0
  mat_Neighbors <- 0
  out <- FALSE
  #CHECK IF NODE EXISTS
  if(node_A < node_B){
    
    mat_Neighbors <- cur_Tree$edge[which(cur_Tree$edge[,1] == node_A),2]
    mat_Neighbors <- mat_Neighbors[which(mat_Neighbors > node_A)]
    
    
    #	e <- intersect(node_B, mat_Neighbors)	
    #	if(e == node_B && length(e) > 0){
    #		out <- TRUE
    #	}
    if(node_B %in% mat_Neighbors){
      out <- TRUE
    }else{
      if(length(mat_Neighbors) == 1){
        o <- cur_Tree$edge[which(cur_Tree$edge[,1] == mat_Neighbors),2]
        if(length(which(o > mat_Neighbors)) == length(o)){
          mat_Neighbors <- o
        }
      }
      if(node_B %in% mat_Neighbors)
        out <- TRUE
    }
    
  }else{
    
    mat_Neighbors <- cur_Tree$edge[which(cur_Tree$edge[,1] == node_B),2]
    mat_Neighbors <- mat_Neighbors[which(mat_Neighbors > node_B)]
    
    
    if(node_A %in% mat_Neighbors){
      out <- TRUE
    }else{
      if(length(mat_Neighbors) == 1){
        o <- cur_Tree$edge[which(cur_Tree$edge[,1] == mat_Neighbors),2]
        if(length(which(o > mat_Neighbors)) == length(o)){
          mat_Neighbors <- o
        }			
      }
    }
    if(node_A %in% mat_Neighbors){
      out <- TRUE
    }
  }
  return(out)
}



isRoot <- function(cur_Node, cur_Tree){
  if(cur_Node == getRoot(cur_Tree)){
    return(TRUE)
  }else{
    return(FALSE)
  }
}



getAncestor <- function(cur_Node, cur_Tree){
  cur_Node <- as.numeric(cur_Node)
  #if(getRoot(cur_Tree) == cur_Node){
  cur_Tree_Edges <- cur_Tree$edge
  mat_Neighbors = cur_Tree_Edges[which(cur_Tree_Edges[,2] == cur_Node),1]
  
  if(length(mat_Neighbors) == 0){
    mat_Neighbors <- 0	
  }
  
  return(mat_Neighbors)
  
}

getRoot <- function(cur_Tree){
  rootOffset <- which(is.na(as.numeric(cur_Tree$node.label)))
  if(length(rootOffset) == 0){
    rootOffset <- 0
  }
  
  return(length(cur_Tree$tip.label)+rootOffset)
}

#Removes the NA vector wherever it is, pushes it to beginning of the list
fixBootNA <- function(cur_Tree){
  #		cur_Tree_k <<- cur_Tree
  nlbl <- cur_Tree$node.label
  nlbl <- nlbl[!is.na(as.numeric(nlbl))]
  return(as.vector(c("", nlbl)))
  #print(cur_Tree_k$node.label)
  #	print(cur_Tree_k$node.label)
  #	getBoot(cur_Node, cur_Tree_k)
}

#Can sometimes be NAs
getBoot <- function(cur_Node, cur_Tree){
  cur_Node <- as.numeric(cur_Node)
  boot_Position = cur_Node-length(cur_Tree$tip.label)
  bootstrap <- as.numeric(cur_Tree$node.label[boot_Position])
  
  if(is.na(bootstrap)){
    
    bootNeighbors <- getNeighbors(cur_Node, cur_Tree)
    if(length(bootNeighbors) == 1){
      if(bootNeighbors[1] == 0){
        bootNeighbors <- getAncestor(cur_Node, cur_Tree)
        #check.Bootstrap(as.numeric(bootNeighbors), cur_Tree)
        return(getBoot(bootNeighbors, cur_Tree))
      }else{
        #check.Bootstrap(as.numeric(bootNeighbors), cur_Tree)
        return(getBoot(bootNeighbors, cur_Tree))
      }
    }
  }
  return(bootstrap)
}


###USE in/out # nodes from adj list
isLeaf <- function(cur_Node, cur_Tree){
  cur_Node <- as.numeric(cur_Node)
  node_Neighbors <- getNeighbors(cur_Node, cur_Tree)
  
  if(cur_Node <= length(cur_Tree$tip.label)+1){ 	#Floor
    return(FALSE)
  }else{											#Ceiling
    if(as.numeric(min(node_Neighbors)) == 0){
      return(TRUE)
    }else{
      #return("ERROR in isLeaf")
      return(FALSE)
    }
  }
}

isTerminal <- function(cur_Node, cur_Tree){
  cur_Node <- as.numeric(cur_Node)
  
  out <- FALSE
  if(isLeaf(cur_Node, cur_Tree) || isRoot(cur_Node, cur_Tree)){
    out <- TRUE
  }
  
  return(out)
}

getNames <- function(cur_Node, cur_Tree, nested = FALSE){
  cur_Node <- as.numeric(cur_Node)
  
  
  if(nested == TRUE){
    
    if(isLeaf(cur_Node, cur_Tree) == TRUE)	{
      #	r <<- pop(r)
      r <<- r[-1]
    }
    k <<- cur_Tree$edge[cur_Tree$edge[,1] == cur_Node,][,2]
    
    lapply(k, function(x) 
      if(x >= getRoot(cur_Tree)){ 
        return(getNames(x, cur_Tree))
      } else if(x < getRoot(cur_Tree)){
        tax <<- c(tax, cur_Tree$tip.label[x])
      }else{
        stop("Error: getting taxa names")			
      }
    )
    
  }else{
    
    return(cur_Tree$tip.label[getEdges(cur_Node, cur_Tree)])
    
  }
}

#Number of species PROCEEDING from cur_Node
speciesInClade <- function(cur_Node, cur_Tree){
  cur_Node <- as.numeric(cur_Node)
  if(cur_Node == getRoot(cur_Tree)){
    return(cur_Node - 1)
  }else{
    #return(sum((getNodeDB(cur_Node, cur_Tree))$Freq))
    return(sum(as.data.frame((table(getNodeDB(cur_Node, cur_Tree))))$Freq))
  }
}

speciesInTree <- function(cur_Tree){
  return(numTerminalTaxa(0, cur_Tree, TRUE))
}

targetGroupInClade <- function(cur_Node, cur_Tree, conjunction = FALSE){
  cur_Node <- as.numeric(cur_Node)
  
  mat_Taxa <- getNodeDB(getRoot(cur_Tree), cur_Tree)
  mat_Taxa <- as.character(mat_Taxa)
  
  num_targetGroupInClade <- 0
  
  taxa_Dictionary <- c(targrp1, targrp2)
  
  numTargrp1 <- length(unlist((lapply(taxa_Dictionary[1], grep, mat_Taxa, ignore.case = TRUE))))
  numTargrp2 <- length(unlist((lapply(taxa_Dictionary[2], grep, mat_Taxa, ignore.case = TRUE))))
  
  if(conjunction == FALSE){
    num_targetGroupInClade <- numTargrp1+numTargrp2
  }else{
    if(numTargrp1 > 0 & numTargrp2 > 0){
      num_targetGroupInClade <- numTargrp1+numTargrp2	
    }
  }
  return(num_targetGroupInClade)
}

targetGroupInTree <- function(cur_Tree, conjunction = FALSE){
  
  mat_Taxa <- cur_Tree$tip.label
  mat_Taxa <- as.character(mat_Taxa)
  
  num_targetGroupInTree <- 0
  
  
  taxa_Dictionary <- c(targrp1, targrp2)
  
  numTargrp1 <- length(unlist((lapply(taxa_Dictionary[1], grep, mat_Taxa, ignore.case = TRUE))))
  numTargrp2 <- length(unlist((lapply(taxa_Dictionary[2], grep, mat_Taxa, ignore.case = TRUE))))
  
  if(conjunction == FALSE){
    num_targetGroupInTree <- numTargrp1+numTargrp2
  }else{
    if(numTargrp1 > 0 & numTargrp2 > 0){
      num_targetGroupInTree <- numTargrp1+numTargrp2	
    }
  }
  return(num_targetGroupInTree)
}

totalNumTaxa <- function(cur_Tree, defaultRoot = FALSE){
  mat_Taxa <- 0
  if(defaultRoot == TRUE){
    num_Taxa <- length(cur_Tree$tip.label)
  }else{
    num_Taxa <- speciesInClade(getRoot(cur_Tree), cur_Tree)
  }
  return(num_Taxa)
}

totalTGInTree <- function(targrp1, targrp2, cur_Tree, sum = FALSE){
  #Initializes variables
  if(sum == FALSE){
    numTG1 <<- length(cur_Tree$tip.label[sapply(strsplit(as.character(cur_Tree$tip.label), "_")) == targrp1])
    numTG2 <<- length(cur_Tree$tip.label[sapply(strsplit(as.character(cur_Tree$tip.label), "-")) == targrp2])
    sum(numTG1, numTG2)
  }else{
    sum(numTG1, numTG2)		
  }
}

numDistinctPhyla <- function(cur_Node, cur_Tree, defaultRoot = FALSE){
  cur_Node <- as.numeric(cur_Node)
  
  mat_Taxa <- NULL
  
  if(defaultRoot == TRUE){
    mat_Taxa <- cur_Tree$tip.label
    
  }else{
    mat_Taxa <- getNodeDB(cur_Node, cur_Tree)
  }
  
  #	print(mat_Taxa)
  
  return(nrow(as.data.frame(table(sapply(strsplit(as.character(mat_Taxa), "-"), head, 1)))))
}

dfDistinctPhyla <- function(cur_Node, cur_Tree){
  cur_Node <- as.numeric(cur_Node)
  
  df <- as.data.frame(table(sapply(strsplit(as.character(getNodeDB(cur_Node, cur_Tree)), "-"), head, 1)))
  names(df) <- c("tax", "Freq")
  df <- as.character(df)
  return(df)
}

numTerminalTaxa <- function(cur_Node, cur_Tree, defaultRoot = FALSE){
  cur_Node <- as.numeric(cur_Node)
  
  mat_Taxa <- NULL
  
  if(defaultRoot == TRUE){
    mat_Taxa <- cur_Tree$tip.label	
  }else{
    mat_Taxa <- getNodeDB(cur_Node, cur_Tree)
  }
  
  return(length(mat_Taxa))
}

#sapply(strsplit(as.character(cur_Tree$tip.label), "_"))
#Relative query prop with respect to total taxa
relativeQueryProp <- function(cur_Node, cur_Tree, conjection = FALSE){
  cur_Node <- as.numeric(cur_Node)
  
  if(speciesInClade(cur_Node, cur_Tree) == 0){
    return(0)
  }else if(conjection == TRUE){
    round(targetGroupInClade(cur_Node, cur_Tree, TRUE)/speciesInClade(cur_Node, cur_Tree), 3)
  }else{
    round(targetGroupInClade(cur_Node, cur_Tree)/speciesInClade(cur_Node, cur_Tree), 3)
  }
}

#Absolute query prop with respect to total taxa
absoluteQueryProp <- function(cur_Node, cur_Tree, conjection = FALSE){
  
  cur_Node <- as.numeric(cur_Node)
  if(totalNumTaxa(cur_Tree) == 0){
    return(0)
  }else if(conjection == TRUE){
    round(targetGroupInClade(cur_Node, cur_Tree, TRUE)/totalNumTaxa(cur_Tree), 3)
  }else{
    round(targetGroupInClade(cur_Node, cur_Tree)/totalNumTaxa(cur_Tree), 3)
  }
}

getArgs <- function(my_Arguments){
  if(length(my_Arguments) > 0){
    readParams(my_Arguments)
  }else{
    #SET#
    tgrps1("Viridiplantae")#("Glaucophyta")#("Amoebozoa")
    tgrps2("Amoebozoa")#("Viridiplantae")
    minBoostrapThreshold("0")
    queryPropTT("0")
    queryProp("0")
    #setwd(path.expand("~/Documents/DBLab/treeSortR")) #/CYANOPHORA_BS90"))	# SET WORKING DIR
  }
}

summaryWithDF <- function(row){
  nde <- dft$path.node[row]
  print(paste("NE Bootstrap: ", getBoot(nde)))
  print(paste("Target Group in clade: ", targetGroupInClade(nde)))
  print(paste("Species in clade: ", speciesInClade(nde)))
  print(paste("Total in tree: ", total_Taxa))
}

getOutGroup <- function(targrp1, targrp2, cur_Node, cur_Tree, mat_Taxa = FALSE) { #targrp1 & 2
  cur_Node <- as.numeric(cur_Node)
  
  if(mat_Taxa == FALSE){
    mat_Taxa <- getNodeDB(cur_Node, cur_Tree)
  }
  
  mat_Taxa <- as.character(mat_Taxa)
  taxa_Dictionary <- c(targrp1, targrp2)
  
  mat_Outgroup <- unlist((lapply(taxa_Dictionary, grep, mat_Taxa, ignore.case = TRUE, value = TRUE, invert = TRUE)))
  return(mat_Outgroup[duplicated(mat_Outgroup)])
}

#Returns the outgroup of the default tree
getOutGroupOfTree <- function(targrp1, targrp2, cur_Tree) { #targrp1 & 2
  
  mat_Taxa <- cur_Tree$tip.label
  mat_Taxa <- as.character(mat_Taxa)
  
  taxa_Dictionary <- c(targrp1, targrp2)
  
  mat_Outgroup <- unlist((lapply(taxa_Dictionary, grep, mat_Taxa, ignore.case = TRUE, value = TRUE, invert = TRUE)))
  return(mat_Outgroup[duplicated(mat_Outgroup)])
}

getInGroup <- function(targrp1, targrp2, cur_Node, cur_Tree){ #targrp1 & 2
  mat_Taxa <- getNodeDB(cur_Node, cur_Tree)
  mat_Taxa <- as.character(mat_Taxa)
  
  taxa_Dict <- c(targrp1, targrp2)
  
  return(unlist((lapply(taxa_Dict, grep, mat_Taxa, ignore.case = TRUE, value = TRUE))))
}

getInGroupOfTree <- function(targrp1, targrp2, cur_Tree){ #targrp1 & 2
  mat_Taxa <- cur_Tree$tip.label
  mat_Taxa <- as.character(mat_Taxa)
  
  taxa_Dict <- c(targrp1, targrp2)
  
  return(unlist((lapply(taxa_Dict, grep, mat_Taxa, ignore.case = TRUE, value = TRUE))))
}

#Checks if the clade contains either all targetGroup1 or all targetGroup2 
onlyTargetGroupInClade <- function(targrp1, targrp2, cur_Node, cur_Tree, quota){ #targrp1 & 2
  cur_Node <- as.numeric(cur_Node)
  
  mat_Taxa <- getNodeDB(cur_Node, cur_Tree)
  mat_Taxa <- as.character(mat_Taxa)
  
  taxa_Dict <- c(targrp1, targrp2)
  mat_inGroup <- unlist((lapply(taxa_Dict, grep, mat_Taxa, ignore.case = TRUE, value = TRUE)))
  
  if(length(mat_Taxa) >= quota & length(mat_inGroup) >= quota & length(mat_Taxa) == length(mat_inGroup)){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

#onlyTargetGroupInClade(targrp1, targrp2, 38, a.tree, 2)

#CONTAINS GROUPS 2
containsGroups <- function(cur_Node, cur_Tree, out = FALSE){ #targrps stays the same but db changes
  cur_Node <- as.numeric(cur_Node)
  
  words <- c(targrp1, targrp2)
  outGroup <- getOutGroup(targrp1, targrp2, cur_Node, cur_Tree)	
  inGroup <- getInGroup(targrp1, targrp2, cur_Node, cur_Tree)
  num.splitByTargetGroup <- splitByTargetGroup(targrp1, targrp2, inGroup, TRUE)
  
  if(length(outGroup) > 0) {								# Contaminated
    if(num.splitByTargetGroup == length(words)){ 		# Both target groups exist
      ("NE")
    }else{												# Only one target group exists
      ("N")		# ("N-not NE")
    }
  }else if(length(outGroup) == 0){						# No OutGroups
    if(num.splitByTargetGroup == length(words)){		# Both target groups exist
      ("E")
    }else{												# Only one target group exists
      ("N")	#("N-not E")
    }
  }else{
    stop("Error: OutGroup calculation is negative!")	# Error
  }
}

getNodeDB <- function(cur_Node, cur_Tree){
  cur_Node <- as.numeric(cur_Node)
  
  
  cur_Node <- as.numeric(cur_Node)
  
  if(nodeDB[cur_Node] != "NULL"){
    nodeDB[[cur_Node]]
  }else{
    makeNodeDB(cur_Node, cur_Tree, TRUE)
    nodeDB[[cur_Node]]
  }
}

makeNodeDB  <- function(cur_Node, cur_Tree, new = FALSE){
  cur_Node <- as.numeric(cur_Node)
  #	If exists, invoke mat.db
  
  if(nodeDB[cur_Node] == "NULL" || new == TRUE){
    
    #		message("Entry found")
    #		nodeDB[cur_Node]
    #		message("Entry not found")
    
    #	if(as.numeric(cur_Node) == as.numeric(getRoot(cur_Tree))){
    
    #		nodeDB[[cur_Node]] <<- setdiff(as.character(mat.db(as.numeric(getRoot(cur_Tree)), cur_Tree)), as.character(mat.db(as.numeric(getRoot(cur_Tree))+1, cur_Tree)))
    #		nodeDB[[cur_Node]] <<- setdiff(as.character(mat.db(as.numeric(getRoot(cur_Tree)), cur_Tree)), cur_Tree$tip.label)
    
    #	Don't go to mat.db
    
    #	}else{
    nodeDB[[cur_Node]] <<- mat.db(cur_Node, cur_Tree)	
    #	}
  }
}

clearNodeDB <- function(nodeDB = FALSE){
  nodeDB <<- list()
}

mat.db <- function(cur_Node, cur_Tree){
  tax <<- character() #tax
  r <<- numeric() #nodes
  tax <<-getNames(cur_Node, cur_Tree)
  return(tax)
}

#Put in empty stack, output stack of only *.tres
#Creates treeStack, consists of trees
isolateTrees <- function(directory=FALSE){
  
  treeStack <<- new('Stack')
  
  if(directory == FALSE){
    treeList <<- as.vector(dir())
  }else{
    treeList <<- as.vector(path.expand(directory))
  }
  
  if(length(treeList) > 0){
    p <- 1
    while(p < length(treeList)){
      wrd <- substring(treeList[p], nchar(treeList[p])-3, nchar(treeList[p]))
      if(wrd == ".tre"){
        treeStack <<- push(treeStack, treeList[p])
      }
      p <- p + 1
    }
  }
  
  if(size(treeStack) > 0){
    print(paste("Found trees to sort:", size(treeStack)))
    return(TRUE)
  }else{
    return(FALSE)
  }
}

truncatedNames <- function(cur_Tree, regex){
  return(sapply(strsplit(cur_Tree$tip.label, regex), head, 1))
}

