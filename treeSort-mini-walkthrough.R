#################################################
library(ape)
library(igraph)

setwd("Desktop/swajid-R-Code-Snippets")
source("treeSort-Selected-Functions.R")
source("treeSort-Selected-Functions-igraph.R")
source("stackR.R")
treeName <- "Data/Glaucophyta-Cyanophora_paradoxa_dxRU30.gp.tre"

###If issues, try this:##########################
#setwd("Data")
#treeName <- "Glaucophyta-Cyanophora_paradoxa_dxRU30.gp.tre"
#################################################

myTree <- read.tree(treeName)

#Plot
autoPlot(myTree) #IF this plots, then the rest should work fine

#Neighbors
areNeighbors(60,67, myTree) #No, these two nodes are not neighbors
areNeighbors(60,55, myTree) #TRUE, these node pairs are connected

#Roots
isRoot(55, myTree) #False, 
isRoot(51, myTree) #Node 51 is the arbitrary root 

#Ancestors
getAncestor(55, myTree)
getAncestor(getRoot(myTree), myTree) #Returns 0 because the root doesn't have an ancestor

#Bootstraps
getBoot(getRoot(myTree), myTree) #Root doesn't have a bootstrap value

getNeighbors(getRoot(myTree),myTree) #Manually move the pointer

lapply(getNeighbors(getRoot(myTree),myTree), function(x) {
  getBoot(x, myTree)
})
#Bootstrap for node 52 is 68 and for node 55 is 100.

#Leaf nodes
isLeaf(79, myTree) #FALSE 
isLeaf(98, myTree) #TRUE

#Descendant taxa
#no terminal taxa names are pointed directedly by this node
tax <<- ""
getNames(62, myTree, nested = FALSE) 

#If we want to traverse the tree until the pointer reaches the end of the path, the results are different
tax <<- ""
getNames(62, myTree, nested = TRUE) 

#Truncate Taxa Names based on regex
#The tree is the same but the names are shorter
truncTree <- myTree
truncTree$tip.label <- truncatedNames(truncTree,"-")

clearNodeDB()

autoPlot(truncTree) #Easy to view names

speciesInTree(truncTree)
speciesInClade(63, truncTree)

set.taxA("Viridiplantae")
set.taxB("Stramenopiles")

targetGroupInTree(truncTree)
targetGroupInClade(55, truncTree)


#Conversion to iGraph

#Make tip.label names unique otherwise graph will have cycles
truncTree$tip.label <- uniqeify(truncTree$tip.label, ".")

#Label rooot/Remove empty bootstrap vector
truncTree$node.label[1] = "root"
truncTree$node.label <- uniqeify(truncTree$node.label, ".")
#Now the graph holds the same topological relationships as a bifurcating phylogenetic tree (cur_Tree or truncTree).


#invoke as.igraph from APE
myGraphFromTree <- as.igraph(truncTree)

#Plot
plot(myGraphFromTree)

#Central Node
toName(centralNode(myGraphFromTree), myGraphFromTree) #100, makes sense this bootstrap value is most common

#All leaf nodes are taxa names and not bootstrap
toName(leafNodes(myGraphFromTree), myGraphFromTree)

#Search for nodes containing "Viridiplant"
toName(toNode("Viridiplant", myGraphFromTree), myGraphFromTree)

#Display as nodes
toNode("Viridiplant", myGraphFromTree)

#Search for nodes containing "Viridiplant"
toName(toNode("Metazoa", myGraphFromTree), myGraphFromTree)

#Display as nodes
toNode("Metazoa", myGraphFromTree)

#Get shortest path between two "random" nodes
##Shortest path should contain " "
V(myGraphFromTree)[get.shortest.paths(myGraphFromTree, 40,68)$vpath[[1]]]



