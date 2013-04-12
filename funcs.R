# TODO: Add comment
# 
# Author: sana wajid
###############################################################################

###ACCESSORS
getNeighbors <- function(cur_Node, tree) {
	tree_Edges <- tree$edge
	mat = tree_Edges[which(tree_Edges[,1] == cur_Node),2]
	mat = mat[mat > cur_Node]
	if(length(mat) > 0){
		(mat)
	}else{
		(0)
	}
}

getChild <-function(node){ 
	mat = moo$edge[which(moo$edge[,1] == node),2]
	mat = mat[mat > node]
	if(length(mat) > 0){
		return(mat)
	}else{
		return(0)
	}
}

getRoot <- function(tree){
	(length(tree$tip.label)+1)
}

getBoot <- function(node){
	node <- as.numeric(node)
	mat = node-length(moo$tip.label)
	as.numeric(tree$node.label[mat])
}

is.leaf <- function(node){
	children = getNeighbors(node, tree)
	if(length(children) > 1 || node <= length(tree$tip.label)+1){
		("F")
	}else if(as.numeric(min(children)) == 0){
		("T")
	}else{
		("F")
	}
}

###OUTPUT
excPerTT <- function(node){
	node <- as.numeric(node)
	db <- mat.db(tree,  node)
	round(sum(subset(db, Var1 == targrp1 | Var1 == targrp2)$Freq)/total_Taxa,3)
#sum(db$Freq)
}

excPerQP <- function(node){
	node <- as.numeric(node)
	db <- mat.db(tree,  node)
	round(sum(subset(db, Var1 == targrp1 | Var1 == targrp2)$Freq)/sum(db$Freq),3)
}


speciesInClade <- function(node){
	node <- as.numeric(node)
	sum((mat.db(tree, node))$Freq)
}

targetGroupInClade <- function(node){
	node <- as.numeric(node)
	sum(subset((mat.db(tree, node)),Var1 == targrp1 | Var1 == targrp2)$Freq)
}

totalInTree <- function(targrp1, targrp2, tree, sum = FALSE){
	#Initializes variables
	if(sum == FALSE){
		numTG1 <<- length(tree$tip.label[tree$tip.label == targrp1])
		numTG2 <<- length(tree$tip.label[tree$tip.label == targrp2])
		sum(numTG1, numTG2)
	}else{
		sum(numTG1, numTG2)		
	}
}
	

summaryWithDF <- function(row){
	nde <- dft$path.node[row]
	print(paste("NE Bootstrap: ", getBoot(nde)))
	print(paste("Target Group in clade: ", targetGroupInClade(nde)))
	print(paste("Species in clade: ", speciesInClade(nde)))
	print(paste("Total in tree: ", total_Taxa))
}


##Modifiers
trim.names <- function(tree){
	moo <- tree	
	moo$tip.label = ((substring(moo$tip.label, 1, attr(unlist(regexpr(".*-",moo$tip.label)), "match.length")-1)))
}

##PLOT 
simPl <- function(treenames){
	moo <- tree <- read.tree(treenames)
	trim.names <- function(tree){
		moo <- tree
		moo$tip.label = ((substring(moo$tip.label, 1, attr(unlist(regexpr(".*-",moo$tip.label)), "match.length")-1)))
	}

	tree$tip.label <- trim.names(tree)
	plot(tree, cex = 0.8)
	nodelabels(cex = 0.8)
}

### MAIN
tSR <- function(t1,t2, tree){
	db <- mat.db(tree, getRoot(tree))			#  D A T A B A S E  O F  L E A V E S     #
	path <<- new('Stack')
	cg.path <<- new('Stack')
	iE(getRoot(tree))
}

iE <- function(i){
	i <- as.numeric(i)
	hlist <- new('Stack')
	
	if(is.leaf(i) == "T"){
#		hlist <- pop(hlist)
#		print("arrived at leassf with node = ", i)
		return()
	}
	
	hlist <- push(hlist, getNeighbors(i, tree))
	while(isEmpty(hlist) == "F"){
		db <- mat.db(tree, as.numeric(first(hlist)))
		i <- first(hlist)
		
		if(containsGroups(db) == "N"){ ##none, dead end
			hlist <- pop(hlist)

		}else if(containsGroups(db) == "NE"){ #nonexclusive
#			slist <<- push(slist, i)
			hlist <- pop(hlist)
			
			path <<- push(path, i)
			cg.path <<- push(cg.path, "NE")
			#print(slist@list)
			
			#		cat(paste("found Non exclusive at node " , i, "with btv = ",getBoot(i),"\n"))
			#print(i)
			iE(i)
			
		}else if(containsGroups(db) == "E"){
			path <<- push(path, i)
			cg.path <<- push(cg.path, "E")
			
			break()
			hlist <- pop(hlist)
		}
	}
	#print(slist@list)
}




isExclusive <- function(i){
	i <- as.numeric(i)
	path <- new('Stack')
	hlist <- new('Stack')
	slist <- new('Stack')
	
	#print(paste("> New stack call with i = ", i))
	
	#BASE CASE
	if(is.leaf(i) == "T"){
#		hlist <- pop(hlist)
#		print("arrived at leaf with node = ", i)
		return()
	}
	
	p <- i #node of choice
	p <- lapply(unlist(p), getNeighbors, tree)
	#hlist <-push(hlist, rapply(as.list(unlist(p)), print, how = "list"))
	hlist <-push(hlist, (as.list(unlist(p))))
	
	
#	k = "F"
	while(isEmpty(hlist) == "F"){
		db <- mat.db(tree, as.numeric(first(hlist)))
		i <- first(hlist)
		
		if(containsGroups(db) == "N"){ ##none, dead end
			hlist <- pop(hlist)
	#		cat(paste("\t\tfound Non at node " , i, "\n"))
			
		}else if(containsGroups(db) == "NE"){ #nonexclusive
			slist <- push(slist, i)
			hlist <- pop(hlist)
			path <- push(path, i)
	#		cat(paste("found Non exclusive at node " , i, "with btv = ",getBoot(i),"\n"))
			isExclusive(i)
			
		}else if(containsGroups(db) == "E"){
	#		cat(paste("\tfound All Exclusive " , i, "with btv = ",getBoot(i),"\n"))
			path <- push(path, i)
			break()
			hlist <- pop(hlist)
		}
	}
	
	path2 <- push(path2, rapply(as.list(unlist(path@list)), print, how = "list"))
	assign("path2",path2,.GlobalEnv)
}

containsGroups <- function(db){ #targrps stays the same but db changes
	sub.db <- subset(db, Var1 == targrp1 | Var1 == targrp2)
	num.outgrp <- (nrow(db)-nrow(sub.db))
	#N = end search
	#NE = non exclusive
	#E = exclusive
	
	### Contains Outgroups
	if( (num.outgrp) > 0 )	{ 		
		
		### ### Both target groups exist
		if((nrow(sub.db) == 2)){
			("NE") 
		}else if ((nrow(sub.db) != 2)){
			("N")
		}
	}else if (num.outgrp == 0 && nrow(sub.db) == 2){
		("E") 
	}else{
		("N")
	}
}


getNames <- function(cur_Node, tree) {
	r <<- push(r, (tree$edge[tree$edge[,1] == cur_Node,][,2]))
	
	while(isEmpty(r) == "F"){ #&& is.leaf(cur_Node) == "F"){
		num <<- as.numeric(first(r))

		if(num > cur_Node){
			#k <<- push(k, num)
			r <<- pop(r)
			getNames((num), tree)
			
		}else if(num < cur_Node){
			#r <<- push(r, num)
			taxa <<- tree$tip.label[num]
			tax <<- push(tax, taxa)
			r <<- pop(r)

		}else{	
			stop("Error: edges cannot be duplicated")
		}
	}
}

o <<- new('Stack') #tax
r <<- new('Stack') #nodes

gS <- function(node,tree){

	if(is.leaf(node) == "T"){
		r <<- pop(r)
	}
	
	k <<-tree$edge[tree$edge[,1] == node,][,2]
	lapply(k, function(x) 
		if(x >= getRoot(tree)){ 
#			r <<- push(r, x)
			return(gS(x, tree))
		} else if(x < getRoot(tree)){
			o <<- push(o, tree$tip.label[x])
		}else{
			stop("Error: getting taxa names")
		}
	)
}

PgS <- function(node,tree){
	if(is.leaf(node) == "T"){
		r <<- pop(r)
	}
	
	k <<-tree$edge[tree$edge[,1] == node,][,2]
	mclapply(k, function(x) 
				if(x >= getRoot(tree)){ 
#			r <<- push(r, x)
					return(gS(x, tree))
				} else if(x < getRoot(tree)){
					o <<- push(o, tree$tip.label[x])
				}else{
					stop("Error: getting taxa names")
				}
	,mc.cores = 4)
}


Pmat.db <- function(tree, node){
	o <<- new('Stack') #tax
	r <<- new('Stack') #nodes
	PgS(node, tree)
	as.data.frame(table(sapply(o@list, as.vector)))
#	(table(node.leaves(moo, node)))
}

#o <<- new('Stack') #tax
#r <<- new('Stack') #nodes
#gS(101, tree)

mat.db <- function(tree, node){
	o <<- new('Stack') #tax
	r <<- new('Stack') #nodes
	gS(node, tree)
	as.data.frame(table(sapply(o@list, as.vector)))
#	(table(node.leaves(moo, node)))
}


cat.db <- function(tree, node){
	r <<- new('Stack') #names
	tax <<- new('Stack')
	getNames(node, tree)
	as.data.frame(table(sapply(tax@list, as.vector)))
#	(table(node.leaves(moo, node)))
}

#Put in empty stack, output stack of only *.tres
isolateTrees <- function(treeStk){
	tree.names <<- as.vector(dir())
	p <- 1
	while(p < length(tree.names)){
		wrd <- substring(tree.names[p], nchar(tree.names[p])-3, nchar(tree.names[p]))
		if(wrd == ".tre"){
			t.names <<- push(t.names, tree.names[p])
		}
		p <- p + 1
	}
	print(paste("Found trees to sort:",size(t.names)))
}

makeDftall <- function(){
	dft.all <<- data.frame(	name = 0, 
			path.node=0, 
			path.btv = 0, 
			trgp.clade = 0,
			spec.clade = 0,
			total.tree = 0,
			type.phylo = 0,
			query.propTT = 0, 
			query.prop = 0, 
			pass=0)
}

makeDft <- function(){
	dft <<- data.frame(	name = 0, 
			path.node=0, 
			path.btv = 0, 
			trgp.clade = 0,
			spec.clade = 0,
			total.tree = 0,
			type.phylo = 0,
			query.propTT = 0, 
			query.prop = 0, 
			pass=0)
}

#mat.db <- function(tree, node){
#	as.data.frame(table(node.leaves(tree, node)))
#}

#i <- 71
#while(i < 83){
#	print(paste("Subtree with node = ",i)) 
#	j <- as.data.frame(table(mat.db(tree, i)))
#	print(j)
#	i <- i + 1
#}
