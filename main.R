####################################################################################
#
#
# 888                             .d8888b.                   888    8888888b.  
# 888                            d88P  Y88b                  888    888   Y88b 
# 888                            Y88b.                       888    888    888 
# 888888 888d888 .d88b.   .d88b.  "Y888b.    .d88b.  888d888 888888 888   d88P 
# 888    888P"  d8P  Y8b d8P  Y8b    "Y88b. d88""88b 888P"   888    8888888P"  
# 888    888    88888888 88888888      "888 888  888 888     888    888 T88b   
# Y88b.  888    Y8b.     Y8b.    Y88b  d88P Y88..88P 888     Y88b.  888  T88b  
#  "Y888 888     "Y8888   "Y8888  "Y8888P"   "Y88P"  888      "Y888 888   T88b 
#                    aaa                                                      
# 2012-2013
###################################################################################

#!/bin/Rscript
library(ape)
#initial location of package and scripts
setwd(path.expand("~/git/tSR/treeSortR/tSR"))

#Maybe this is a better way to load scripts:
source("structures/stackR.R")
source("sysfuncs.R")
source("funcs.R")
source("args.R")

#Take in arguments unless using main.R from R
args <- commandArgs(trailingOnly = TRUE)
if(length(args) > 0){ 
	readParams(args)
}else{
	tgrps1("Viridiplantae")#("Glaucophyta")#("Amoebozoa")
	tgrps2("Amoebozoa")#("Viridiplantae")
	minBoostrapThreshold("0")
	queryPropTT("0")
	queryProp("0")
	#setwd(path.expand("~/Documents/DBLab/treeSortR")) #/CYANOPHORA_BS90"))	# SET WORKING DIR
}


#remove dft objects
if(exists("dft.all")){remove(dft.all)}
if(exists("dft")){remove(dft)}

#sink(paste("tSR_dft",targrp1,"-",targrp2,"_",format(Sys.time(), "%Y%b.%d.%H%M%S"),".csv", sep = ""))

###Declare Params unless coming from source
#(lapply(path@list, function(x) mat.db(moo, x)))

NEcount <- Ecount <- Ncount <- new('Stack')

t.names <- new('Stack')
isolateTrees(t.names)

ptm <- proc.time()
curr_Tree <- ""
c <- 1
makeDftall()

invisible(lapply(t.names@list, function(x) {		
	message(paste("Number of trees left:", size(t.names)))
	
	moo <<- tree <<- read.tree(first(t.names))
	curr_Tree <<- first(t.names)
	tree$tip.label <<- trim.names(tree)
	
	message(paste("tSR with tree = ", as.vector(first(t.names))))

	total_Taxa <<- totalInTree(targrp1, targrp2, tree)
	
	message(paste("Total targets in tree: ", total_Taxa))
	
	if(numTG1 > 0 && numTG2 > 0){
		u <<- ""
		
		message("GO: Tree contains both target groups, proceeding with tSR...")
		
		tSR(targrp1, targrp2, tree)
		
		if(isEmpty(path) == "F"){
			
			dft <<- data.frame(	name=curr_Tree, 
					path.node=0, 
					path.btv = 0, 
					trgp.clade = 0,
					spec.clade = 0,
					total.tree = total_Taxa,
					type.phylo = 0,
					query.propTT = 0, 
					query.prop = 0, 
					pass=1:size(path))
			
			i <<- 1
			
			while(i <= size(path)){
				dft$path.node[i] = unlist(path@list[i])
				dft$path.btv[i] = getBoot(unlist(path@list[i]))
				
				dft$query.propTT[i] = excPerTT(unlist(path@list[i]))
				dft$query.prop[i] = excPerQP(unlist(path@list[i]))
				
				dft$pass[i] = unlist(cg.path@list[i])
								
				i <<- i + 1
			}
			#print(paste("NE Bootstrap: ", getBoot(first(path))))
			dft$trgp.clade = unlist(lapply(dft$path.node, FUN= function(x) targetGroupInClade(as.numeric(x))))
			dft$spec.clade = unlist(lapply(dft$path.node, FUN= function(x) speciesInClade(as.numeric(x))))
			dft.all <<- rbind(dft.all, dft)
		}
		
#		k <- subset(dft.all, name == curr_Tree)
#		lapply(k$path.node, function(x,y) mat.db(tree, x), print(y))
		
		#subset(b, path.btv >= mBT && query.propTT >= (qPrTT/100) && query.prop >= (qPr/100))
		#while(q <= length(w$path.node)){
		#	if(w$path.btv[q] >= mBT && w$query.propTT[q] >= (qPrTT/100) && w$query.prop[q] >= (qPr/100)){
		#w <- subset(dft.all, name == curr_Tree)
		q <- 1

		w <- subset(dft, as.numeric(path.btv) >= mBT & as.numeric(query.propTT) >= (qPrTT/100) & as.numeric(query.prop) >= (qPr/100))
		if(nrow(w) > 0){
			while(q <= nrow(w)){	
			cat(paste("\n - - - ", "\n"))
			print((w[q,]))
			print(mat.db(tree, w$path.node[q]))
					cat("\n")	
				if(w$pass[q] == "E"){
					Ecount <<- push(Ecount, curr_Tree)
				}else{
					NEcount <<- push(NEcount, curr_Tree)
				}
				q <- q + 1
			}
		}else{
			Ncount <<- push(Ncount, curr_Tree)
		}
		
		
		#NEcount <<- push(first(t.names))
		t.names <<- pop(t.names)
	#	print("DFT = ")
	#	print(dft)
		
		
	}else{		
		message("DNE: Tree doesn't contain both target groups, skipping to next tree...")
		dft <<- data.frame(name=first(t.names), path.node="N", path.btv = "N", query.propTT = "N", query.prop = "N", pass="N")

		Ncount <<- push(Ncount, curr_Tree)
		
		t.names <<- pop(t.names)
	}
	cat("\n--- - - - - - - - ---\n")
}))


print("end")
#print(paste("Count = ", count))


#Print where each tree goes

#sink()
#subset(dft.all, name == "Glaucophyta-Cyanophora_paradoxa_dxRU9947.gp.tre")

print("Summary")
message(paste("E trees found = ", length(unique(Ecount@list))))
message(paste("NE trees found = ", length(unique(NEcount@list[NEcount@list != Ecount@list]))))
message(paste("N trees found = ", length(unique(Ncount@list))))

#sink()

proc.time() - ptm
write.csv(dft.all, row.names = FALSE, file = paste("tSR_",targrp1,"-",targrp2,"_",format(Sys.time(), "%Y%b.%d.%H%M%S")
,".csv", sep = ""), append = FALSE, quote=FALSE)


