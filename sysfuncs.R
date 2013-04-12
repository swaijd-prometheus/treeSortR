# TODO: Add comment
# 
# Author: dashte
###############################################################################
#!/bin/Rscript


readParams <- function(args){
	u <- 1
	while(u <= length(args)){
		if(substr(args[u], 1, 1) == "-"){
			#other function that says if this starts with this letter then do this
			funcBot(args, u)
			#print(args[u+1])
			u <- u + 2
		}else{
			u <- u + 1
		}
	}
}

funcBot <- function(args, u){
	let = substr(args[u], 2, 3)
	
	if(let == "i"){
		ifelse(nchar(args[u+1]) > 1, tgrps1(args[u+1]), stop("Blank entry for target group 1"))
		
	}else if(let == "j"){
		ifelse(nchar(args[u+1]) > 1, tgrps2(args[u+1]), stop("Blank entry for target group 2"))		
	
	}else if(let == "e"){
		ifelse(nchar(args[u+1]) > 1, minBoostrapThreshold(args[u+1]), stop("Blank entry for minimum boostrap threshold"))		

	}else if(let == "f"){
		ifelse(nchar(args[u+1]) > 1, queryProp(args[u+1]), stop("Blank entry for query proportion"))		

	}else if(let == "g"){
		ifelse(nchar(args[u+1]) > 1, queryPropTT(args[u+1]), stop("Blank entry for queryTT proportion"))		
		
	}else if(let == "d"){
		ifelse(nchar(args[u+1]) > 1, targetDir(args[u+1]), stop("Blank entry for directory"))
	}
}

#http://stackoverflow.com/questions/12000685/write-access-to-commandargs

#1. check if it exists
#if it does, pick a different name OR warn and exit
#if it doesn't, go ahead and make it
#			2. change its permissions appropriaetly (if it's even necessary)

#Make directories
#dir.create
