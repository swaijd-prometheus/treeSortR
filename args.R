# TODO: Main arguments for incoming command line parameters:
# 
# Author: sana wajid
###############################################################################

# target group A
tgrps1 <- function(taxA){
	targrp1 <<- as.character(taxA)
}

# target group B
tgrps2 <- function(taxB){
	targrp2 <<- as.character(taxB)
}

# minimum bootstrap to filter
minBoostrapThreshold <- function(val){
	val <- as.numeric(val)
	if(val >= 0 && val <= 100){
		mBT <<- val		
	}
}

# query proportion of target tree
queryPropTT <- function(val){
	val <- as.numeric(val)
	if(val >= 0 && val <= 100){
		qPrTT <<- val
	}
}

queryProp <- function(val){
	val <- as.numeric(val)
	if(val >= 0 && val <= 100){
		qPr <<- val
	}
}

targetDir <- function(loc){
	loc <- as.character(loc)
	#makeFol(loc)
	setwd(loc)
}

#http://stackoverflow.com/questions/2151212/how-can-i-read-command-line-parameters-from-an-r-script
#http://stackoverflow.com/questions/5247766/running-an-r-file-with-arguments-from-a-bash-script
