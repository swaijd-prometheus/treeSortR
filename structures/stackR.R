#S4 implementation of a stack (FIFO) data structure
# 
# Author: sana wajid
###############################################################################
require(methods)
setClass('Stack', 
		#Slots new class should have
		representation(list='character'), #cursor='numeric'),  # type defs
		
		#Object providing default data for the slots. 
		#This case initializes an empty list and cursor to NA
		prototype(list=character())) #, cursor= 0))#NA_real_))        # default values


#signature:	Character vector of named arugments to be used for dispatch
#signature:	Named character vector matching argument names to types. Implicit type is ANY, another type is missing

setGeneric('push', function(obj, ...) standardGeneric('push'))
setMethod('push', signature(obj='Stack'), 
		function(obj, x) {
			obj@list <- c(x, obj@list)
			#obj@cursor <- obj@cursor + 1
			obj
		}
)


setGeneric('pop', function(obj, ...) standardGeneric('pop'))
setMethod('pop', signature(obj='Stack'),
		function(obj) {
			#obj@cursor <- obj@list[[1]]
			#obj@cursor <- obj@cursor - 1
			obj@list <- obj@list[-1]
			obj
		}
)

setGeneric('size', function(obj, ...) standardGeneric('size'))
setMethod('size', signature(obj='Stack'),
		function(obj) {
			length(obj@list)
		}
)

setGeneric('isEmpty', function(obj, ...) standardGeneric('isEmpty'))
setMethod('isEmpty', signature(obj='Stack'),
		function(obj) {
			if(size(obj) == 0){
				"T"
			}else{
				"F"
			}
		}
)

setGeneric('first', function(obj, ...) standardGeneric('first'))
setMethod('first', signature(obj='Stack'),
		function(obj) {
			if(isEmpty(obj) == "F"){
				obj@list[[1]]
			}else{
				stop("Empty!")
			}	
		}
)
