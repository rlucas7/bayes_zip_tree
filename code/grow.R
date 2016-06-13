#
#
#
#
#==============================================


library(tree)
example(tree)
str(ir.tr)

## function to implement CGMs grow function

grow <- function(arg1=ir.tr, data_nm=iris){
	
	split_rule_result <- split_rule(arg1=ir.tr, data_nm=ur_data)
	
	## NOW SPLIT VALUE IS THE VALUE RETURNED structure is 
	# list -> ()
	
	# now we need to update the tree with the new split rule: 
	
	### CAN we chunk this piece out into its' own function as well? 
	### NOTE1: requires tree object, and split rule
	
	update_tree <- function(arg1, rule, operation=c('g','p', 'c','s')){
		# note list 'g','p', 'c','s' are the operations from CGM 1998 JASA paper
		
		
		
		
	}
	
	
	
	
	
	
}