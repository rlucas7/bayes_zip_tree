#
#	this function implements the prune function
#	from CGMs paper . 
#
#
#
#===================================================


prune <- function(arg1=ir.tr){
	
	t_node_list   <- get_term_nodes(arg1)
	bflag<-TRUE
	while(bflag){
		select_t_node <- sample(t_node_list, 1)
	
		# now check if t_node's sibling is also terminal 
		# if not then we need to look at another node...
		node_nums <- as.numeric(rownames(ir.tr$frame))
	
		if(select_t_node %% 2 == 0){ 
			# this is executed if node is left child	
			sibling_node_num<- 2*floor(select_t_node/2)+1			
		}else{
			 #check if sibling (right) node is leaf
			sibling_node_num<- 2*floor(select_t_node/2)
		}
		
			#check if sibling (right) node is leaf
			row_ind <- match(sibling_node_num, node_nums)
			sibling_leaf_ind <- ir.tr$frame[row_ind,1] =='<leaf>'
		if(sibling_leaf_ind){
			# proceed with pruning
			bflag <- FALSE
		}#else do nothing so loop repeats
	}
	# now we have the terminal node to prune, select_t_node
	
	
	#to prune we need to determine the two rows to remove from the tree$frame
	# and then we need to remove and merge the remaining tables 
	
	# ALSO, we need to update the info in the tree$frame to reflect 
	# the parent node is now a leaf node


}