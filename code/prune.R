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
		node_nums <- as.numeric(rownames(arg1$frame))
	
		if(select_t_node %% 2 == 0){ 
			# this is executed if node is left child	
			sibling_node_num<- 2*floor(select_t_node/2)+1			
		}else{
			 #check if sibling (right) node is leaf
			sibling_node_num<- 2*floor(select_t_node/2)
		}
		
			#check if sibling (right) node is leaf
			row_ind <- match(sibling_node_num, node_nums)
			sibling_leaf_ind <- arg1$frame[row_ind,1] =='<leaf>'
		if(sibling_leaf_ind){
			# proceed with pruning
			bflag <- FALSE
		}#else do nothing so loop repeats
	}
	
	# now we have the terminal node to prune, select_t_node
	parent_node <- floor(select_t_node/2)
	row_ind_node <- match(select_t_node, node_nums)
	new_tree_frame <- arg1$frame[-c(row_ind, row_ind_node),]
	row_ind_parent <- match(parent_node, node_nums)	
	new_tree_frame$var[row_ind_parent] <- '<leaf>'
	arg1$frame <- new_tree_frame
	
	# now update the terminal nodes to parent 
	arg1$where[arg1$where == row_ind_node] <- row_ind_parent
	arg1$where[arg1$where == row_ind] <- row_ind_parent
	
	#return the tree
	arg1


	#NOTE: we most likely will want to use snip.tree here

}