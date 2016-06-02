####
####
####
####
####
####
####
####=========================================

# get terminal nodes for any tree. 

get_term_node_nums <- function(arg1=tree_arg){
#first step is get all terminal nodes
	term_nodes_obs <- arg1$where
	table_obs <- table(term_nodes_obs)
	tree_frame_row_num<-as.numeric(names(table_obs))
	term_node_nums <- rownames(arg1$frame[tree_frame_row_num,])
 
 # make a table to match table rownumbers to node numbers
	match_table<- matrix(0,ncol=length(table_obs), nrow=2)
	match_table[1,] <- tree_frame_row_num
	match_table[2,] <- as.numeric(term_node_nums)
	index <- match(term_nodes_obs, match_table[1,])
	term_nodes <- match_table[2,index]
	term_nodes
}	

# beta test of the code 

library(tree)
example(tree)

get_term_node_nums(ir.tr)
#should return 2,2,2,2,2,2,2,.... ,15,15,15,15
