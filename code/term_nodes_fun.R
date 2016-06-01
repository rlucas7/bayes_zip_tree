####
####
####
####
####
####
####
####=========================================


# get terminal nodes for any tree. 

get_term_nodes <- function(arg1=tree_arg){
#first step is get all terminal nodes
	all_nodes <- rownames(arg1$frame)
	n <- length(all_nodes)
	node_num<-as.numeric(all_nodes)
	term_nodes <- c()
	for(i in 1:n){
		
		left_child  <- match(2*node_num[i], node_num)
		right_child <- match(2*node_num[i]+1, node_num)
		if(is.na(left_child) || is.na(right_child) ){
			#one or the other child nodes is null
			#so node_num[i] is a terminal node
			# b/c all d-trees are complete 
			term_nodes <- append(term_nodes, node_num[i], after = length(term_nodes))
		
		}#else do nothing, node isn't terminal
		
	}
	term_nodes
}	

# beta test of the code 

library(tree)
example(tree)

get_term_nodes(ir.tr)
#should return 2, 24, 25, 13, 14, 15


