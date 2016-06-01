####
####
####
####
####
####
####
####=========================================


# get internal nodes for any tree. 

# depends on get_term_nodes function 


get_int_nodes <- function(arg1=tree_arg){
	#first step is get all terminal nodes
	all_nodes <- rownames(arg1$frame)
	n <- length(all_nodes)
	node_num<-as.numeric(all_nodes)
	int_nodes <- c()
	for(i in 1:n){
		
		left_child  <- match(2*node_num[i], node_num)
		right_child <- match(2*node_num[i]+1, node_num)
		if(is.na(left_child) || is.na(right_child) ){
		# do nothing because nodes are terminal 	
		}else{
			# internal nodes thus append 
			int_nodes <- append(int_nodes, node_num[i], after = length(int_nodes))			
			}		
	}
	int_nodes	
}


# now run a beta test

library(tree)
example(tree)

get_int_nodes(ir.tr)
#should return 1, 3, 6, 12, 7