#
#
#
#
#
#
#
#=====================


swap <- function(arg1=ir.tr){
	
	#randomly select an internal node
	inter_node_list <- get_int_nodes(arg1)
	
	#now pass through the inter_node_list to see which are eligible for swap move
	len <-length(inter_node_list)	
	nodes_to_sample_from <- c()
	for(i in 1:len){
		
		left_child  <- 2*inter_node_list[i]
		right_child <- 2*inter_node_list[i] +1
		rname  <- rownames(arg1$frame)
		lc_row <- match(left_child, rname)
		rc_row <- match(right_child, rname)
		rc_logic <- arg1$frame$var[rc_row]=='<leaf>'
		lc_logic <- arg1$frame$var[lc_row]=='<leaf>'
		if(rc_logic && lc_logic){
			print('both child nodes are leaf...')
			#do not append
			}else{
			nodes_to_sample_from <- 	append(inter_node_list[i],nodes_to_sample_from,length(nodes_to_sample_from) )
			}
	}
	#now randomly select a node eligible for a swap move
	sel_inter_node  <- sample(nodes_to_sample_from,1) 
	
	# now check if both child nodes split on the same variable
	left_child  <- 2*sel_inter_node
	right_child <- 2*sel_inter_node +1
	rname  <- rownames(arg1$frame)
	lc_row <- match(left_child, rname)
	rc_row <- match(right_child, rname)
	joint_swap_logic <- arg1$frame$var[rc_row] == arg1$frame$var[lc_row]
	
	if(joint_swap_logic){
		
		#both split rules are the same so swap both with parent
		
		
		
		
	}else{
		#both split rules aren't the same, only swap with one child (which?)
		rc_logic <- arg1$frame$var[rc_row] =='<leaf>'
		lc_logic <- arg1$frame$var[lc_row] =='<leaf>'		
		if(!rc_logic && !lc_logic){ 
			rbin<-rbinom(1,1,0.5) 
			if(rbin==0){ 
				#randomly choose the left node 
				
				}else{
				# randomly choose right node	
				
				}
			
			}else{
				# in this case only one of the two child nodes is non-terminal so choose that one
			
				
			}
		
		
		
	}# end of ifelse(joint_swap_logic)


}# end of function