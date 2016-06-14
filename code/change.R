#
#
#
#
# code to implemt the change function for the proposal in CGM x
#
#=================================================================

change <- function(arg1=ir.tr, data_nm=iris){
	
	#####NOTE: this function has a potential infinite loop...
	
	
	#get all the internal nodes and choose one randomly 
	inter_node_list <- get_int_nodes(arg1)
	sel_inter_node  <- sample(inter_node_list,1) 
	
	# now determine what are the values you may sample from to propose a changed value, do this using an accept reject proposal, not efficient but gets the job done for now...
	factors <- dimnames(attr(arg1$terms, 'factors'))[[2]]
	factor_data <- data_nm[factors]
	
	bflag <- TRUE
	while(bflag){
	# now randomly select covariate and split value 
	column <- sample(1:length(factors),1)
	split_factor_vals <- factor_data[,column]
	split_val <- sample(split_factor_vals,1)
	
	#now update the tree with the new split rule
	node_rownum <- match(sel_inter_node, rownames(arg1$frame))
	#update var
	arg1$frame[node_rownum,]$var <- factors[column]
	#update splits 
	#LHS split
	arg1$frame[node_rownum,5][1]<- paste("<",as.character(split_val), sep='',collapse='')
	#RHS split
	arg1$frame[node_rownum,5][2]<- paste(">",as.character(split_val), sep='',collapse='')
	where_obs_tnodes <- predict(arg1,new_data=data_nm,type='where')
	enough_obs <- sum(table(where_obs_tnodes)>4)
	if(enough_obs == length(unique(where_obs_tnodes))){
		#then all internal nodes have enough observations
		bflag <- FALSE
	}#else do nothing and repeat the while loop
	
	#update the where list in the tree
	arg1$where <- where_obs_tnodes
	#update the n column in the tree$frame

	#update the deviance column in the tree$frame
	
	#update the response in the tree$frame	
	
	}#end of while loop
	arg1
}



#simple beta test

c1 <- change(arg1=ir.tr, data_nm=iris)

p2t(c1,ir.tr)