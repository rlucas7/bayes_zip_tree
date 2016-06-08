### CAN WE CHUNK THIS BIT OF CODE INTO A DEPENDENT FUNCTION??
	### NOTE1: THIS IS WORKING FOR NUMERIC, ALSO NEED FACTOR...
	### NOTE2: THE WHILE LOOP IS INFINITE IF <3 OBS ... (REALLY?)
	
split_rule <- function(arg1=ir.tr, data_nm=ur_data){
	#first get all the terminal nodes 
	t_nodes <- get_term_nodes(arg1)
	# next pick one at random 
	selected_node <- sample(t_nodes, 1)
	# now we want to get all the observations within selected node
	tnode_obs <- get_term_node_nums(arg1)
	#subset the data to the factors in the tree(if needed)
	factors <- dimnames(attr(arg1$terms, 'factors'))[[2]]
	factor_data <- data_nm[factors]
	# logic vector of which rows to sample from
	subset_data <- factor_data[tnode_obs == selected_node,]
	#select the splitting factor
	column <- sample(1:length(factors),1)
	split_factor_vals <- subset_data[,column]
	### code differs if the are categorical vs numeric
	bflag <- TRUE
	if(is.numeric(split_factor_vals)){		
		while(bflag ==TRUE){
		split_val <- split_factor_vals[sample(1:length(split_factor_vals),1)]
		# now check if split val has obs in the two new leaf nodes
		stop_criteria <- sum(1*(split_factor_vals < split_val)) < length(split_factor_vals)
		# if stop criteria == TRUE stop else, repeat 
		if(stop_criteria == TRUE){bflag <- FALSE}
		}
	}else{
			 #### =======factor is categorical
			 ##
			 ####=======factor is categorical
		while(bflag ==TRUE){
			len <- length(unique(as.numeric(iris$Species)))
			#create binary vector for subsetting 
			bool_vec <- matrix(0,nrow=1, ncol=len)
			bbflag <- TRUE
			while( bbflag == TRUE){ 
				bool_vec <- rbinom(len, 1,0.5)
				if(sum(bool_vec) > 0 || sum(bool_vec < len)){bbflag <- FALSE}
				}
		#now use the Boolean vector to subset factor levels
		set_A <- levels(iris$Species)[as.logical(bool_vec)]
		set_A_complement <- levels(iris$Species)[as.logical(!bool_vec)]
		# now check if split val has obs in the two new leaf nodes
		stop_criteria <- sum(1*(set_A < split_val)) < length(split_factor_vals)
		# if stop criteria == TRUE stop else, repeat 
		if(stop_criteria == TRUE){bflag <- FALSE}	
			
			}
	}
	ret_obj <- c(split_val, factors[column], selected_node)
	names(ret_obj) <- c('split_value','factor_name', 'node_number')
	ret_obj
}


#simple beta test(s)

split_rule(arg1=ir.tr, data_nm=iris)
# returns something like: 
#split_value   factor_name   node_number 
#          "3" "Sepal.Width"          "14" 