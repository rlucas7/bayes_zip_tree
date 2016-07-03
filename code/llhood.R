#
#
#
#
#
#
#
# =========================================


# function to help in llhood_calc for log sum across j=1:n_0
log_sigma <- function(n_0, a=1,b=1, al=1, bl=1){
j <- 1:n_0

TO_SUM <- choose(n_0, j) * gamma(a + j) * gamma(n_0 + b + j) * ( (n_0 - j + 1 / bl ) / bl )^ al
		
	SUM <- sum(TO_SUM)*beta(a=a,b=b)
	SUM
	
}






llhood_calc <- function(arg1=ir.tr, data_nm =ir.tr, a=1,b=1, al=1, bl=1){
	
	### first get all the terminal nodes 
	tnode_nobs_table <- table(arg1$where)
	len <- length(tnode_nobs_table)
	n_0 <- matrix(0.0, nrow=len, ncol=1)
	y_plus <- matrix(0.0, nrow=len, ncol=1)
	pytx <- matrix(0.0, nrow=len, ncol=1)
	### now get all the terminal node stats
	for(i in 1:len){
		i_node <- as.numeric(names(tnode_nobs_table))[i]
		n_0[i] <- sum( arg1$y[arg1$where == i_node] == 0 )
		y_plus[i] <- sum( arg1$y[arg1$y[arg1$where == i_node] != 0 ])
		n_plus <- tnode_nobs_table[i] - n_0[i]
		
		A <- ( gamma(al + y_plus[i]) * (n_plus + al)^(al + y_plus[i]) )
		B <- prod( factorial(arg1$y[arg1$y[arg1$where == i_node] != 0 ]) )
		pytx[i] <- A/B + n_0[i]/2 + 1
	}# end of for(i in 1:len){} loop
	sum( log(pytx) )
}

