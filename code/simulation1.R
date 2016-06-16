#
#
#
#	this is code to simulate the 
# 	Markov Chain Monte Carlo sampler 
#	for the Bayes zip tree
#
#
#======================================================


### first simulate data 

n <- 100
mu_vals <- c(-2,2)

binom_rv <- rbinom(n,1,0.5)

x1 <- rnorm(n, mean = mu_vals[binom_rv+1])
x2 <- sample(c('A','B', 'C'), n, replace=TRUE)


response_sample <- function(x1,x2){ 
	
	len <- length(x1)
	response_list <- matrix(0, nrow=len, ncol=1)
	if(len != length(x2)){stop('x1 is not the same length as x2, what are you doing silly?')} 
for(i in 1:len){
	if(x1 < 0 && x2 =='A'){
		z <- rbinom(1,1,0.5)
		response_list[i] <- rpois(1, lambda=2)*z + (1-z)*0 
	}
	if(x1 < 0 && x2 !='A'){
		z <- rbinom(1,1,0.5)
		response_list[i] <- rpois(1, lambda=5)*z + (1-z)*0 
	}
	if(x2 > 0 && x2 == 'A'){
		z <- rbinom(1,1,0.5)
		response_list[i] <- rpois(1, lambda=9)*z + (1-z)*0 
	}
	if(x2 > 0 && x2 != 'A'){
		z <- rbinom(1,1,0.5)
		response_list[i] <- rpois(1, lambda=1)*z + (1-z)*0 
	}
}#end of for loop
response_list

}#end of function
y <- response_sample(x1,x2)
y

#make a plot of observed data and overlay a Poisson

hist(y, breaks=25, prob=TRUE)
points(0:25,dpois(0:25, lambda=mean(y))  )
lines(0:25,dpois(0:25, lambda=mean(y))  )

### second setup data structures needed for the MCMC sampler 


### sample the decision trees 


### now calculate the convergence diagnostics 


### now calculate the summary statistics for the output tables in the manuscript
