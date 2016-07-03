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

data_frame <- data.frame(y,x1,x2)

# m: the number of MCMC draws within a chain
m <- 10
# p: the number of restarts to the MCMC chain
p <- 1 

# the binary vector (1=MCMC stays at current state on next iteration, 0=go to proposed state)
stay_or_go <- matrix(1,nrow=m*p, ncol=1)

# log-likelihood measure
llhood <- matrix(0.1, nrow=m*p, ncol=1)

# list of sampled trees 
samp_tree_list <- vector(mode='list', length=m*p)

# initial decision tree fit 
library(tree)
init_tree <- tree(y~x1, data=data_frame)
plot(init_tree,type='unif')
text(init_tree)

### sample the decision trees 

for(i in 2:m){
	
	propose_choice <- sample(1:2, 1)
	if(propose_choice == 1){ 
		### grow step proposal 
		proposed_tree <- grow(init_tree, data_nm=data_frame)
		
		}else{
			### this is the prune step proposal
		proposed_tree <- prune(init_tree)	
			
		}
print(proposed_tree)
	### now calculate the log-likelihood of the tree. 

		 prop_llhood <- llhood_calc(proposed_tree, data_nm=data_frame)
		print(prop_llhood)
	### need a function that takes in grow/prune and gives the llhood `correct' terms
	
	### 
	  log_ratio <- llhood[i-1] - prop_llhood
	print(log_ratio)	
	# #### if/else criteria for stay or go in MCMC 
	if(-rexp(1) > log_ratio ){
		# # go! 
		 llhood[i] <- prop_llhood
		 samp_tree_list[[i]] <- proposed_tree
		 stay_or_go[i] <- 0
	}else{
		 #stay!
		 llhood[i] <- llhood[i]		
		 samp_tree_list[[i]] <- samp_tree_list[[i-1]]
		 stay_or_go[i] <- 1
	 }

}# end of for(i in 1:m){} loop


### now calculate the convergence diagnostics 


### now calculate the summary statistics for the output tables in the manuscript
