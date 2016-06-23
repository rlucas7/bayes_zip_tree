#
#
#
#
#==============================================


library(tree)
example(tree)
str(ir.tr)

## function to implement CGMs grow function

grow <- function(arg1=init_tree, data_nm=data_frame){
	
	split_rule_result <- split_rule(arg1=arg1, data_nm=data_nm)
	
	## NOW SPLIT VALUE IS THE VALUE RETURNED structure is 
	# list -> ()
	
	
	
	#update_tree <- function(arg1, rule, operation=c('g','p', 'c','s')){
		# note list 'g','p', 'c','s' are the operations from CGM 1998 JASA paper
		
		
	split_rule_result
	# now we need to update the tree with the new split rule: 
	frame_rownum <- match(split_rule_result[3], rownames(arg1$frame) )
	### NOTE: WILL NEED TO ADD A CHECK THAT THE FACTOR LEVEL 
	### IS ALREADY IN  THE FRAME 
	
	### add the var 
	arg1$frame[ frame_rownum, ]$var <- split_rule_result[2]
	### add the  splits.cutleft 
	arg1$frame[ frame_rownum, ]$splits[1] <- paste("<", split_rule_result[1], sep='', colapse='')
	### add the  splits.cutright
	arg1$frame[ frame_rownum, ]$splits[2] <- paste(">", split_rule_result[1], sep='', colapse='')
	#### NEED TO COME TO A DECISION ON THE YVAL TO ENTER INTO THE FRAME
	
	### now to add in the two resulting child nodes 
	lchild <- arg1$frame[ frame_rownum, ]
	rchild <- arg1$frame[ frame_rownum, ]
	
	#### make the var-s leaf 
	rchild$var <- '<leaf>'
	lchild$var <- '<leaf>'	

	### make the splits empty 
	rchild$splits[1:2] <- ''
	lchild$splits[1:2] <- ''
	
	### now figure out how many obs go left and right 
	#left child
	lchild$n <- sum(data_nm[arg1$where==frame_rownum,split_rule_result[2]] < split_rule_result[1])
	#right child
	rchild$n <- arg1$frame[ frame_rownum, ]$n - lchild$n
	
	### fix up the node numbering 
	rownames(lchild) <- as.character(as.numeric(rownames(lchild))*2)
	rownames(rchild) <- as.character(as.numeric(rownames(rchild))*2 +1)

	### fix the tree_object$where elements (move them down to new child nodes)
	logic_vec <- data_nm[arg1$where==frame_rownum,split_rule_result[2]] < split_rule_result[1]
	
	#first shift all those entries > frame_rownum (+2) to account for added rows
	
	arg1$where[arg1$where>frame_rownum] <- arg1$where[arg1$where>frame_rownum] + 2
	
	# observations going to the left node
	arg1$where[arg1$where==frame_rownum][logic_vec] <- rep(frame_rownum + 1, sum(logic_vec) )
	
	#observations going to the right node
	arg1$where[arg1$where==frame_rownum] <- rep(frame_rownum + 2, sum(!logic_vec) )
	
	# now update the tree_object$frame to include above, lchild, rchild, beneath
	top    <- arg1$frame[1:frame_rownum,]
	if(frame_rownum < dim(arg1$frame)[1] ){
		bottom <- arg1$frame[(frame_rownum+1):dim(arg1$frame)[1],]
		arg1$frame <- data.frame(rbind(top, lchild, rchild, bottom) )
	}else{
		#case when the grow is on the last node in the tree_object$frame
		
		#there is no bottom component :) 
		arg1$frame <- data.frame(rbind(top, lchild, rchild) )
		
	} 
	
	
	
	#### now return the new tree!!! yay!!!
	arg1
	}