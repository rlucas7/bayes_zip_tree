#
#
#
#
#
#



p2t <- function(tree1, tree2){
	par(mfcol=c(1,2))
	plot(tree1,type='unif')
	text(tree1)
	plot(tree2,type='unif')	
	text(tree2)
}