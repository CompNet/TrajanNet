#############################################################################################
# Modified versions of functions from the signnet package.
# 
# Original code source by David Schoch, 2019
# https://github.com/schochastics/signnet
#############################################################################################




#############################################################################################
# Returns the triangles present in the signed graph, with their class.
#
# sg: signed graph.
#
# returns: matrix with triangles on the rows, with the node ids on the first 3 columns and
# 		   the triangle type on the fourth one: 0=---, 1=-++, 2=--+, 3=+++.
#############################################################################################
get_signed_triangles <- function(sg)
{	# preliminary verifications
	if(!"sign" %in% igraph::edge_attr_names(sg))
	{	stop("network does not have a sign edge attribute")
	}
	if(igraph::is.directed(sg))
	{	stop("g must be undirected")
	}
	
	# get the attribute representing link signs
	eattrV <- igraph::get.edge.attribute(sg,"sign")
	# check that their values are -1 or +1
	if(!all(eattrV%in%c(-1,1)))
	{	stop("sign may only contain -1 and 1")
	}
	
	# get all triangles in the graph as a 3-column matrix
	tmat <- t(matrix(igraph::triangles(sg),nrow=3))
	
	# if there is no triangle at all
	if(nrow(tmat)==0)
	{	result <- matrix(ncol=4,nrow=0)
	}
	# if there are triangles
	else
	{	# get the link ids of the triangles
		emat <- t(apply(tmat,1,function(x) c(
					igraph::get.edge.ids(sg,x[1:2]),
					igraph::get.edge.ids(sg,x[2:3]),
					igraph::get.edge.ids(sg,x[c(3,1)])
				)))
		# get the signs of these links
		emat[,1] <- eattrV[emat[,1]]
		emat[,2] <- eattrV[emat[,2]]
		emat[,3] <- eattrV[emat[,3]]
		# identify the class of each triangle
		cls <- apply(emat, 1, function(v) length(which(v==1)))
		result <- cbind(tmat,cls)
	}
	
	colnames(result) <- c("V1","V2","V3","Class")
	return(result)
}
