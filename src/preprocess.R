#############################################################################################
# Auxiliary functions used during network extraction.
# 
# 01/2019 Vincent Labatut
#
# setwd("D:/Eclipse/workspaces/Networks/NetVotes")
# setwd("~/eclipse/workspaces/Networks/NetVotes")
# source("src/main.R")
#############################################################################################

#############################################################
# Receives a vector of attribute values (one per node), some
# of which are actually sequences of semicolon-separated values.
# This method splits them appropriately, and defines (in the graph)
# as many new nodal attributes as the maximal multiplicity met.
# 
# Ex. if one node has attribute value ATT="x,y,z" and all others have 
# ATT="a,b" then three new nodal attributes are created, with ATT1="x"
# ATT2="y" ATT3="z" for the first node, and ATT1="a" ATT2="b" ATT3=NA
# for the others.
#
# g: original graph.
# values: vector of strings, each string representing the attribute(s)
#         values for a node.
# name: name of the new nodal attribute.
#############################################################
split.attribute.by.order <- function(g, values, name)
{	# break down the attribute values
	atts <- strsplit(x=values, split=";")
#	atts <- lapply(X=atts, function(vect) 
#		{	if(all(vect=="NA"))
#				res <- NA
#			else
#				res <- vect
#			return(res)
#		})
	
	# determine the max multiplicity of the attribute
	nval <- max(sapply(atts,length))
	
	# add them to the graph as nodal attributes
	for(i in 1:nval)
	{	if(nval>1)
			nm <- paste0(name,i)
		else
			nm <- name
		g <- set_vertex_attr(graph=g, name=nm, 
				value=sapply(atts,function(a)
					{	if(length(a)>=i)
							res <- a[i]
						else
							res <- NA
#						if(!is.na(res))
#						{	if(res=="Non")
#								res <- FALSE
#							else if(res=="Oui")
#								res <- TRUE
#						}
						return(res)
					}))
	}
	
	return(g)
}


#############################################################
# Receives a vector of attribute values (one per node), some
# of which are actually sequences of semicolon-separated values.
# This method splits them appropriately, and defines (in the graph)
# as many new nodal attributes as the number of unique values met.
# 
# Ex. if one node has attribute value ATT="x,y,z" and all others have 
# ATT="a,b" then five new nodal attributes are created, with ATT_a=FALSE
# ATT_b=FALSE ATT_x=TRUE ATT_y=TRUE ATT_z=TRUE for the first node, and 
# ATT_a=TRUE ATT_b=TRUE ATT_x=FALSE ATT_y=FALSE ATT_z=FALSE for the others.
#
# g: original graph.
# values: vector of strings, each string representing the attribute(s)
#         values for a node.
# name: name of the new nodal attribute.
#############################################################
split.attribute.by.value <- function(g, values, name)
{	# break down the attribute values
	atts <- strsplit(x=values, split=";")
	
	# determine the number of unique values
	u.vals <- sort(unique(unlist(atts)))
	nval <- length(u.vals)
	
	# add them to the graph as nodal attributes
	for(u.val in u.vals)
	{	if(nval>1)
			nm <- paste0(name,"_",u.val)
		else
			nm <- name
		g <- set_vertex_attr(graph=g, name=nm, 
				value=sapply(atts,function(a)
						{	res <- u.val %in% a
							return(res)
						}))
	}
	
	return(g)
}




#sapply(list.vertex.attributes(g2),function(str) get.vertex.attribute(g2,str))
