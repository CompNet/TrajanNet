#############################################################################################
# Functions used during network extraction.
# 
# 01/2019 Vincent Labatut
#
# setwd("C:/users/Vincent/Eclipse/workspaces/Networks/TrajanNet")
# setwd("~/eclipse/workspaces/Networks/TrajanNet")
# source("src/extraction.R")
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
#
# returns: the updated graph.
#############################################################
split.attribute.by.order <- function(g, values, name)
{	# break down the attribute values
	atts <- strsplit(x=values, split=";")
	
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
#
# returns: the updated graph.
#############################################################
split.attribute.by.value <- function(g, values, name, nodes)
{	# break down the attribute values
	atts <- strsplit(x=values, split=";")
	
	# determine the number of unique values
	u.vals <- sort(unique(unlist(atts)))
	nval <- length(u.vals)
	
	# add them to the graph as nodal or edge attributes
	for(u.val in u.vals)
	{	if(nval>1)
			nm <- paste0(name,"_",u.val)
		else
			nm <- name
		if(nodes)
		{	g <- set_vertex_attr(graph=g, name=nm, 
				value=sapply(atts,function(a)
						{	if(all(is.na(a)))
								res <- NA
							else if(u.val %in% a)
								res <- "TRUE"
							else
								res <- "FALSE"
							return(res)
						}))
		}
		else
		{	g <- set_edge_attr(graph=g, name=nm, 
				value=sapply(atts,function(a)
						{	if(all(is.na(a)))
								res <- NA
							else if(u.val %in% a)
								res <- "TRUE"
							else
								res <- "FALSE"
							return(res)
						}))
		}
	}
	
	return(g)
}




#############################################################
# Reads the previously extracted graph, converting attributes
# types as needed.
#
# returns: the loaded graph.
#############################################################
read.network <- function()
{	# read the graph
	g <- read.graph(graph.file,format="graphml")
	
	# converts vertex attribute
	for(att in vertex_attr_names(g))
	{	# boolean attributes
		if(att %in% c("Adelectio","SoutHadrien","Espagnol","Cercles_Antonins","Cercles_Nigrinus","Cercles_Pline","Cercles_Regulus","Cercles_Septicius Clarus"))
		{	x <- suppressWarnings(as.logical(vertex_attr(g,att)))
			g <- delete_vertex_attr(g,att)
			g <- set_vertex_attr(g,att,value=x)
		}
		# integer attributes
		else if(att %in% c("NbrVoy"))
		{	x <- suppressWarnings(as.integer(vertex_attr(g,att)))
			g <- delete_vertex_attr(g,att)
			g <- set_vertex_attr(g,att,value=x)
		}
		# other attributes
		else
		{	x <- vertex_attr(g,att)
			x[which(x=="NA")] <- NA
			g <- suppressWarnings(delete_vertex_attr(g,att))
			g <- set_vertex_attr(g,att,value=x)
		}
	}
	
	# convert edge attributes
	for(att in edge_attr_names(g))
	{	# boolean attributes
		if(att %in% c("Nature_Amicale","Nature_Familiale","Nature_Professionnelle","Polarite"))
		{	x <- suppressWarnings(as.logical(edge_attr(g,att)))
			g <- delete_edge_attr(g,att)
			g <- set_edge_attr(g,att,value=x)
		}
#		# integer attributes
#		else if(att %in% c())
#		{	x <- suppressWarnings(as.integer(edge_attr(g,att)))
#			g <- delete_edge_attr(g,att)
#			g <- set_edge_attr(g,att,value=x)
#		}
#		# other attributes
#		else
#		{	x <- edge_attr(g,att)
#			x[which(x=="NA")] <- NA
#			g <- delete_edge_attr(g,att)
#			g <- set_edge_attr(g,att,value=x)
#		}
	}
	
	return(g)
}




#############################################################
# Reads both individual attribute and relationship tables,
# clean the obtained values, and extract the network.
# The network is then recorded.
#
# returns: the extracted graph.
#############################################################
extract.network <- function()
{	############
	cat("Extracting nodes and their information\n")
	# load node attributes
	attr.file <- file.path(TABLE_FOLDER,"trajan_attributes.csv")
	attr.data <- as.matrix(read.csv(file=attr.file,header=TRUE,check.names=FALSE))
	
	# create empty graph
	g <- graph.empty(n=nrow(attr.data),directed=FALSE)
	
	# add personal node attributes 
	V(g)$name <- attr.data[,"Id"]
	V(g)$label <- attr.data[,"Nom"]
	
	# add chronological (or single) nominal node attributes
	att.names <- c("PolitSenat", "DerPolitSenat","PolitEques","DerPolitEques","MilitSenat","MilitEques","DestVoy","MotifVoy","RelTrajan")
	for(att.name in att.names)
	{	cat("  Processing chronological nominal node attribute ",att.name,"\n",sep="")
		g <- split.attribute.by.order(g, attr.data[,att.name], name=att.name)
	}
	
	# add multiple nominal attributes
	att.names <- c("Cercles")
	for(att.name in att.names)
	{	cat("  Processing multiple nominal node attribute ",att.name,"\n",sep="")
		g <- split.attribute.by.value(g, attr.data[,att.name], name=att.name, nodes=TRUE)
	}
	
	# add single boolean attributes (as strings, to handle NAs)
	att.names <- c("Adelectio","SoutHadrien","Espagnol")
	for(att.name in att.names)
	{	cat("  Processing single boolean node attribute ",att.name,"\n",sep="")
		vals <- attr.data[,att.name]
		vals[vals=="Oui"] <- "TRUE"
		vals[vals=="Non"] <- "FALSE"
		g <- set_vertex_attr(graph=g, name=att.name, 
#				value=attr.data[,att.name]=="Oui")
				value=vals)
	}
	
	# add single numerical attributes (as strings, to handle NAs) 
	att.names <- c("NbrVoy")
	for(att.name in att.names)
	{	cat("  Processing single numerical node attribute ",att.name,"\n",sep="")
		g <- set_vertex_attr(graph=g, name=att.name, 
#			value=as.integer(attr.data[,att.name]))
			value=attr.data[,att.name])
}
	
#	m <- sapply(list.vertex.attributes(g),function(str) get.vertex.attribute(g,str))
#	print(m)
#	write.csv(x=m,file=file.path(TABLE_FOLDER,"verification.csv"))
	
	
	############
	cat("Adding links and their information\n")
	# load relations
	rel.file <- file.path(TABLE_FOLDER,"trajan_relations.csv")
	rel.data <- as.matrix(read.csv(file=rel.file,header=TRUE,check.names=FALSE))
	
	# add links
	g <- add_edges(g, edges=c(t(rel.data[,c("Id1","Id2")])))
	
	# add multiple nominal attributes
	att.names <- c("Nature")
	for(att.name in att.names)
	{	cat("  Processing multiple nominal link attribute ",att.name,"\n",sep="")
		g <- split.attribute.by.value(g, rel.data[,att.name], name=att.name, nodes=FALSE)
	}
	
	# add single nominal attributes
	att.names <- c("Polarite")
	for(att.name in att.names)
	{	cat("  Processing single nominal link attribute ",att.name,"\n",sep="")
		vals <- rel.data[,att.name]
		vals[vals=="Positive"] <- "TRUE"
		vals[vals=="Négative"] <- "FALSE"
		g <- set_edge_attr(graph=g, name=att.name, 
#				value=rel.data[,att.name])
				value=vals)
	}
	
	
	############
	# record graph
	graph.file <- file.path(NET_FOLDER,"all.graphml")
	cat("Recording graph in",graph.file,"\n")
	write.graph(graph=g,file=graph.file,format="graphml")
	
	
	return(g)
}




#sapply(list.vertex.attributes(g2),function(str) get.vertex.attribute(g2,str))
