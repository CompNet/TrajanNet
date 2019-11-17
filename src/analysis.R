#############################################################################################
# Functions used during network analysis.
# 
# 09/2019 Vincent Labatut
#
# setwd("C:/users/Vincent/Eclipse/workspaces/Networks/TrajanNet")
# setwd("~/eclipse/workspaces/Networks/TrajanNet")
# source("src/analysis.R")
#############################################################################################




#############################################################
# Transform the received graph so that instead of having multitype
# links, each type occurrence is represented by a distinct link.
# Parameter link.types lists the types to retain in the result graph.
#
# g : original graph, whose links will be cleaned to get the result graph.
# link.types : list of link types to keep in the result graph.
#
# returns: graph resulting from the cleaning.
#############################################################
clean.links <- function(g, link.types)
{	# remove all links and link attributes 
	res <- delete_edges(g, edges=E(g))
	for(ea in list.edge.attributes(g))
		res <- delete_edge_attr(res, ea)

	# possibly remove NA from list of link types
	link.types0 <- link.types
	idx <- which(is.na(link.types0))
	if(length(idx)>0)
		link.types0 <- link.types0[-idx]
	
	# process each edge
	for(e in E(g))
	{	#print(e)
		uv = ends(g, E(g)[e], names=FALSE)
		
		# process each specified link type (except NA)
		for(lt in link.types0)
		{	# convert attribute values
			vv <- get.edge.attribute(g,lt,e)
			pol <- get.edge.attribute(g,ATT_EDGE_POL,e)
			
			if(!is.na(vv) && vv)
			{	# convert link nature
				if(lt==paste0(ATT_EDGE_NAT,"_",ATT_VAL_FRIEND))
					ltp <- ATT_VAL_FRIEND
				else if(lt==paste0(ATT_EDGE_NAT,"_",ATT_VAL_FAMILY))
					ltp <- ATT_VAL_FAMILY
				else if (lt==paste0(ATT_EDGE_NAT,"_",ATT_VAL_PRO))
					ltp <- ATT_VAL_PRO
				
				# add to new graph
				lst <- list()
				lst[[ATT_EDGE_POL]] <- pol
				lst[[ATT_EDGE_NAT]] <- ltp
				res <- add_edges(res, edges=uv, attr=lst)
			}
		}
		
		# handle unknown links
		if(any(is.na(link.types)))
		{	unk <- TRUE
			for(lt in c(paste0(ATT_EDGE_NAT,"_",ATT_VAL_FRIEND),
					paste0(ATT_EDGE_NAT,"_",ATT_VAL_FAMILY),
					paste0(ATT_EDGE_NAT,"_",ATT_VAL_PRO)))
			{	unk <- unk && is.na(get.edge.attribute(g,lt,e))
				#print(get.edge.attribute(g,lt,e))
			}
			if(unk)
			{	ltp <- ATT_VAL_UNK
				pol <- get.edge.attribute(g,ATT_EDGE_POL,e)
				
				# add to new graph
				lst <- list()
				lst[[ATT_EDGE_POL]] <- pol
				lst[[ATT_EDGE_NAT]] <- ltp
				res <- add_edges(res, edges=uv, attr=lst)
			}
		}
	}
	
	return(res)
}




#############################################################
# Transform the received graph in order to get a uniplex signed
# graph.
#
# g : original graph, whose links will be cleaned to get the result graph.
#
# returns: a list of two graphs resulting from the cleaning. In the first,
#          links whose polarity is NA are considered as positive. In the 
#		   second they are plainly not included.
#############################################################
flatten.signed.graph <- function(g)
{	# remove all links and link attributes 
	sg1 <- delete_edges(g, edges=E(g))
	sg2 <- delete_edges(g, edges=E(g))
	for(ea in list.edge.attributes(g))
	{	sg1 <- delete_edge_attr(sg1, ea)
		sg2 <- delete_edge_attr(sg2, ea)
	}
	
	# process each link
	for(e in E(g))
	{	# get the link
		#print(e)
		uv = ends(g, E(g)[e], names=FALSE)
		# get its polarity
		pol <- 2*as.integer(get.edge.attribute(g,ATT_EDGE_POL,e)==ATT_VAL_POSITIVE) - 1
		
		# add to graphs
		lst <- list()
		lst[[ATT_EDGE_SIGN]] <- pol
		if(is.na(pol))
		{	lst[[ATT_EDGE_SIGN]] <- 1
			sg1 <- add_edges(sg1, edges=uv, attr=lst)
		}
		else
		{	sg1 <- add_edges(sg1, edges=uv, attr=lst)
			sg2 <- add_edges(sg2, edges=uv, attr=lst)
		}
	}
	
	sg1$name <- "withNAs"
	sg2$name <- "withoutNAs"
	res <- list(withNAs=sg1, withoutNAs=sg2)
	return(res)
}




#############################################################
# Removes the links of the targeted nodes, in order to isolate them.
#
# g: graph to process.
# nodes: nodes to isolate.
#
# returns: the modified graph.
#############################################################
disconnect.nodes <- function(g, nodes)
{	# process each node one by one
	for(n in nodes)
	{	# old version: not good, plot-wise
#		g <- delete_vertices(g,1)
	
		es <- incident(graph=g, v=n, mode="all")
		g <- delete.edges(g,es)
	}
	
	return(g)
}




#############################################################
# Computes the diameter, the corresponding paths, and plots them.
# Same thing for radius and eccentricity.
#
# g: original graph to process (ignored here).
# g0: same graph except the main node is isolated.
#############################################################
analyze.net.eccentricity <- function(g, g0)
{	###########################
	# compute diameter
	cat("  Computing diameter & radius\n")
	diam <- diameter(g0)					# get the network diameter
	cat("    Diameter=",diam,"\n",sep="")
	dd <- distances(graph=g0)				# compute all inter-node distances
	idx <- which(dd==diam, arr.ind=TRUE)	# retrieve pairs of nodes matching the diameter
	idx <- idx[idx[,1]<idx[,2],,drop=FALSE]	# filter (each pair appears twice due to symmetric matrix)
	
	# possibly create folder
	diameter.folder <- file.path(NET_FOLDER,g0$name,"diameter")
	dir.create(path=diameter.folder, showWarnings=FALSE, recursive=TRUE)
	
	# plot diameter
	diam.paths <- lapply(1:nrow(idx), function(r) all_shortest_paths(graph=g0, from=idx[r,1], to=idx[r,2])$res)
	for(pp in 1:length(diam.paths))
	{	custom.gplot(g0, paths=diam.paths[[pp]], file=file.path(diameter.folder,paste0("diam_graph0_",pp)))
		
		q <- 1
		for(p in 1:length(diam.paths[[pp]]))
		{	if(p==1 || !all(diam.paths[[pp]][[p]]==diam.paths[[pp]][[p-1]]))
			{	custom.gplot(g0, paths=diam.paths[[pp]][[p]], file=file.path(diameter.folder,paste0("diam_graph0_",pp,"_",q)))
				q <- q + 1
			}
		}
	}
		
	###########################
	# compute eccentricity
	cat("  Computing eccentricity\n")
	vals <- eccentricity(g0)
	
	# possibly create folder
	eccentricity.folder <- file.path(NET_FOLDER,g0$name,"eccentricity")
	dir.create(path=eccentricity.folder, showWarnings=FALSE, recursive=TRUE)
	
	# plot distribution
	custom.hist(vals, name=LONG_NAME[MEAS_ECCENTRICITY], file=file.path(eccentricity.folder,"eccentricity_histo"))
	
	# export CSV with eccentricity
	df <- data.frame(V(g0)$name,V(g0)$label,vals)
	colnames(df) <- c("Name","Label",MEAS_ECCENTRICITY) 
	write.csv(df, file=file.path(eccentricity.folder,"eccentricity_values.csv"), row.names=FALSE)
	
	# add eccentricity (as node attributes) to the graph
	V(g)$Eccentricity0 <- vals
	V(g0)$Eccentricity <- vals

	# plot graph using color for eccentricity
	custom.gplot(g0,col.att=MEAS_ECCENTRICITY,file=file.path(eccentricity.folder,"eccentricity_graph0"))
#	custom.gplot(g0,col.att=MEAS_ECCENTRICITY)
	
	###########################
	# compute radius
	rad <- min(vals[vals>0])
	
	# add radius and diameter to the graph (as attributes) and record
	g$diameter0 <- diam
	g0$diameter <- diam
	g$radius0 <- rad
	g0$radius <- rad
	cat("    Radius=",rad,"\n",sep="")
	
	# export CSV with radius and diameter
	stat.file <- file.path(NET_FOLDER,g0$name,"stats.csv")
	if(file.exists(stat.file))
	{	df <- read.csv(file=stat.file,header=TRUE,row.names=1)
		df[MEAS_DIAMETER, ] <- list(Value=diam, Mean=NA, Stdv=NA)
		df[MEAS_RADIUS, ] <- list(Value=rad, Mean=NA, Stdv=NA)
		df[MEAS_ECCENTRICITY, ] <- list(Value=NA, Mean=mean(vals), Stdv=sd(vals))
	}
	else
	{	df <- data.frame(Value=c(diam,rad,NA),Mean=c(NA,NA,mean(vals)),Stdv=c(NA,NA,sd(vals)))
		row.names(df) <- c(MEAS_DIAMETER,MEAS_RADIUS,MEAS_ECCENTRICITY)
	}
	write.csv(df, file=stat.file, row.names=TRUE)
	
	###########################
	# record graph and return it
	write.graph(graph=g, file=file.path(NET_FOLDER,g$name,"graph.graphml"), format="graphml")
	write.graph(graph=g0, file=file.path(NET_FOLDER,g0$name,"graph0.graphml"), format="graphml")

	lst <- list(g, g0)
	return(lst)
}




#############################################################
# Computes the degree and generates plots and CSV files.
#
# g: original graph to process.
# g0: same graph except the main node is isolated.
#############################################################
analyze.net.degree <- function(g, g0)
{	cat("  Computing degree\n")
	# possibly create folder
	degree.folder <- file.path(NET_FOLDER,g$name,"degree")
	dir.create(path=degree.folder, showWarnings=FALSE, recursive=TRUE)
	
	lst <- list(g, g0)
	sufxx <- c("","0")
	for(i in 1:length(lst))
	{	cat("    Processing graph ",i,"/",length(lst),"\n",sep="")
		g <- lst[[i]]
		sufx <- sufxx[i]
		
		# degree distribution
		vals <- degree(g)
		custom.hist(vals, name=LONG_NAME[MEAS_DEGREE], file=file.path(degree.folder,paste0("degree_histo",sufx)))
			
		# export CSV with degree
		df <- data.frame(V(g)$name,V(g)$label,vals)
		colnames(df) <- c("Name","Label",MEAS_DEGREE) 
		write.csv(df, file=file.path(degree.folder,paste0("degree_values",sufx,".csv")), row.names=FALSE)
		
		# add results to the graph (as attributes) and record
		V(g)$Degree <- vals
		g$DegreeAvg <- mean(vals)
		g$DegreeStdv <- sd(vals)
		write.graph(graph=g, file=file.path(NET_FOLDER,g$name,paste0("graph",sufx,".graphml")), format="graphml")
		
		# plot graph using color for degree
		custom.gplot(g,col.att=MEAS_DEGREE,file=file.path(degree.folder,paste0("degree_graph",sufx)))
#		custom.gplot(g,col.att=MEAS_DEGREE)
		
		# export CSV with average degree
		stat.file <- file.path(NET_FOLDER,g$name,"stats.csv")
		if(file.exists(stat.file))
		{	df <- read.csv(file=stat.file,header=TRUE,row.names=1)
			df[MEAS_DEGREE, ] <- list(Value=NA, Mean=mean(vals), Stdv=sd(vals))
		}
		else
		{	df <- data.frame(Value=c(NA),Mean=c(mean(vals)),Stdv=c(sd(vals)))
			row.names(df) <- c(MEAS_DEGREE)
		}
		write.csv(df, file=stat.file, row.names=TRUE)
		
		lst[[i]] <- g
	}
	
	return(lst)
}




#############################################################
# Computes Eigencentrality and generates plots and CSV files.
#
# g: original graph to process.
# g0: same graph except the main node is isolated.
#############################################################
analyze.net.eigencentrality <- function(g, g0)
{	cat("  Computing Eigencentrality\n")
	# possibly create folder
	eigen.folder <- file.path(NET_FOLDER,g$name,"eigencentrality")
	dir.create(path=eigen.folder, showWarnings=FALSE, recursive=TRUE)
	
	lst <- list(g, g0)
	sufxx <- c("","0")
	for(i in 1:length(lst))
	{	cat("    Processing graph ",i,"/",length(lst),"\n",sep="")
		g <- lst[[i]]
		sufx <- sufxx[i]
		
		# Eigencentrality distribution
		vals <- eigen_centrality(graph=g, scale=FALSE)$vector
		custom.hist(vals, name=LONG_NAME[MEAS_EIGEN], file=file.path(eigen.folder,paste0("eigencentrality_histo",sufx)))
		
		# export CSV with Eigencentrality
		df <- data.frame(V(g)$name,V(g)$label,vals)
		colnames(df) <- c("Name","Label",MEAS_EIGEN) 
		write.csv(df, file=file.path(eigen.folder,paste0("eigencentrality_values",sufx,".csv")), row.names=FALSE)
		
		# add results to the graph (as attributes) and record
		V(g)$Eigencentrality <- vals
		g$EigencentralityAvg <- mean(vals)
		g$EigencentralityStdv <- sd(vals)
		write.graph(graph=g, file=file.path(NET_FOLDER,g$name,paste0("graph",sufx,".graphml")), format="graphml")
		
		# plot graph using color for Eigencentrality
		custom.gplot(g,col.att=MEAS_EIGEN,file=file.path(eigen.folder,paste0("eigencentrality_graph",sufx)))
#		custom.gplot(g,col.att=MEAS_EIGEN)
		
		# export CSV with average Eigencentrality
		stat.file <- file.path(NET_FOLDER,g$name,"stats.csv")
		if(file.exists(stat.file))
		{	df <- read.csv(file=stat.file,header=TRUE,row.names=1)
			df[MEAS_EIGEN, ] <- list(Value=NA, Mean=mean(vals), Stdv=sd(vals))
		}
		else
		{	df <- data.frame(Value=c(NA),Mean=c(mean(vals)),Stdv=c(sd(vals)))
			row.names(df) <- c(MEAS_EIGEN)
		}
		write.csv(df, file=stat.file, row.names=TRUE)
		
		lst[[i]] <- g
	}
	
	return(lst)
}




#############################################################
# Computes betweenness and generates plots and CSV files.
#
# g: original graph to process.
# g0: same graph except the main node is isolated.
#############################################################
analyze.net.betweenness <- function(g, g0)
{	cat("  Computing betweenness\n")
	# possibly create folder
	betweenness.folder <- file.path(NET_FOLDER,g$name,"betweenness")
	dir.create(path=betweenness.folder, showWarnings=FALSE, recursive=TRUE)
	
	lst <- list(g, g0)
	sufxx <- c("","0")
	for(i in 1:length(lst))
	{	cat("    Processing graph ",i,"/",length(lst),"\n",sep="")
		g <- lst[[i]]
		sufx <- sufxx[i]
		
		# betweenness distribution
		vals <- betweenness(graph=g, normalized=FALSE)
		custom.hist(vals, name=LONG_NAME[MEAS_BETWEENNESS], file=file.path(betweenness.folder,paste0("betweenness_histo",sufx)))
		
		# export CSV with betweenness
		df <- data.frame(V(g)$name,V(g)$label,vals)
		colnames(df) <- c("Name","Label",MEAS_BETWEENNESS) 
		write.csv(df, file=file.path(betweenness.folder,paste0("betweenness_values",sufx,".csv")), row.names=FALSE)
		
		# add results to the graph (as attributes) and record
		V(g)$Betweenness <- vals
		g$BetweennessAvg <- mean(vals)
		g$BetweennessStdv <- sd(vals)
		write.graph(graph=g, file=file.path(NET_FOLDER,g$name,paste0("graph",sufx,".graphml")), format="graphml")
		
		# plot graph using color for betweenness
		custom.gplot(g,col.att=MEAS_BETWEENNESS,file=file.path(betweenness.folder,paste0("betweenness_graph",sufx)))
#		custom.gplot(g,col.att=MEAS_BETWEENNESS)
		
		# export CSV with average betweenness
		stat.file <- file.path(NET_FOLDER,g$name,"stats.csv")
		if(file.exists(stat.file))
		{	df <- read.csv(file=stat.file,header=TRUE,row.names=1)
			df[MEAS_BETWEENNESS, ] <- list(Value=NA, Mean=mean(vals), Stdv=sd(vals))
		}
		else
		{	df <- data.frame(Value=c(NA),Mean=c(mean(vals)),Stdv=c(sd(vals)))
			row.names(df) <- c(MEAS_BETWEENNESS)
		}
		write.csv(df, file=stat.file, row.names=TRUE)
		
		lst[[i]] <- g
	}
	
	return(lst)
}




#############################################################
# Computes closeness and generates plots and CSV files.
#
# g: original graph to process.
# g0: same graph except the main node is isolated.
#############################################################
analyze.net.closeness <- function(g, g0)
{	cat("  Computing closeness\n")
	# possibly create folder
	closeness.folder <- file.path(NET_FOLDER,g$name,"closeness")
	dir.create(path=closeness.folder, showWarnings=FALSE, recursive=TRUE)
	
	lst <- list(g, g0)
	sufxx <- c("","0")
	for(i in 1:length(lst))
	{	cat("    Processing graph ",i,"/",length(lst),"\n",sep="")
		g <- lst[[i]]
		sufx <- sufxx[i]
		
		# closeness distribution
		vals <- suppressWarnings(closeness(graph=g, normalized=TRUE))	# avoid warnings due to graph being disconnected
		custom.hist(vals, name=LONG_NAME[MEAS_CLOSENESS], file=file.path(closeness.folder,paste0("closeness_histo",sufx)))
		
		# export CSV with closeness
		df <- data.frame(V(g)$name,V(g)$label,vals)
		colnames(df) <- c("Name","Label",MEAS_CLOSENESS) 
		write.csv(df, file=file.path(closeness.folder,paste0("closeness_values",sufx,".csv")), row.names=FALSE)
		
		# add results to the graph (as attributes) and record
		V(g)$Closeness <- vals
		g$ClosenessAvg <- mean(vals)
		g$ClosenessStdv <- sd(vals)
		write.graph(graph=g, file=file.path(NET_FOLDER,g$name,paste0("graph",sufx,".graphml")), format="graphml")
		
		# plot graph using color for closeness
		custom.gplot(g,col.att=MEAS_CLOSENESS,file=file.path(closeness.folder,paste0("closeness_graph",sufx)))
#		custom.gplot(g,col.att=MEAS_CLOSENESS)
		
		# export CSV with average closeness
		stat.file <- file.path(NET_FOLDER,g$name,"stats.csv")
		if(file.exists(stat.file))
		{	df <- read.csv(file=stat.file,header=TRUE,row.names=1)
			df[MEAS_CLOSENESS, ] <- list(Value=NA, Mean=mean(vals), Stdv=sd(vals))
		}
		else
		{	df <- data.frame(Value=c(NA),Mean=c(mean(vals)),Stdv=c(sd(vals)))
			row.names(df) <- c(MEAS_CLOSENESS)
		}
		write.csv(df, file=stat.file, row.names=TRUE)
		
		lst[[i]] <- g
	}
	
	return(lst)
}




#############################################################
# Computes transitivity and generates plots and CSV files.
#
# g: original graph to process.
# g0: same graph except the main node is isolated.
#############################################################
analyze.net.transitivity <- function(g, g0)
{	cat("  Computing transitivity\n")
	# possibly create folder
	transitivity.folder <- file.path(NET_FOLDER,g$name,"transitivity")
	dir.create(path=transitivity.folder, showWarnings=FALSE, recursive=TRUE)
	
	lst <- list(g, g0)
	sufxx <- c("","0")
	for(i in 1:length(lst))
	{	cat("    Processing graph ",i,"/",length(lst),"\n",sep="")
		g <- lst[[i]]
		sufx <- sufxx[i]
		
		# transitivity distribution
		vals <- transitivity(graph=g, type="localundirected", isolates="zero")
		custom.hist(vals, name=LONG_NAME[MEAS_TRANSITIVITY], file=file.path(transitivity.folder,paste0("transitivity_histo",sufx)))
		
		# export CSV with transitivity
		df <- data.frame(V(g)$name,V(g)$label,vals)
		colnames(df) <- c("Name","Label",MEAS_TRANSITIVITY) 
		write.csv(df, file=file.path(transitivity.folder,paste0("transitivity_values",sufx,".csv")), row.names=FALSE)
		
		# add results to the graph (as attributes) and record
		V(g)$Transitivity <- vals
		g$Transitivity <- transitivity(graph=g, type="globalundirected", isolates="zero")
		g$TransitivityAvg <- mean(vals)
		g$TransitivityStdv <- sd(vals)
		write.graph(graph=g, file=file.path(NET_FOLDER,g$name,paste0("graph",sufx,".graphml")), format="graphml")
		
		# plot graph using color for transitivity
		custom.gplot(g,col.att=MEAS_TRANSITIVITY,file=file.path(transitivity.folder,paste0("transitivity_graph",sufx)))
#		custom.gplot(g,col.att=MEAS_TRANSITIVITY)
		
		# export CSV with average transitivity
		stat.file <- file.path(NET_FOLDER,g$name,"stats.csv")
		if(file.exists(stat.file))
		{	df <- read.csv(file=stat.file,header=TRUE,row.names=1)
			df[MEAS_TRANSITIVITY, ] <- list(Value=g$Transitivity, Mean=mean(vals), Stdv=sd(vals))
		}
		else
		{	df <- data.frame(Value=c(g$Transitivity),Mean=c(mean(vals)),Stdv=c(sd(vals)))
			row.names(df) <- c(MEAS_TRANSITIVITY)
		}
		write.csv(df, file=stat.file, row.names=TRUE)
		
		lst[[i]] <- g
	}
	
	return(lst)
}




#############################################################
# Detects the community structure of the network.
#
# g: original graph to process.
# g0: same graph except the main node is isolated.
#############################################################
analyze.net.comstruct <- function(g, g0)
{	cat("  Detecting community structure\n")
	# possibly create folder
	communities.folder <- file.path(NET_FOLDER,g$name,"communities")
	dir.create(path=communities.folder, showWarnings=FALSE, recursive=TRUE)
	
	lst <- list(g, g0)
	sufxx <- c("","0")
	for(i in 1:length(lst))
	{	cat("    Processing graph ",i,"/",length(lst),"\n",sep="")
		g <- lst[[i]]
		sufx <- sufxx[i]
		op <- delete_edges(graph=g, edges=which(is.na(E(g)$Polarite) | E(g)$Polarite==ATT_VAL_NEGATIVE))
		nn <- delete_edges(graph=g, edges=which(E(g)$Polarite==ATT_VAL_NEGATIVE))
		idx.op <- which(degree(op)>0)
		idx.nn <- which(degree(nn)>0)
		
		# community size distribution
#		coms.op <- cluster_optimal(graph=simplify(op))	# much slower, obviously
#		coms.op <- cluster_spinglass(graph=simplify(op))
		coms.op <- cluster_infomap(graph=simplify(op))
		mbrs.op <- as.integer(membership(coms.op))
		mbrs.op[-idx.op] <- NA
		sizes.op <- table(mbrs.op,useNA="ifany")
		custom.barplot(sizes.op, text=names(sizes.op), xlab=LONG_NAME[MEAS_COMMUNITY_ONLYPOS], ylab="Taille", file=file.path(communities.folder,paste0("onlypos_community_size_bars",sufx)))
		#
		coms.nn <- cluster_infomap(graph=simplify(nn))
		mbrs.nn <- as.integer(membership(coms.nn))
		mbrs.nn[-idx.nn] <- NA
		sizes.nn <- table(mbrs.nn,useNA="ifany")
		custom.barplot(sizes.nn, text=names(sizes.nn), xlab=LONG_NAME[MEAS_COMMUNITY_NONEG], ylab="Taille", file=file.path(communities.folder,paste0("noneg_community_size_bars",sufx)))
		
		# export CSV with community membership
		df <- data.frame(V(op)$name,V(op)$label,mbrs.op)
		colnames(df) <- c("Name","Label",MEAS_COMMUNITY_ONLYPOS) 
		write.csv(df, file=file.path(communities.folder,paste0("onlypos_community_membership",sufx,".csv")), row.names=FALSE)
		#
		df <- data.frame(V(nn)$name,V(nn)$label,mbrs.nn)
		colnames(df) <- c("Name","Label",MEAS_COMMUNITY_NONEG) 
		write.csv(df, file=file.path(communities.folder,paste0("noneg_community_membership",sufx,".csv")), row.names=FALSE)
		
		# add results to the graph (as attributes) and record
		mod.op <- modularity(coms.op)
		op <- set_vertex_attr(graph=op,name=MEAS_COMMUNITY_ONLYPOS,value=mbrs.op)
		op <- set_graph_attr(graph=op,name=MEAS_MODULARITY_ONLYPOS,value=mod.op)
		cat("    Modularity with only positive links: ",mod.op,"\n",sep="")
		#
		mod.nn <- modularity(coms.nn)
		nn <- set_vertex_attr(graph=nn,name=MEAS_COMMUNITY_NONEG,value=mbrs.nn)
		nn <- set_graph_attr(graph=nn,name=MEAS_MODULARITY_NONEG,value=mod.nn)
		cat("    Modularity without negative links: ",mod.nn,"\n",sep="")
		#
		g <- set_vertex_attr(graph=g,name=MEAS_COMMUNITY_ONLYPOS,value=mbrs.op)
		g <- set_graph_attr(graph=g,name=MEAS_MODULARITY_ONLYPOS,value=mod.op)
		g <- set_vertex_attr(graph=g,name=MEAS_COMMUNITY_NONEG,value=mbrs.nn)
		g <- set_graph_attr(graph=g,name=MEAS_MODULARITY_NONEG,value=mod.nn)
		write.graph(graph=g, file=file.path(NET_FOLDER,g$name,paste0("graph",sufx,".graphml")), format="graphml")
		
		# plot graph using color for communities
		custom.gplot(op,col.att=MEAS_COMMUNITY_ONLYPOS,cat.att=TRUE,file=file.path(communities.folder,paste0("onlypos_communities_graph",sufx)))
#		custom.gplot(op,col.att=MEAS_COMMUNITY_ONLYPOS,cat.att=TRUE)
		custom.gplot(nn,col.att=MEAS_COMMUNITY_NONEG,cat.att=TRUE,file=file.path(communities.folder,paste0("noneg_communities_graph",sufx)))
#		custom.gplot(nn,col.att=MEAS_COMMUNITY_NONEG,cat.att=TRUE)

		# export CSV with modularity
		stat.file <- file.path(NET_FOLDER,g$name,"stats.csv")
		if(file.exists(stat.file))
		{	df <- read.csv(file=stat.file,header=TRUE,row.names=1)
			df[MEAS_MODULARITY_ONLYPOS, ] <- list(Value=mod.op, Mean=NA, Stdv=NA)
		}
		else
		{	df <- data.frame(Value=c(mod.op),Mean=c(NA),Stdv=c(NA))
			row.names(df) <- c(MEAS_MODULARITY_ONLYPOS)
		}
		df[MEAS_MODULARITY_NONEG, ] <- list(Value=mod.nn, Mean=NA, Stdv=NA)
		write.csv(df, file=stat.file, row.names=TRUE)
		
		lst[[i]] <- g
	}
	
	return(lst)
}




#############################################################
# Computes the assortativity of the network.
#
# g: original graph to process (ignored here).
# g0: same graph except the main node is isolated.
#############################################################
analyze.net.assortativity <- function(g, g0)
{	cat("  Computing the assortativity\n")
	
	# retrieve the list of vertex attributes
	att.list <- list.vertex.attributes(g0)
	
	#############################
	# deal with categorical attributes
	cat.data <- NA
	
	# gather regular categorical attributes
	attrs <- c(ATT_NODE_REL_TRAJ, ATT_NODE_REL_HADR,			# relationships
			ATT_NODE_SEN_POLDER, ATT_NODE_EQU_POLDER,			# last political positions
			ATT_NODE_ADELECTIO, ATT_NODE_SPANISH)				# misc
	for(attr in attrs)
	{	tmp <- vertex_attr(g0, attr)
		if(all(is.na(cat.data)))
			cat.data <- matrix(as.integer(factor(tmp)),ncol=1)
		else
			cat.data <- cbind(cat.data, as.integer(factor(tmp)))
		colnames(cat.data)[ncol(cat.data)] <- attr
	}
	
	# convert tag-type attributes
	attrs <- c(ATT_NODE_SEN_POL, ATT_NODE_SEN_MILIT,			# senatorial positions
			ATT_NODE_EQU_POL, ATT_NODE_EQU_MILIT,				# equestrian positions
			ATT_NODE_TRAV_DEST, ATT_NODE_TRAV_REAS,				# travels
			ATT_NODE_CIRCLES)									# circles
	for(attr in attrs)
	{	tmp <- att.list[grepl(att.list,pattern=attr)]
		m <- sapply(tmp, function(att) vertex_attr(g0, att))
		uvals <- sort(unique(c(m)))
		for(uval in uvals)
		{	cat.data <- cbind(cat.data, as.integer(factor(apply(m, 1, function(v) uval %in% v[!is.na(v)]))))
			colnames(cat.data)[ncol(cat.data)] <- paste(attr,uval,sep="_")
		}
	}
	
	# compute the assortativity for all categorical attributes
	vals <- c()
	for(i in 1:ncol(cat.data))
	{	# compute the assortativity
		attr <- colnames(cat.data)[i]
		
		# if there are some NAs
		if(any(is.na(cat.data[,i])))
		{	# explicitly represent them as a class
			cd <- cat.data[,i]
			cd[is.na(cd)] <- max(cd,na.rm=TRUE) + 1
			ass <- assortativity_nominal(graph=g0, types=cd)
			cat("    Assortativity for attribute \"",attr,"\" when representing NAs by 0: ",ass,"\n",sep="")
			vals <- c(vals, ass)
			names(vals)[length(vals)] <- paste(attr,"_explicitNA",sep="")
			# ignore them
			cd <- cat.data[,i]
			cd <- cd[!is.na(cd)]
			if(length(cd)>1)
			{	gg <- delete_vertices(g, which(is.na(cat.data[,i])))
				ass <- assortativity_nominal(graph=gg, types=cd)
			}
			else
				ass <- NA
			cat("    Assortativity for attribute \"",attr,"\" when ignoring NAs: ",ass,"\n",sep="")
			vals <- c(vals, ass)
			names(vals)[length(vals)] <- paste(attr,"_noNA",sep="")
		}
		
		# no NA at all
		else
		{	ass <- assortativity_nominal(graph=g0, types=cat.data[,i])
			cat("    Assortativity for attribute \"",attr,"\": ",ass,"\n",sep="")
			vals <- c(vals, ass)
			names(vals)[length(vals)] <- attr
		}
	}
	
	#############################
	# deal with numerical attributes
	num.data <- NA
	
	# gather regular numerical attributes
	attrs <- c(ATT_NODE_TRAV_NBR)					# number of travels
	for(attr in attrs)
	{	tmp <- vertex_attr(g0, attr)
		if(all(is.na(num.data)))
			num.data <- matrix(tmp,ncol=1)
		else
			num.data <- cbind(num.data, tmp)
		colnames(num.data)[ncol(num.data)] <- attr
	}
	
	# compute the assortativity for all numerical attributes
	for(i in 1:ncol(num.data))
	{	# compute the assortativity
		attr <- colnames(num.data)[i]
		
		# if there are some NAs
		if(any(is.na(num.data[,i])))
		{	# explicitly represent them as zeroes
			cd <- num.data[,i]
			cd[is.na(cd)] <- 0
			ass <- assortativity(graph=g0, types1=cd)
			cat("    Assortativity for attribute \"",attr,"\" when replacing NAs by 0: ",ass,"\n",sep="")
			vals <- c(vals, ass)
			names(vals)[length(vals)] <- paste(attr,"_explicitNA",sep="")
			# ignore them
			cd <- num.data[,i]
			cd <- cd[!is.na(cd)]
			if(length(cd)>1)
			{	gg <- delete_vertices(g, which(is.na(num.data[,i])))
				ass <- assortativity(graph=gg, types1=cd)
			}
			else
				ass <- NA
			cat("    Assortativity for attribute \"",attr,"\" when ignoring NAs: ",ass,"\n",sep="")
			vals <- c(vals, ass)
			names(vals)[length(vals)] <- paste(attr,"_noNA",sep="")
		}
		# no NA at all
		else
		{	ass <- assortativity(graph=g0, types1=num.data[,i])
			cat("    Assortativity for attribute \"",attr,"\": ",ass,"\n",sep="")
			vals <- c(vals, ass)
			names(vals)[length(vals)] <- attr
		}
	}
	
	#############################
	# record the results
	
	# add results to the graph (as attributes) and record
	for(i in 1:length(vals))
	{	attr <- names(vals)[i]
		g <- set_vertex_attr(graph=g, name=attr, value=vals[i])
		g0 <- set_vertex_attr(graph=g0, name=attr, value=vals[i])
	}
	write.graph(graph=g, file=file.path(NET_FOLDER,g$name,paste0("graph.graphml")), format="graphml")
	write.graph(graph=g0, file=file.path(NET_FOLDER,g0$name,paste0("graph0.graphml")), format="graphml")
	
	# add assortativity to main CSV
	stat.file <- file.path(NET_FOLDER,g0$name,"stats.csv")
	if(file.exists(stat.file))
	{	df <- read.csv(file=stat.file,header=TRUE,row.names=1)
		for(i in 1:length(vals))
		{	attr <- names(vals)[i]
			df[attr, ] <- list(Value=vals[i], Mean=NA, Stdv=NA)
		}
	}
	else
	{	df <- data.frame(Value=c(vals[i]),Mean=c(NA),Stdv=c(NA))
		row.names(df) <- c(names(vals)[1])
		for(i in 2:length(vals))
		{	attr <- names(vals)[i]
			df[attr, ] <- list(Value=vals[i], Mean=NA, Stdv=NA)
		}
		
	}
	write.csv(df, file=stat.file, row.names=TRUE)
	
	#############################
	# assortativity over
	lst <- list(g, g0)
	return(lst)
}




#############################################################
# Computes stats related to the node attributes.
#
# g: original graph to process (ignored here).
# g0: same graph except the main node is isolated.
#############################################################
analyze.net.attributes <- function(g, g0)
{	cat("  Computing nodal attribute stats\n")
	# possibly create folders
	graph.folder <- file.path(NET_FOLDER,g$name,"attributes")
	dir.create(path=graph.folder, showWarnings=FALSE, recursive=TRUE)
	dir.create(path=COMP_FOLDER, showWarnings=FALSE, recursive=TRUE)
	
	# retrieve the list of vertex attributes
	att.list <- list.vertex.attributes(g)
	
	#############################
	# deal with categorical attributes
	cat.data <- NA
	
	# gather regular categorical attributes
	attrs <- c(ATT_NODE_REL_TRAJ, ATT_NODE_SEN_POLDER, ATT_NODE_EQU_POLDER,	# several categories
			ATT_NODE_ADELECTIO, ATT_NODE_REL_HADR, ATT_NODE_SPANISH)		# only two categories (plus NA)
	for(attr in attrs)
	{	# get values
		tmp <- vertex_attr(g, attr)
		
		# plot the attribute distribution as a barplot
		cat("    Bar-plotting attribute \"",attr,"\"\n",sep="")
		tt <- table(tmp, useNA="ifany")
		plot.folder <- file.path(ATT_FOLDER,attr)
		dir.create(path=plot.folder, showWarnings=FALSE, recursive=TRUE)
		plot.file <- file.path(plot.folder,paste0(attr,"_bars"))
		custom.barplot(tt, 
				text=names(tt), 
				xlab=LONG_NAME[attr], ylab="Frequence",
				file=plot.file)
		# record as a table
		tt <- as.data.frame(tt)
		colnames(tt) <- c("Value","Frequency")
		table.file <- file.path(plot.folder,paste0(attr,"_vals.csv"))
		write.csv(tt, file=table.file, row.names=FALSE)
		
		# add to matrix
		cat("    Adding attribute \"",attr,"\" to data matrix\n",sep="")
		if(all(is.na(cat.data)))
			cat.data <- matrix(tmp,ncol=1)
		else
			cat.data <- cbind(cat.data, tmp)
		colnames(cat.data)[ncol(cat.data)] <- attr
	}
	
	# convert tag-type attributes
	attrs <- c(ATT_NODE_SEN_POL, ATT_NODE_SEN_MILIT,			# senatorial positions
			ATT_NODE_EQU_POL, ATT_NODE_EQU_MILIT,				# equestrian positions
			ATT_NODE_TRAV_DEST, ATT_NODE_TRAV_REAS,				# travels
			ATT_NODE_CIRCLES)									# circles
	for(attr in attrs)
	{	tmp <- att.list[grepl(att.list,pattern=attr)]
		m <- sapply(tmp, function(att) vertex_attr(g, att))
		
		# count tag distribution
		idx.nas <- which(apply(m,1,function(r) all(is.na(r))))	# detect individuals with only NAs
		nbr.nas <- length(idx.nas) 								# count them
		dt <- c(m)[!is.na(c(m))]								# handles non-NA values
		dt <- c(dt,rep(NA,nbr.nas))								# insert the appropriate number of NAs
		# compute highest frequency for later use (to handle plot y-scale)
		tt <- table(dt, useNA="ifany")
		if(any(is.na(names(tt))))
			na.nbr <- tt[is.na(names(tt))]
		else
			na.nbr <- 0
		tmp <- sapply(tt, function(x) gorder(g)-x-na.nbr)
		ymax <- max(tmp,na.nbr)
		# identify least frequent values
		unfrequent <- names(tt)[which(tt<=2)]
		# plot tag distribution as barplot
		cat("    Bar-plotting attributes containing \"",attr,"\"\n",sep="")
		plot.folder <- file.path(ATT_FOLDER,attr)
		dir.create(path=plot.folder, showWarnings=FALSE, recursive=TRUE)
		plot.file <- file.path(plot.folder,paste0(attr,"_bars"))
		custom.barplot(tt, 
				text=names(tt), 
				xlab=LONG_NAME[attr], ylab="Frequence", 
				file=plot.file)
		# record tag distribution as table
		tt <- as.data.frame(tt)
		colnames(tt) <- c("Value","Frequency")
		table.file <- file.path(plot.folder,paste0(attr,"_vals.csv"))
		write.csv(tt, file=table.file, row.names=FALSE)
		# plot tags on a graph
		if(attr==ATT_NODE_TRAV_DEST)
		{	gg0 <- g0
			gg <- g
			for(a in colnames(m))
			{	vals <- vertex_attr(g0,a)
				vals[which(!is.na(match(vals,unfrequent)))] <- paste0(" ",ATT_VAL_OTHER)
				g0 <- set_vertex_attr(g0, a, value=vals)
				g <- set_vertex_attr(g, a, value=vals)
			}
		}
		custom.gplot(g=g0, col.att=attr, cat.att=TRUE, color.isolates=TRUE, file=file.path(graph.folder,paste0(attr,"_graph0")))
		custom.gplot(g=g, col.att=attr, cat.att=TRUE, color.isolates=TRUE, file=file.path(graph.folder,paste0(attr,"_graph")))
		if(attr==ATT_NODE_TRAV_DEST)
		{	g0 <- gg0
			g <- gg
		}
			
		# add to matrix
		cat("    Adding attribute \"",attr,"\" to data matrix\n",sep="")
		uvals <- sort(unique(c(m)))
		for(uval in uvals)
		{	# binarize tags
			vals <- apply(m, 1, function(v) uval %in% v[!is.na(v)])
			idxt <- which(vals)
			idxf <- which(!vals)
			vals[idxt] <- ATT_VAL_TRUE
			vals[idxf] <- ATT_VAL_FALSE
			vals[idx.nas] <- NA
			cat.data <- cbind(cat.data, vals)
			att_name <- paste(attr,uval,sep="_")
			colnames(cat.data)[ncol(cat.data)] <- att_name
			
			# produce TRUE/FALSE barplots
			tt <- table(vals, useNA="ifany")
			plot.file <- file.path(plot.folder,paste0(att_name,"_bars"))
			custom.barplot(tt, 
					text=names(tt), 
					xlab=LONG_NAME[att_name], ylab="Frequence", 
					file=plot.file,
					ylim=c(0,ymax))
			# record values as table
			tt <- as.data.frame(tt)
			colnames(tt) <- c("Value","Frequency")
			table.file <- file.path(plot.folder,paste0(att_name,"_vals.csv"))
			write.csv(tt, file=table.file, row.names=FALSE)
		}
	}
	
	# replace NAs by "Unknown" tags
#	cat.data[which(is.na(cat.data))] <- ATT_VAL_UNK
	
	for(i in 1:ncol(cat.data))
	{	attr <- colnames(cat.data)[i]
		
		# plot one attribute versus another
		if(i<ncol(cat.data))
		{	for(j in (i+1):ncol(cat.data))
			{	attr2 <- colnames(cat.data)[j]
				vals1 <- cat.data[,i]
				vals2 <- cat.data[,j]
				tt <- table(vals1, vals2, useNA="ifany")
				names(dimnames(tt)) <- c(LONG_NAME[attr],LONG_NAME[attr2])
				# plot file
				plot.file <- file.path(COMP_FOLDER,paste0(attr,"_vs_",attr2,"_bars"))
				custom.barplot(vals=tt, 
						text=colnames(tt), 
						xlab=LONG_NAME[attr2], ylab="Frequence",
						file=plot.file)
				# record tag distribution as table
				tt <- as.data.frame(tt)
				table.file <- file.path(COMP_FOLDER,paste0(attr,"_vs_",attr2,"_vals.csv"))
				write.csv(tt, file=table.file, row.names=FALSE)
			}
		}
		
		# plot the graph using colors for attribute values
		cat("    Graph-plottig attribute \"",attr,"\"\n",sep="")
		# with trajan
		gg <- set_vertex_attr(graph=g, name=attr, value=cat.data[,i])
		custom.gplot(gg,col.att=attr,cat.att=TRUE,color.isolates=TRUE,file=file.path(graph.folder,paste0(attr,"_graph")))
#		custom.gplot(gg,col.att=attr,cat.att=TRUE,color.isolates=TRUE)
		# without trajan
		gg <- set_vertex_attr(graph=g0, name=attr, value=cat.data[,i])
		custom.gplot(gg,col.att=attr,cat.att=TRUE,color.isolates=TRUE,file=file.path(graph.folder,paste0(attr,"_graph0")))
#		custom.gplot(gg,col.att=attr,cat.att=TRUE,color.isolates=TRUE)
	}
	
	#############################
	# deal with numerical attributes
	num.data <- NA
	
	# gather regular numerical attributes
	attrs <- c(ATT_NODE_TRAV_NBR)
	for(attr in attrs)
	{	# get values
		tmp <- vertex_attr(g0, attr)
		
		# plot the attribute distribution as a histogram 
		# (actually a barplot, for now, as the only numeric attribute is an integer)
		cat("    Bar-plotting attribute \"",attr,"\"\n",sep="")
		tt <- table(tmp, useNA="ifany")
		plot.folder <- file.path(ATT_FOLDER,attr)
		dir.create(path=plot.folder, showWarnings=FALSE, recursive=TRUE)
		plot.file <- file.path(plot.folder,paste0(attr,"_bars"))
		custom.barplot(tt, 
				text=names(tt), 
				xlab=LONG_NAME[attr], ylab="Frequence", 
				file=plot.file)
		# record as a table
		tt <- as.data.frame(tt)
		colnames(tt) <- c("Value","Frequency")
		table.file <- file.path(plot.folder,paste0(attr,"_vals.csv"))
		write.csv(tt, file=table.file, row.names=FALSE)
		
		# add to matrix
		if(all(is.na(num.data)))
			num.data <- matrix(tmp,ncol=1)
		else
			num.data <- cbind(num.data, tmp)
		colnames(num.data)[ncol(num.data)] <- attr
	}
	
	# replace NAs by "Unknown" tags
#	num.data[which(is.na(num.data))] <- ATT_VAL_UNK
	
	# plot the graph using colors for attribute values
	for(i in 1:ncol(num.data))
	{	attr <- colnames(num.data)[i]
		cat("    Plottig attribute \"",attr,"\"\n",sep="")
		# with trajan
		gg <- set_vertex_attr(graph=g, name=attr, value=num.data[,i])
		custom.gplot(gg,col.att=attr,cat.att=FALSE,color.isolates=TRUE,file=file.path(graph.folder,paste0(attr,"_graph")))
#		custom.gplot(gg,col.att=attr,cat.att=FALSE,color.isolates=TRUE)
		# without trajan
		gg <- set_vertex_attr(graph=g0, name=attr, value=num.data[,i])
		custom.gplot(gg,col.att=attr,cat.att=FALSE,color.isolates=TRUE,file=file.path(graph.folder,paste0(attr,"_graph0")))
#		custom.gplot(gg,col.att=attr,cat.att=FALSE,color.isolates=TRUE)
	}
	
	#############################
	# attributes over
	lst <- list(g, g0)
	return(lst)
}




#############################################################
# Recursively computes articulation points.
#
# g: original graph to process.
# g0: same graph without the main node (ignored here).
#############################################################
analyze.net.articulation <- function(g, g0)
{	# init 
	cat("  Computing articulation points\n")
	g1 <- g
	level <- 1
	art <- articulation_points(g1)
	
	# repeat until no more articulation point
	while(length(art)>0)
	{	cat("    Level ",level,"\n",sep="")
		# disconnect the articulation nodes
		g1 <- disconnect.nodes(g1, nodes=art)
		# mark them
		vals <- apply(cbind(rep(level,length(art)),V(g1)$Articulation[art]),1,function(v) min(v,na.rm=TRUE))
		V(g1)[art]$Articulation <- vals
		# proceed with the next level
		art <- articulation_points(g1)
		level <- level + 1
	}
	V(g1)$Articulation[is.na(V(g1)$Articulation)] <- level
	vals <- V(g1)$Articulation
	
	# possibly create folder
	articulation.folder <- file.path(NET_FOLDER,g$name,"articulation")
	dir.create(path=articulation.folder, showWarnings=FALSE, recursive=TRUE)
	
	# plot distribution
	custom.hist(vals, name=LONG_NAME[MEAS_ARTICULATION], file=file.path(articulation.folder,paste0("articulation_histo")))
	
	# export CSV with articulation
	df <- data.frame(V(g)$name,V(g)$label,vals)
	colnames(df) <- c("Name","Label",MEAS_ARTICULATION) 
	write.csv(df, file=file.path(articulation.folder,paste0("articulation_values.csv")), row.names=FALSE)
	
	# add results to the graph (as attributes) and record
	V(g)$Articulation <- vals
	V(g0)$Articulation00 <- vals
	g$ArticulationAvg <- mean(vals)
	g0$ArticulationAvg00 <- mean(vals)
	g$ArticulationAvg <- mean(vals)
	g0$ArticulationAvg00 <- mean(vals)
	write.graph(graph=g, file=file.path(NET_FOLDER,g$name,paste0("graph.graphml")), format="graphml")
	write.graph(graph=g0, file=file.path(NET_FOLDER,g0$name,paste0("graph0.graphml")), format="graphml")
	
	# plot graph using color for articulation
	custom.gplot(g,col.att=MEAS_ARTICULATION,file=file.path(articulation.folder,paste0("articulation_graph")))
#	custom.gplot(g,col.att=MEAS_ARTICULATION)
	
	# export CSV with average articulation
	stat.file <- file.path(NET_FOLDER,g$name,"stats.csv")
	if(file.exists(stat.file))
	{	df <- read.csv(file=stat.file,header=TRUE,row.names=1)
		df[MEAS_ARTICULATION, ] <- list(Value=NA, Mean=mean(vals), Stdv=sd(vals))
	}
	else
	{	df <- data.frame(Value=c(NA),Mean=c(mean(vals)),Stdv=c(sd(vals)))
		row.names(df) <- c(MEAS_ARTICULATION)
	}
	write.csv(df, file=stat.file, row.names=TRUE)
	
	lst <- list(g, g0)
	return(lst)
}




#############################################################
# Computes average distances and generates plots and CSV files.
#
# g: original graph to process.
# g0: same graph except the main node is isolated.
#############################################################
analyze.net.distance <- function(g, g0)
{	cat("  Computing average distances\n")
	# possibly create folder
	distance.folder <- file.path(NET_FOLDER,g$name,"distance")
	dir.create(path=distance.folder, showWarnings=FALSE, recursive=TRUE)
	
	lst <- list(g, g0)
	sufxx <- c("","0")
	for(i in 1:length(lst))
	{	cat("    Processing graph ",i,"/",length(lst),"\n",sep="")
		g <- lst[[i]]
		sufx <- sufxx[i]
		
		# distance distribution
		vals <- distances(graph=g)
		flat.vals <- vals[upper.tri(vals)]
		custom.hist(vals=flat.vals, name=LONG_NAME[MEAS_DISTANCE], file=file.path(distance.folder,paste0("distance_histo",sufx)))
		# average distance distribution
		avg.vals <- apply(X=vals,MARGIN=1,FUN=function(v) mean(v[!is.infinite(v)]))
		custom.hist(vals=avg.vals, name=LONG_NAME[MEAS_DISTANCE_AVG], file=file.path(distance.folder,paste0("distance_avg_histo",sufx)))
		
		# export CSV with average distance
		df <- data.frame(V(g)$name,V(g)$label,avg.vals)
		colnames(df) <- c("Name","Label",MEAS_DISTANCE_AVG) 
		write.csv(df, file=file.path(distance.folder,paste0("distance_avg_values",sufx,".csv")), row.names=FALSE)
		
		# add results to the graph (as attributes) and record
		V(g)$AverageDistance <- avg.vals
		g$DistanceAvg <- mean(flat.vals)
		g$DistanceStdv <- sd(flat.vals)
		write.graph(graph=g, file=file.path(NET_FOLDER,g$name,paste0("graph",sufx,".graphml")), format="graphml")
		
		# for each node, plot graph using color for distance
		for(n in 1:gorder(g))
		{	nname <- V(g)$name[n]
			V(g)$Distance <- vals[n,]
			if(all(is.infinite(vals[n,-n])))
				cat("    NOT plotting graph for node #",nname,", as all values are infinite\n",sep="")
			else
			{	cat("    Plotting graph for node #",nname,"\n",sep="")
				custom.gplot(g,col.att=MEAS_DISTANCE,v.hl=n,file=file.path(distance.folder,paste0("distance_graph",sufx,"_",nname)))
			}
			g <- delete_vertex_attr(graph=g, name=MEAS_DISTANCE)
		}
		
		# export CSV with average distance
		stat.file <- file.path(NET_FOLDER,g$name,"stats.csv")
		if(file.exists(stat.file))
		{	df <- read.csv(file=stat.file,header=TRUE,row.names=1)
			df[MEAS_DISTANCE, ] <- list(Value=NA, Mean=mean(flat.vals), Stdv=sd(flat.vals))
		}
		else
		{	df <- data.frame(Value=c(NA),Mean=c(mean(flat.vals)),Stdv=c(sd(flat.vals)))
			row.names(df) <- c(MEAS_DISTANCE)
		}
		write.csv(df, file=stat.file, row.names=TRUE)
		
		lst[[i]] <- g
	}
	
	return(lst)
}




#############################################################
# Computes vertex connectivity and generates plots and CSV files.
#
# g: original graph to process.
# g0: same graph except the main node is isolated.
#############################################################
analyze.net.connectivity <- function(g, g0)
{	cat("  Computing vertex connectivity\n")
	# possibly create folder
	connectivity.folder <- file.path(NET_FOLDER,g$name,"connectivity")
	dir.create(path=connectivity.folder, showWarnings=FALSE, recursive=TRUE)
	
	lst <- list(g, g0)
	sufxx <- c("","0")
	for(i in 1:length(lst))
	{	cat("    Processing graph ",i,"/",length(lst),"\n",sep="")
		g <- lst[[i]]
		sufx <- sufxx[i]
		
		# connectivity distribution
		vals <- matrix(NA, nrow=gorder(g), ncol=gorder(g))
		for(n in 1:(gorder(g)-1))
		{	vals[n,n] <- 0
			neigh <- neighbors(graph=g, v=n)
			for(n2 in (n+1):gorder(g))
			{	if(n2 %in% neigh)
					tmp <- 1
				else
					tmp <- vertex_connectivity(graph=g, source=n, target=n2)
				vals[n,n2] <- tmp
				vals[n2,n] <- vals[n,n2]
			}
		}
		vals[gorder(g),gorder(g)] <- 0
		flat.vals <- vals[upper.tri(vals)]
		custom.hist(vals=flat.vals, name=LONG_NAME[MEAS_CONNECTIVITY], file=file.path(connectivity.folder,paste0("connectivity_histo",sufx)))
		# connectivity distribution
		avg.vals <- apply(X=vals,MARGIN=1,FUN=function(v) mean(v[!is.infinite(v)]))
		custom.hist(vals=avg.vals, name=LONG_NAME[MEAS_CONNECTIVITY_AVG], file=file.path(connectivity.folder,paste0("connectivity_avg_histo",sufx)))
		
		# export CSV with average connectivity
		df <- data.frame(V(g)$name,V(g)$label,avg.vals)
		colnames(df) <- c("Name","Label",MEAS_CONNECTIVITY_AVG) 
		write.csv(df, file=file.path(connectivity.folder,paste0("connectivity_avg_values",sufx,".csv")), row.names=FALSE)
		
		# add results to the graph (as attributes) and record
		V(g)$AverageConnectivity <- avg.vals
		g$ConnectivityAvg <- mean(flat.vals)
		g$ConnectivityStdv <- sd(flat.vals)
		write.graph(graph=g, file=file.path(NET_FOLDER,g$name,paste0("graph",sufx,".graphml")), format="graphml")
		
		# for each node, plot graph using color for connectivity
		for(n in 1:gorder(g))
		{	nname <- V(g)$name[n]
			V(g)$Connectivity <- vals[n,]
			if(all(vals[n,]==0))
				cat("    NOT plotting graph for node #",nname,", as all values are zero\n",sep="")
			else
			{	cat("    Plotting graph for node #",nname,"\n",sep="")
				custom.gplot(g,col.att=MEAS_CONNECTIVITY,v.hl=n,file=file.path(connectivity.folder,paste0("connectivity_graph",sufx,"_",nname)))
			}
			g <- delete_vertex_attr(graph=g, name="Connectivity")
		}
		
		# export CSV with average connectivity
		stat.file <- file.path(NET_FOLDER,g$name,"stats.csv")
		if(file.exists(stat.file))
		{	df <- read.csv(file=stat.file,header=TRUE,row.names=1)
			df[MEAS_CONNECTIVITY, ] <- list(Value=NA, Mean=mean(flat.vals), Stdv=sd(flat.vals))
		}
		else
		{	df <- data.frame(Value=c(NA),Mean=c(mean(flat.vals)),Stdv=c(sd(flat.vals)))
			row.names(df) <- c(MEAS_CONNECTIVITY)
		}
		write.csv(df, file=stat.file, row.names=TRUE)
		
		lst[[i]] <- g
	}
	
	return(lst)
}




#############################################################
# Computes the signed degree and generates plots and CSV files.
#
# sg: signed graph to process.
# sg0: same graph except the main node is isolated.
#############################################################
analyze.net.signed.degree <- function(sg, sg0)
{	cat("  Computing signed degree\n")
	# possibly create folder
	degree.folder <- file.path(SIGNED_FOLDER,sg$name,"degree")
	dir.create(path=degree.folder, showWarnings=FALSE, recursive=TRUE)
	
	lst <- list(sg, sg0)
	sufxx <- c("","0")
	for(i in 1:length(lst))
	{	cat("    Processing graph ",i,"/",length(lst),"\n",sep="")
		sg <- lst[[i]]
		sufx <- sufxx[i]
		
		# degree distribution
		pos.deg <- degree_signed(sg, mode="all", type="pos")
		neg.deg <- degree_signed(sg, mode="all", type="neg")
#sgg <- delete_edges(sg, which(E(sg)$sign==-1))
#pos.deg <- degree(sgg)
#sgg <- delete_edges(sg, which(E(sg)$sign==1))
#neg.deg <- degree(sgg)
		custom.hist(pos.deg, name=LONG_NAME[MEAS_DEGREE_POS], file=file.path(degree.folder,paste0("degree_pos_histo",sufx)))
		custom.hist(neg.deg, name=LONG_NAME[MEAS_DEGREE_NEG], file=file.path(degree.folder,paste0("degree_neg_histo",sufx)))
		
		# export CSV with degree
		df <- data.frame(V(sg)$name,V(sg)$label,pos.deg,neg.deg)
		colnames(df) <- c("Name","Label",MEAS_DEGREE_POS,MEAS_DEGREE_NEG) 
		write.csv(df, file=file.path(degree.folder,paste0("degree_values",sufx,".csv")), row.names=FALSE)
		
		# add results to the graph (as attributes) and record
		V(sg)$DegreePos <- pos.deg
		V(sg)$DegreeNeg <- neg.deg
		sg$DegreePosAvg <- mean(pos.deg)
		sg$DegreeNegAvg <- mean(neg.deg)
		sg$DegreePosStdv <- sd(pos.deg)
		sg$DegreeNegStdv <- sd(neg.deg)
		write.graph(graph=sg, file=file.path(SIGNED_FOLDER,sg$name,paste0("graph",sufx,".graphml")), format="graphml")
		
		# plot graph using color for degree
		custom.gplot(sg,col.att=MEAS_DEGREE_POS,file=file.path(degree.folder,paste0("degree_pos_graph",sufx)))
#		custom.gplot(sg,col.att=MEAS_DEGREE_POS)
		custom.gplot(sg,col.att=MEAS_DEGREE_NEG,file=file.path(degree.folder,paste0("degree_neg_graph",sufx)))
#		custom.gplot(sg,col.att=MEAS_DEGREE_NEG)
		
		# export CSV with average degree
		stat.file <- file.path(SIGNED_FOLDER,sg$name,"stats.csv")
		if(file.exists(stat.file))
		{	df <- read.csv(file=stat.file,header=TRUE,row.names=1)
			df[MEAS_DEGREE_POS, ] <- list(Value=NA, Mean=mean(pos.deg), Stdv=sd(pos.deg))
		}
		else
		{	df <- data.frame(Value=c(NA),Mean=c(mean(pos.deg)),Stdv=c(sd(pos.deg)))
			row.names(df) <- c(MEAS_DEGREE_POS)
		}
		df[MEAS_DEGREE_NEG, ] <- list(Value=NA, Mean=mean(neg.deg), Stdv=sd(neg.deg))
		write.csv(df, file=stat.file, row.names=TRUE)
		
		lst[[i]] <- sg
	}
	
	return(lst)
}




#############################################################
# Computes the signed centrality and generates plots and CSV files.
#
# sg: signed graph to process.
# sg0: same graph except the main node is isolated.
#############################################################
analyze.net.signed.centrality <- function(sg, sg0)
{	cat("  Computing signed centrality\n")
	# possibly create folder
	centr.folder <- file.path(SIGNED_FOLDER,sg$name,"pnindex")
	dir.create(path=centr.folder, showWarnings=FALSE, recursive=TRUE)
	
	lst <- list(sg, sg0)
	sufxx <- c("","0")
	for(i in 1:length(lst))
	{	cat("    Processing graph ",i,"/",length(lst),"\n",sep="")
		sg <- lst[[i]]
		sufx <- sufxx[i]
		
		# centrality distribution
		vals <- pn_index(sg, mode="all")
		custom.hist(vals, name=LONG_NAME[MEAS_SIGN_CENTR], file=file.path(centr.folder,paste0("pnindex_histo",sufx)))
		
		# export CSV with centrality values
		df <- data.frame(V(sg)$name,V(sg)$label,vals)
		colnames(df) <- c("Name","Label",MEAS_SIGN_CENTR) 
		write.csv(df, file=file.path(centr.folder,paste0("pnindex_values",sufx,".csv")), row.names=FALSE)
		
		# add results to the graph (as attributes) and record
		V(sg)$PNindex <- vals
		sg$PNindexAvg <- mean(vals)
		sg$PNindexStdv <- sd(vals)
		write.graph(graph=sg, file=file.path(SIGNED_FOLDER,sg$name,paste0("graph",sufx,".graphml")), format="graphml")
		
		# plot graph using color for degree
		custom.gplot(sg,col.att=MEAS_SIGN_CENTR,file=file.path(centr.folder,paste0("pnindex_graph",sufx)))
#		custom.gplot(sg,col.att=MEAS_SIGN_CENTR)
		
		# export CSV with average degree
		stat.file <- file.path(SIGNED_FOLDER,sg$name,"stats.csv")
		if(file.exists(stat.file))
		{	df <- read.csv(file=stat.file,header=TRUE,row.names=1)
			df[MEAS_SIGN_CENTR, ] <- list(Value=NA, Mean=mean(vals), Stdv=sd(vals))
		}
		else
		{	df <- data.frame(Value=c(NA),Mean=c(mean(vals)),Stdv=c(sd(vals)))
			row.names(df) <- c(MEAS_SIGN_CENTR)
		}
		write.csv(df, file=stat.file, row.names=TRUE)
		
		lst[[i]] <- sg
	}
	
	return(lst)
}




#############################################################
# Computes the signed triangles, structural balance, and generates 
# plots and CSV files.
#
# sg: signed graph to process.
# sg0: same graph except the main node is isolated.
#############################################################
analyze.net.signed.triangles <- function(sg, sg0)
{	cat("  Computing signed triangles\n")
	# possibly create folder
	triangles.folder <- file.path(SIGNED_FOLDER,sg$name,"triangles")
	dir.create(path=triangles.folder, showWarnings=FALSE, recursive=TRUE)
	
	lst <- list(sg, sg0)
	sufxx <- c("","0")
	for(i in 1:length(lst))
	{	cat("    Processing graph ",i,"/",length(lst),"\n",sep="")
		sg <- lst[[i]]
		sufx <- sufxx[i]
		
		# signed triangle distribution
		triangles <- get_signed_triangles(sg)
		tri.classes <- c("---","--+","-++","+++")
		cls <- tri.classes[triangles[,"Class"]+1]
		counts <- table(factor(cls,levels=tri.classes))
		custom.barplot(
				vals=counts, 
				text=tri.classes, 
				xlab=LONG_NAME[MEAS_TRI_SIGNED], ylab="Frequence",
				file=file.path(triangles.folder,paste0("triangle_bars",sufx)))
		
		# export CSV with triangle counts
		df <- data.frame(names(counts),counts)
		colnames(df) <- c("Class","Number") 
		write.csv(df, file=file.path(triangles.folder,paste0("triangle_values",sufx,".csv")), row.names=FALSE)
		
		# compute structural balance
		str.struct.bal <- (counts["--+"]+counts["+++"])/sum(counts)
		gen.struct.bal <- (counts["---"]+counts["--+"]+counts["+++"])/sum(counts)
		
		# add results to the graph (as attributes) and record
		sg$StrStructBal <- str.struct.bal
		sg$GenStructBal <- gen.struct.bal
		write.graph(graph=sg, file=file.path(SIGNED_FOLDER,sg$name,paste0("graph",sufx,".graphml")), format="graphml")
		
		# export CSV with structural balance values
		stat.file <- file.path(SIGNED_FOLDER,sg$name,"stats.csv")
		if(file.exists(stat.file))
		{	df <- read.csv(file=stat.file,header=TRUE,row.names=1)
			df[MEAS_STR_STRUCT_BAL, ] <- list(Value=str.struct.bal, Mean=NA, Stdv=NA)
		}
		else
		{	df <- data.frame(Value=str.struct.bal,Mean=NA,Stdv=NA)
			row.names(df) <- c(MEAS_STR_STRUCT_BAL)
		}
		df[MEAS_GEN_STRUCT_BAL, ] <- list(Value=gen.struct.bal, Mean=NA, Stdv=NA)
		write.csv(df, file=stat.file, row.names=TRUE)
		
		# plot graph with strict unbalanced triangles
		idx <- which(cls=="---" | cls=="-++")
		for(t in 1:length(idx))
		{	tri <- idx[t]
			imb.edges <- c(get.edge.ids(sg,triangles[tri,1:2]),
					get.edge.ids(sg,triangles[tri,2:3]),
					get.edge.ids(sg,triangles[tri,c(3,1)]))
			custom.gplot(sg,e.hl=imb.edges,file=file.path(triangles.folder,paste0("triangles_str_imb_graph",sufx,"_",t)))
#			custom.gplot(sg,e.hl=imb.edges)
		}
		
		# plot graph with generalized unbalanced triangles
		idx <- which(cls=="-++")
		for(t in 1:length(idx))
		{	tri <- idx[t]
			imb.edges <- c(get.edge.ids(sg,triangles[tri,1:2]),
					get.edge.ids(sg,triangles[tri,2:3]),
					get.edge.ids(sg,triangles[tri,c(3,1)]))
			custom.gplot(sg,e.hl=imb.edges,file=file.path(triangles.folder,paste0("triangles_gen_imb_graph",sufx,"_",t)))
#			custom.gplot(sg,e.hl=imb.edges)
		}
		
		lst[[i]] <- sg
	}
	
	return(lst)
}




#############################################################
# Partitions the signed specified graph by solving the correlation
# clustering problem.
#
# sg: original signed graph to process.
# sg0: same graph except the main node is isolated.
#############################################################
analyze.net.corclust <- function(sg, sg0)
{	cat("  Performing correlation clustering\n")
	# possibly create folder
	corclust.folder <- file.path(SIGNED_FOLDER,sg$name,"corclust")
	dir.create(path=corclust.folder, showWarnings=FALSE, recursive=TRUE)
	
	lst <- list(sg, sg0)
	sufxx <- c("","0")
	for(i in 1:length(lst))
	{	cat("    Processing graph ",i,"/",length(lst),"\n",sep="")
		sg <- lst[[i]]
		sufx <- sufxx[i]
		
		memberships <- NA
		perfs <- c()
		
		# try each possible number of clusters
		for(k in 1:gorder(sg))
		{	cat("    Performing correlation clustering for k=",k,"\n",sep="")
			
			# cluster size distribution
			tmp <- signed_blockmodel(sg, k=k, annealing=TRUE)
			mbrs <- tmp$membership
			perf <- tmp$criterion
			cat("    Imbalance: ",perf,"\n",sep="")
			sizes <- table(mbrs) 
#			custom.barplot(sizes, text=names(sizes), xlab=LONG_NAME[MEAS_COR_CLUST], ylab="Taille", file=file.path(corclust.folder,paste0("cluster_size_bars",sufx,"_k",k)))
			
			# add to general structures
			colname <- paste0("k=",k)
			if(all(is.na(memberships)))
			{	memberships <- matrix(mbrs,ncol=1)
				colnames(memberships) <- colname
			}
			else
			{	memberships <- cbind(memberships, mbrs)
				colnames(memberships)[ncol(memberships)] <- colname
			}
			perfs[colname] <- perf
			
			# add results to the graph as attributes
			sg <- set_vertex_attr(graph=sg,name=MEAS_COR_CLUST,value=mbrs)
			sg <- set_graph_attr(graph=sg,name=MEAS_COR_CLUST,value=perf)
			#attr_name <- paste0(MEAS_COR_CLUST,"_k",k)
			#sg <- set_vertex_attr(graph=sg,name=attr_name,value=mbrs)
			#sg <- set_graph_attr(graph=sg,name=attr_name,value=perf)
			
			# plot graph using color for clusters
#			custom.gplot(sg,col.att=MEAS_COR_CLUST,cat.att=TRUE,file=file.path(corclust.folder,paste0("clusters_graph",sufx,"_k",k)))
			#custom.gplot(sg,col.att=MEAS_COR_CLUST,cat.att=TRUE)
		
			# plot block model
#			sg2 <- sg
#			V(sg2)$name <- V(sg2)$label
#			bm.file <- file.path(corclust.folder,paste0("block_model",sufx,"_k",k))
#			if(FORMAT=="pdf")
#				bm.file <- paste0(bm.file,".pdf")
#			else if(FORMAT=="png")
#				bm.file <- paste0(bm.file,".png")
#			ggblock(sg2, mbrs, show_blocks=TRUE, show_labels=TRUE)
#			ggsave(bm.file, width=35, height=25, units="cm")
		}
		
		# restore best partition
		idx <- which.min(perfs)
		mbrs <- memberships[,idx]
		perf <- perfs[idx]
		sg <- set_vertex_attr(graph=sg,name=MEAS_COR_CLUST,value=mbrs)
		sg <- set_graph_attr(graph=sg,name=MEAS_COR_CLUST,value=perf)
		
		# plot best cluster size distribution
		sizes <- table(memberships[,idx]) 
		custom.barplot(sizes, text=names(sizes), xlab=LONG_NAME[MEAS_COR_CLUST], ylab="Taille", file=file.path(corclust.folder,paste0("cluster_size_bars",sufx)))
		
		# plot graph using color for best partition
		custom.gplot(sg,col.att=MEAS_COR_CLUST,cat.att=TRUE,file=file.path(corclust.folder,paste0("clusters_graph",sufx)))
		
		# plot best block model
		sg2 <- sg
		V(sg2)$name <- V(sg2)$label
		bm.file <- file.path(corclust.folder,paste0("block_model",sufx))
		if(FORMAT=="pdf")
			bm.file <- paste0(bm.file,".pdf")
		else if(FORMAT=="png")
			bm.file <- paste0(bm.file,".png")
		ggblock(sg2, mbrs, show_blocks=TRUE, show_labels=TRUE)
		ggsave(bm.file, width=35, height=25, units="cm")
		
		# export CSV with cluster membership
		df <- data.frame(V(sg)$name,V(sg)$label,memberships)
		colnames(df) <- c("Name","Label",MEAS_COR_CLUST) 
		write.csv(df, file=file.path(corclust.folder,paste0("cluster_membership",sufx,".csv")), row.names=FALSE)
		
		# export CSV with performance as imbalance
		df <- data.frame(1:gorder(sg),perfs)		
		colnames(df) <- c("k",MEAS_COR_CLUST) 
		write.csv(df, file=file.path(corclust.folder,paste0("corclust_imbalance",sufx,".csv")), row.names=FALSE)
		
		# add imbalance to stat CSV
		stat.file <- file.path(SIGNED_FOLDER,sg$name,"stats.csv")
		if(file.exists(stat.file))
		{	df <- read.csv(file=stat.file,header=TRUE,row.names=1)
			df[MEAS_COR_CLUST, ] <- list(Value=min(perfs), Mean=NA, Stdv=NA)
		}
		else
		{	df <- data.frame(Value=c(min(perfs)),Mean=c(NA),Stdv=c(NA))
			row.names(df) <- c(MEAS_COR_CLUST)
		}
		write.csv(df, file=stat.file, row.names=TRUE)
		
		# record graph with all its attributes
#		sg <- delete_vertex_attr(graph=sg,name=MEAS_COR_CLUST)
#		sg <- delete_graph_attr(graph=sg,name=MEAS_COR_CLUST)
		write.graph(graph=sg, file=file.path(SIGNED_FOLDER,sg$name,paste0("graph",sufx,".graphml")), format="graphml")
		
		lst[[i]] <- sg
	}
	
	return(lst)
}




#############################################################
# Main method for the graph analysis. Uses a predefined layout.
# Generates a bunch of plots and CSV files.
#
# og: graph to process.
#############################################################
analyze.network <- function(og)
{	# set up list of graphs
	g.lst <- list()
	
	# extract various graphs depending on link types
	{	g.lst[[GRAPH_TYPE_ALL]] <- clean.links(og, link.types=c(paste0(ATT_EDGE_NAT,"_",ATT_VAL_FAMILY), paste0(ATT_EDGE_NAT,"_",ATT_VAL_FRIEND), paste0(ATT_EDGE_NAT,"_",ATT_VAL_PRO),NA))
		g.lst[[GRAPH_TYPE_ALL]]$name <- GRAPH_TYPE_ALL
		g.lst[[GRAPH_TYPE_FAMILY]] <- clean.links(og, link.types=paste0(ATT_EDGE_NAT,"_",ATT_VAL_FAMILY))
		g.lst[[GRAPH_TYPE_FAMILY]]$name <- GRAPH_TYPE_FAMILY
		g.lst[[GRAPH_TYPE_FRIEND]] <- clean.links(og, link.types=paste0(ATT_EDGE_NAT,"_",ATT_VAL_FRIEND))
		g.lst[[GRAPH_TYPE_FRIEND]]$name <- GRAPH_TYPE_FRIEND
		g.lst[[GRAPH_TYPE_PRO]] <- clean.links(og, link.types=paste0(ATT_EDGE_NAT,"_",ATT_VAL_PRO))
		g.lst[[GRAPH_TYPE_PRO]]$name <- GRAPH_TYPE_PRO
		g.lst[[GRAPH_TYPE_UNK]] <- clean.links(og, link.types=NA)
		g.lst[[GRAPH_TYPE_UNK]]$name <- GRAPH_TYPE_UNK
	}
	
	# process each graph
	for(g in g.lst)
	{	# g <- g.lst[[1]]
		cat("Processing graph '",g$name,"'\n",sep="")
		# create graph-specific folder
		tmp.folder <- file.path(NET_FOLDER, g$name)
		dir.create(path=tmp.folder, showWarnings=FALSE, recursive=TRUE)
		
		# record graph as a graphml file
		write.graph(graph=g, file=file.path(tmp.folder,"graph.graphml"), format="graphml")
		
		# plot full graph
		custom.gplot(g, file=file.path(tmp.folder,"graph"))
#		custom.gplot(g)
		
		# delete trajan's links for better visibility
		# TODO maybe better to just draw them using a light color?
		g0 <- disconnect.nodes(g, nodes=1)
		custom.gplot(g0, file=file.path(tmp.folder,"graph0"))
#		custom.gplot(g0)
		write.graph(graph=g0, file=file.path(tmp.folder,"graph0.graphml"), format="graphml")
		
		# compute attribute stats 
		# (must be done first, before other results are added as attributes)
		tmp <- analyze.net.attributes(g, g0)
		g <- tmp[[1]]
		g0 <- tmp[[2]]
		
		# compute diameters, eccentricity, radius
		tmp <- analyze.net.eccentricity(g, g0)
		g <- tmp[[1]]
		g0 <- tmp[[2]]
		
		# compute degree
		tmp <- analyze.net.degree(g, g0)
		g <- tmp[[1]]
		g0 <- tmp[[2]]
		
		# compute eigencentrality
		tmp <- analyze.net.eigencentrality(g, g0)
		g <- tmp[[1]]
		g0 <- tmp[[2]]
		
		# compute betweenness
		tmp <- analyze.net.betweenness(g, g0)
		g <- tmp[[1]]
		g0 <- tmp[[2]]
		
		# compute closeness
		tmp <- analyze.net.closeness(g, g0)
		g <- tmp[[1]]
		g0 <- tmp[[2]]
		
		# compute distances
		tmp <- analyze.net.distance(g, g0)
		g <- tmp[[1]]
		g0 <- tmp[[2]]
		
		# compute articulation points
		tmp <- analyze.net.articulation(g, g0)
		g <- tmp[[1]]
		g0 <- tmp[[2]]
		
		# detect communities
		tmp <- analyze.net.comstruct(g, g0)
		g <- tmp[[1]]
		g0 <- tmp[[2]]
		
		# compute transitivity
		tmp <- analyze.net.transitivity(g, g0)
		g <- tmp[[1]]
		g0 <- tmp[[2]]
		
		# compute vertex connectivity
		tmp <- analyze.net.connectivity(g, g0)
		g <- tmp[[1]]
		g0 <- tmp[[2]]
		
		# compute assortativity
		tmp <- analyze.net.assortativity(g, g0)
		g <- tmp[[1]]
		g0 <- tmp[[2]]
	}
	
	# extract and process the signed graphs
	sg.lst <- flatten.signed.graph(og)
	for(sg in sg.lst)
	{	#sg <- sg.lst[[1]]
		
		# create graph-specific folder
		tmp.folder <- file.path(SIGNED_FOLDER, sg$name)
		dir.create(path=tmp.folder, showWarnings=FALSE, recursive=TRUE)
		
		# plot the signed graph
		custom.gplot(sg, file=file.path(tmp.folder,"graph"))
#		custom.gplot(sg)
		# record graph as a graphml file
		write.graph(graph=sg, file=file.path(tmp.folder,"graph.graphml"), format="graphml")
		
		# get the version without Trajan
		sg0 <- disconnect.nodes(sg, nodes=1)
		custom.gplot(sg0, file=file.path(tmp.folder,"graph0"))
#		custom.gplot(sg0)
		write.graph(graph=sg0, file=file.path(tmp.folder,"graph0.graphml"), format="graphml")
		
		# compute signed degree
		tmp <- analyze.net.signed.degree(sg, sg0)
		sg <- tmp[[1]]
		sg0 <- tmp[[2]]
		
		# compute signed triangles
		tmp <- analyze.net.signed.triangles(sg, sg0)
		sg <- tmp[[1]]
		sg0 <- tmp[[2]]
		
		# compute signed centrality
		tmp <- analyze.net.signed.centrality(sg, sg0)
		sg <- tmp[[1]]
		sg0 <- tmp[[2]]
		
		# compute correlation clustering
		tmp <- analyze.net.corclust(sg, sg0)
		sg <- tmp[[1]]
		sg0 <- tmp[[2]]
	}
}
