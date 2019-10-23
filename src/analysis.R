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
# Parameter link.types lists the types to retain in the result
# graph.
#
# g : original graph, whose links will be cleaned to get the result graph.
# link:types : list of link types to keep in the result graph.
#############################################################
clean.links <- function(g, link.types)
{	res <- delete_edges(g, edges=E(g))
	for(ea in list.edge.attributes(g))
		res <- delete_edge_attr(res, ea)

	# possibly remove NA from list of link types
	idx <- which(is.na(link.types))
	if(length(idx)>0)
		link.types0 <- link.types[-idx]
	else
		link.types0 <- link.types
	
	# process each edge
	for(e in E(g))
	{	uv = ends(g, E(g)[e], names=FALSE)
		
		# process each specified link type (except NA)
		for(lt in link.types0)
		{	# convert attribute values
			vv <- as.logical(get.edge.attribute(g,lt,e))
			pol <- as.logical(get.edge.attribute(g,"Polarite",e))
			
			if(!is.na(vv) && vv)
			{	# convert link nature
				if(lt=="Nature_Amicale")
					ltp <- "Friend"
				else if(lt=="Nature_Familiale")
					ltp <- "Family"
				else if (lt=="Nature_Professionnelle")
					ltp <- "Pro"
				
				# add to new graph
				res <- add_edges(res, edges=uv, attr=list(
								"Polarity"=pol,
								"Type"=ltp))
			}
		}
		
		# handle unknown links
		if(any(is.na(link.types)))
		{	unk <- TRUE
			for(lt in c("Nature_Amicale","Nature_Familiale","Nature_Professionnelle"))
			{	unk <- unk && is.na(get.edge.attribute(g,lt,e))
#				print(get.edge.attribute(g,lt,e))				
			}
			if(unk)
			{	ltp <- "Unknown"
				pol <- as.logical(get.edge.attribute(g,"Polarite",e))
				
				# add to new graph
				res <- add_edges(res, edges=uv, attr=list(
								"Polarity"=pol,
								"Type"=ltp))
			}
		}
	}
	
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
	custom.hist(vals, name="Eccentricity", file=file.path(eccentricity.folder,"eccentricity_histo"))
	
	# export CSV with eccentricity
	df <- data.frame(V(g0)$name,V(g0)$label,vals)
	colnames(df) <- c("Name","Label","Eccentricity") 
	write.csv(df, file=file.path(eccentricity.folder,"eccentricity_values.csv"))
	
	# add eccentricity (as node attributes) to the graph
	V(g)$Eccentricity0 <- vals
	V(g0)$Eccentricity <- vals

	# plot graph using color for eccentricity
	custom.gplot(g0,col.att="Eccentricity",file=file.path(eccentricity.folder,"eccentricity_graph0"))
#	custom.gplot(g0,col.att="Eccentricity")
	
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
		df["Diameter", ] <- list(Value=diam, Mean=NA, Stdv=NA)
		df["Radius", ] <- list(Value=rad, Mean=NA, Stdv=NA)
		df["Eccentricity", ] <- list(Value=NA, Mean=mean(vals), Stdv=sd(vals))
	}
	else
	{	df <- data.frame(Value=c(diam,rad,NA),Mean=c(NA,NA,mean(vals)),Stdv=c(NA,NA,sd(vals)))
		row.names(df) <- c("Diameter","Radius","Eccentricity")
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
# Computes degree and generates plots and CSV files.
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
		custom.hist(vals, name="Degree", file=file.path(degree.folder,paste0("degree_histo",sufx)))
			
		# export CSV with degree
		df <- data.frame(V(g)$name,V(g)$label,vals)
		colnames(df) <- c("Name","Label","Degree") 
		write.csv(df, file=file.path(degree.folder,paste0("degree_values",sufx,".csv")))
		
		# add results to the graph (as attributes) and record
		V(g)$Degree <- vals
		g$DegreeAvg <- mean(vals)
		g$DegreeStdv <- sd(vals)
		write.graph(graph=g, file=file.path(NET_FOLDER,g$name,paste0("graph",sufx,".graphml")), format="graphml")
		
		# plot graph using color for degree
		custom.gplot(g,col.att="Degree",file=file.path(degree.folder,paste0("degree_graph",sufx)))
#		custom.gplot(g,col.att="Degree")
		
		# export CSV with average degree
		stat.file <- file.path(NET_FOLDER,g$name,"stats.csv")
		if(file.exists(stat.file))
		{	df <- read.csv(file=stat.file,header=TRUE,row.names=1)
			df["Degree", ] <- list(Value=NA, Mean=mean(vals), Stdv=sd(vals))
		}
		else
		{	df <- data.frame(Value=c(NA),Mean=c(mean(vals)),Stdv=c(sd(vals)))
			row.names(df) <- c("Degree")
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
		custom.hist(vals, name="Eigencentrality", file=file.path(eigen.folder,paste0("eigencentrality_histo",sufx)))
		
		# export CSV with Eigencentrality
		df <- data.frame(V(g)$name,V(g)$label,vals)
		colnames(df) <- c("Name","Label","Eigencentrality") 
		write.csv(df, file=file.path(eigen.folder,paste0("eigencentrality_values",sufx,".csv")))
		
		# add results to the graph (as attributes) and record
		V(g)$Eigencentrality <- vals
		g$EigencentralityAvg <- mean(vals)
		g$EigencentralityStdv <- sd(vals)
		write.graph(graph=g, file=file.path(NET_FOLDER,g$name,paste0("graph",sufx,".graphml")), format="graphml")
		
		# plot graph using color for Eigencentrality
		custom.gplot(g,col.att="Eigencentrality",file=file.path(eigen.folder,paste0("eigencentrality_graph",sufx)))
#		custom.gplot(g,col.att="Eigencentrality")
		
		# export CSV with average Eigencentrality
		stat.file <- file.path(NET_FOLDER,g$name,"stats.csv")
		if(file.exists(stat.file))
		{	df <- read.csv(file=stat.file,header=TRUE,row.names=1)
			df["Eigencentrality", ] <- list(Value=NA, Mean=mean(vals), Stdv=sd(vals))
		}
		else
		{	df <- data.frame(Value=c(NA),Mean=c(mean(vals)),Stdv=c(sd(vals)))
			row.names(df) <- c("Eigencentrality")
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
		custom.hist(vals, name="betweenness", file=file.path(betweenness.folder,paste0("betweenness_histo",sufx)))
		
		# export CSV with betweenness
		df <- data.frame(V(g)$name,V(g)$label,vals)
		colnames(df) <- c("Name","Label","Betweenness") 
		write.csv(df, file=file.path(betweenness.folder,paste0("betweenness_values",sufx,".csv")))
		
		# add results to the graph (as attributes) and record
		V(g)$Betweenness <- vals
		g$BetweennessAvg <- mean(vals)
		g$BetweennessStdv <- sd(vals)
		write.graph(graph=g, file=file.path(NET_FOLDER,g$name,paste0("graph",sufx,".graphml")), format="graphml")
		
		# plot graph using color for betweenness
		custom.gplot(g,col.att="Betweenness",file=file.path(betweenness.folder,paste0("betweenness_graph",sufx)))
#		custom.gplot(g,col.att="Betweenness")
		
		# export CSV with average betweenness
		stat.file <- file.path(NET_FOLDER,g$name,"stats.csv")
		if(file.exists(stat.file))
		{	df <- read.csv(file=stat.file,header=TRUE,row.names=1)
			df["Betweenness", ] <- list(Value=NA, Mean=mean(vals), Stdv=sd(vals))
		}
		else
		{	df <- data.frame(Value=c(NA),Mean=c(mean(vals)),Stdv=c(sd(vals)))
			row.names(df) <- c("Betweenness")
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
		vals <- suppressWarnings(closeness(graph=g, normalized=FALSE))	# avoid warnings due to graph being disconnected
		custom.hist(vals, name="closeness", file=file.path(closeness.folder,paste0("closeness_histo",sufx)))
		
		# export CSV with closeness
		df <- data.frame(V(g)$name,V(g)$label,vals)
		colnames(df) <- c("Name","Label","Closeness") 
		write.csv(df, file=file.path(closeness.folder,paste0("closeness_values",sufx,".csv")))
		
		# add results to the graph (as attributes) and record
		V(g)$Closeness <- vals
		g$ClosenessAvg <- mean(vals)
		g$ClosenessStdv <- sd(vals)
		write.graph(graph=g, file=file.path(NET_FOLDER,g$name,paste0("graph",sufx,".graphml")), format="graphml")
		
		# plot graph using color for closeness
		custom.gplot(g,col.att="Closeness",file=file.path(closeness.folder,paste0("closeness_graph",sufx)))
#		custom.gplot(g,col.att="Closeness")
		
		# export CSV with average closeness
		stat.file <- file.path(NET_FOLDER,g$name,"stats.csv")
		if(file.exists(stat.file))
		{	df <- read.csv(file=stat.file,header=TRUE,row.names=1)
			df["Closeness", ] <- list(Value=NA, Mean=mean(vals), Stdv=sd(vals))
		}
		else
		{	df <- data.frame(Value=c(NA),Mean=c(mean(vals)),Stdv=c(sd(vals)))
			row.names(df) <- c("Closeness")
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
		custom.hist(vals, name="transitivity", file=file.path(transitivity.folder,paste0("transitivity_histo",sufx)))
		
		# export CSV with transitivity
		df <- data.frame(V(g)$name,V(g)$label,vals)
		colnames(df) <- c("Name","Label","Transitivity") 
		write.csv(df, file=file.path(transitivity.folder,paste0("transitivity_values",sufx,".csv")))
		
		# add results to the graph (as attributes) and record
		V(g)$Transitivity <- vals
		g$Transitivity <- transitivity(graph=g, type="globalundirected", isolates="zero")
		g$TransitivityAvg <- mean(vals)
		g$TransitivityStdv <- sd(vals)
		write.graph(graph=g, file=file.path(NET_FOLDER,g$name,paste0("graph",sufx,".graphml")), format="graphml")
		
		# plot graph using color for transitivity
		custom.gplot(g,col.att="Transitivity",file=file.path(transitivity.folder,paste0("transitivity_graph",sufx)))
#		custom.gplot(g,col.att="Transitivity")
		
		# export CSV with average transitivity
		stat.file <- file.path(NET_FOLDER,g$name,"stats.csv")
		if(file.exists(stat.file))
		{	df <- read.csv(file=stat.file,header=TRUE,row.names=1)
			df["Transitivity", ] <- list(Value=g$Transitivity, Mean=mean(vals), Stdv=sd(vals))
		}
		else
		{	df <- data.frame(Value=c(g$Transitivity),Mean=c(mean(vals)),Stdv=c(sd(vals)))
			row.names(df) <- c("Transitivity")
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
		idx <- which(degree(g)>0)
		
		# community size distribution
#		coms <- cluster_optimal(graph=simplify(g))	# much slower, obviously
#		coms <- cluster_spinglass(graph=simplify(g))
		coms <- cluster_infomap(graph=simplify(g))
		mbrs <- as.integer(membership(coms))
		sizes <- table(mbrs[idx]) 
		custom.barplot(sizes, text=names(sizes), xlab="Community", ylab="Size", file=file.path(communities.folder,paste0("community_size_bars",sufx)))
		
		# export CSV with community membership
		df <- data.frame(V(g)$name,V(g)$label,mbrs)
		colnames(df) <- c("Name","Label","Community") 
		write.csv(df, file=file.path(communities.folder,paste0("community_membership",sufx,".csv")))
		
		# add results to the graph (as attributes) and record
		V(g)$Communities <- mbrs
		g$Modularity <- modularity(coms)
		cat("    Modularity: ",g$Modularity,"\n",sep="")
		write.graph(graph=g, file=file.path(NET_FOLDER,g$name,paste0("graph",sufx,".graphml")), format="graphml")
		
		# plot graph using color for communities
		custom.gplot(g,col.att="Communities",cat.att=TRUE,file=file.path(communities.folder,paste0("communities_graph",sufx)))
#		custom.gplot(g,col.att="Communities",cat.att=TRUE)
		
		# export CSV with modularity
		stat.file <- file.path(NET_FOLDER,g$name,"stats.csv")
		if(file.exists(stat.file))
		{	df <- read.csv(file=stat.file,header=TRUE,row.names=1)
			df["Modularity", ] <- list(Value=g$Modularity, Mean=NA, Stdv=NA)
		}
		else
		{	df <- data.frame(Value=c(g$Modularity),Mean=c(NA),Stdv=c(NA))
			row.names(df) <- c("Modularity")
		}
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
	attrs <- c("RelTrajan","Adelectio","SoutHadrien","Espagnol",
		att.list[grepl(att.list,pattern="Cercles")])
	for(attr in attrs)
	{	tmp <- vertex_attr(g0, attr)
		if(all(is.na(cat.data)))
			cat.data <- matrix(as.integer(factor(tmp)),ncol=1)
		else
			cat.data <- cbind(cat.data, as.integer(factor(tmp)))
		colnames(cat.data)[ncol(cat.data)] <- attr
	}
	
	# convert tag-type attributes
	attrs <- c("PolitEques", "MilitSenat", "MilitEques", "DestVoy", "MotifVoy")
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
#print(attr)
#print(cat.data[,i])
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
	attrs <- c("NbrVoy")
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
	# possibly create folder
	attr.folder <- file.path(NET_FOLDER,g$name,"attributes")
	dir.create(path=attr.folder, showWarnings=FALSE, recursive=TRUE)
	
	# retrieve the list of vertex attributes
	att.list <- list.vertex.attributes(g)
	
	#############################
	# deal with categorical attributes
	cat.data <- NA
	
	# gather regular categorical attributes
	attrs <- c("RelTrajan","Adelectio","SoutHadrien","Espagnol",
			att.list[grepl(att.list,pattern="Cercles")])
	for(attr in attrs)
	{	tmp <- vertex_attr(g, attr)
		if(all(is.na(cat.data)))
			cat.data <- matrix(tmp,ncol=1)
		else
			cat.data <- cbind(cat.data, tmp)
		colnames(cat.data)[ncol(cat.data)] <- attr
	}
	
	# convert tag-type attributes
	attrs <- c("PolitEques", "MilitSenat", "MilitEques", "DestVoy", "MotifVoy")
	for(attr in attrs)
	{	tmp <- att.list[grepl(att.list,pattern=attr)]
		m <- sapply(tmp, function(att) vertex_attr(g, att))
		uvals <- sort(unique(c(m)))
		for(uval in uvals)
		{	cat.data <- cbind(cat.data, apply(m, 1, function(v) uval %in% v[!is.na(v)]))
			colnames(cat.data)[ncol(cat.data)] <- paste(attr,uval,sep="_")
		}
	}
	
	# replace NAs by "Unknown" tags
	cat.data[which(is.na(cat.data))] <- "Unknown"
	
	# plot the graph using colors for attribute values
	for(i in 1:ncol(cat.data))
	{	attr <- colnames(cat.data)[i]
		cat("    Plottig attribute \"",attr,"\"\n",sep="")
		# with trajan
		gg <- set_vertex_attr(graph=g, name=attr, value=cat.data[,i])
		custom.gplot(gg,col.att=attr,cat.att=TRUE,color.isolates=TRUE,file=file.path(attr.folder,paste0(attr,"_graph")))
#		custom.gplot(gg,col.att=attr,cat.att=TRUE,color.isolates=TRUE)
		# without trajan
		gg <- set_vertex_attr(graph=g0, name=attr, value=cat.data[,i])
		custom.gplot(gg,col.att=attr,cat.att=TRUE,color.isolates=TRUE,file=file.path(attr.folder,paste0(attr,"_graph0")))
#		custom.gplot(gg,col.att=attr,cat.att=TRUE,color.isolates=TRUE)
	}
	
	#############################
	# deal with numerical attributes
	num.data <- NA
	
	# gather regular numerical attributes
	attrs <- c("NbrVoy")
	for(attr in attrs)
	{	tmp <- vertex_attr(g0, attr)
		if(all(is.na(num.data)))
			num.data <- matrix(tmp,ncol=1)
		else
			num.data <- cbind(num.data, tmp)
		colnames(num.data)[ncol(num.data)] <- attr
	}
	
	# replace NAs by "Unknown" tags
	cat.data[which(is.na(cat.data))] <- "Unknown"
	
	# plot the graph using colors for attribute values
	for(i in 1:ncol(num.data))
	{	attr <- colnames(num.data)[i]
		cat("    Plottig attribute \"",attr,"\"\n",sep="")
		# with trajan
		gg <- set_vertex_attr(graph=g, name=attr, value=cat.data[,i])
		custom.gplot(gg,col.att=attr,cat.att=TRUE,color.isolates=TRUE,file=file.path(attr.folder,paste0(attr,"_graph")))
#		custom.gplot(gg,col.att=attr,cat.att=TRUE,color.isolates=TRUE)
		# without trajan
		gg <- set_vertex_attr(graph=g0, name=attr, value=cat.data[,i])
		custom.gplot(gg,col.att=attr,cat.att=TRUE,color.isolates=TRUE,file=file.path(attr.folder,paste0(attr,"_graph0")))
#		custom.gplot(gg,col.att=attr,cat.att=TRUE,color.isolates=TRUE)
	}
	
	#############################
	# assortativity over
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
	custom.hist(vals, name="articulation", file=file.path(articulation.folder,paste0("articulation_histo")))
	
	# export CSV with articulation
	df <- data.frame(V(g)$name,V(g)$label,vals)
	colnames(df) <- c("Name","Label","Articulation") 
	write.csv(df, file=file.path(articulation.folder,paste0("articulation_values.csv")))
	
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
	custom.gplot(g,col.att="Articulation",file=file.path(articulation.folder,paste0("articulation_graph")))
#	custom.gplot(g,col.att="Articulation")
	
	# export CSV with average articulation
	stat.file <- file.path(NET_FOLDER,g$name,"stats.csv")
	if(file.exists(stat.file))
	{	df <- read.csv(file=stat.file,header=TRUE,row.names=1)
		df["Articulation", ] <- list(Value=NA, Mean=mean(vals), Stdv=sd(vals))
	}
	else
	{	df <- data.frame(Value=c(NA),Mean=c(mean(vals)),Stdv=c(sd(vals)))
		row.names(df) <- c("Articulation")
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
		custom.hist(vals=flat.vals, name="Distance", file=file.path(distance.folder,paste0("distance_histo",sufx)))
		# average distance distribution
		avg.vals <- apply(X=vals,MARGIN=1,FUN=function(v) mean(v[!is.infinite(v)]))
		custom.hist(vals=avg.vals, name="Average distance", file=file.path(distance.folder,paste0("distance_avg_histo",sufx)))
		
		# export CSV with average distance
		df <- data.frame(V(g)$name,V(g)$label,avg.vals)
		colnames(df) <- c("Name","Label","AverageDistance") 
		write.csv(df, file=file.path(distance.folder,paste0("distance_avg_values",sufx,".csv")))
		
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
				custom.gplot(g,col.att="Distance",v.hl=n,file=file.path(distance.folder,paste0("distance_graph",sufx,"_",nname)))
			}
			g <- delete_vertex_attr(graph=g, name="Distance")
		}
		
		# export CSV with average distance
		stat.file <- file.path(NET_FOLDER,g$name,"stats.csv")
		if(file.exists(stat.file))
		{	df <- read.csv(file=stat.file,header=TRUE,row.names=1)
			df["Distance", ] <- list(Value=NA, Mean=mean(flat.vals), Stdv=sd(flat.vals))
		}
		else
		{	df <- data.frame(Value=c(NA),Mean=c(mean(flat.vals)),Stdv=c(sd(flat.vals)))
			row.names(df) <- c("Distance")
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
		custom.hist(vals=flat.vals, name="Connectivity", file=file.path(connectivity.folder,paste0("connectivity_histo",sufx)))
		# connectivity distribution
		avg.vals <- apply(X=vals,MARGIN=1,FUN=function(v) mean(v[!is.infinite(v)]))
		custom.hist(vals=avg.vals, name="Average connectivity", file=file.path(connectivity.folder,paste0("connectivity_avg_histo",sufx)))
		
		# export CSV with average connectivity
		df <- data.frame(V(g)$name,V(g)$label,avg.vals)
		colnames(df) <- c("Name","Label","AverageConnectivity") 
		write.csv(df, file=file.path(connectivity.folder,paste0("connectivity_avg_values",sufx,".csv")))
		
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
				custom.gplot(g,col.att="Connectivity",v.hl=n,file=file.path(connectivity.folder,paste0("connectivity_graph",sufx,"_",nname)))
			}
			g <- delete_vertex_attr(graph=g, name="Connectivity")
		}
		
		# export CSV with average connectivity
		stat.file <- file.path(NET_FOLDER,g$name,"stats.csv")
		if(file.exists(stat.file))
		{	df <- read.csv(file=stat.file,header=TRUE,row.names=1)
			df["Connectivity", ] <- list(Value=NA, Mean=mean(flat.vals), Stdv=sd(flat.vals))
		}
		else
		{	df <- data.frame(Value=c(NA),Mean=c(mean(flat.vals)),Stdv=c(sd(flat.vals)))
			row.names(df) <- c("Connectivity")
		}
		write.csv(df, file=stat.file, row.names=TRUE)
		
		lst[[i]] <- g
	}
	
	return(lst)
}




#############################################################
# Main method for the graph analysis. Uses a predefined layout.
# Generates a bunch of plots and CSV files.
#
# g: graph to process.
#############################################################
analyze.network <- function(g)
{	# set up list of graphs
	g.lst <- list()
	
	# extract various graphs depending on link types
	{	g.lst[["all"]] <- clean.links(g, link.types=c("Nature_Amicale","Nature_Familiale","Nature_Professionnelle",NA))
		g.lst[["all"]]$name <- "all"
		g.lst[["friend"]] <- clean.links(g, link.types="Nature_Amicale")
		g.lst[["friend"]]$name <- "friend"
		g.lst[["family"]] <- clean.links(g, link.types="Nature_Familiale")
		g.lst[["family"]]$name <- "family"
		g.lst[["pro"]] <- clean.links(g, link.types="Nature_Professionnelle")
		g.lst[["pro"]]$name <- "pro"
		g.lst[["unk"]] <- clean.links(g, link.types=NA)
		g.lst[["unk"]]$name <- "unknown"
	}
	
	# process each graph
	for(g in g.lst)
	{	cat("Processing graph '",g$name,"'\n",sep="")
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
		
		# compute attribute stats
		tmp <- analyze.net.attributes(g, g0)
		g <- tmp[[1]]
		g0 <- tmp[[2]]
	}
}
