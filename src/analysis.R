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
			vv <- get.edge.attribute(g,lt,e)
			pol <- get.edge.attribute(g,"Polarite",e)
			
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
				pol <- get.edge.attribute(g,"Polarite",e)
				
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
# Computes the diameter, the corresponding paths, and plots them.
# Same thing for radius and eccentricity.
#
# g: graph to process.
#############################################################
analyze.net.eccentricity <- function(g0)
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
	{	custom.gplot(g0, paths=diam.paths[[pp]], file=file.path(diameter.folder,paste0("graph0_diam_",pp)))
		
		q <- 1
		for(p in 1:length(diam.paths[[pp]]))
		{	if(p==1 || !all(diam.paths[[pp]][[p]]==diam.paths[[pp]][[p-1]]))
			{	custom.gplot(g0, paths=diam.paths[[pp]][[p]], file=file.path(diameter.folder,paste0("graph0_diam_",pp,"_",q)))
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
	V(g0)$Eccentricity <- vals
	
	# plot graph using color for eccentricity
	custom.gplot(g0,col.att="Eccentricity",file=file.path(eccentricity.folder,"eccentricity_graph0"))
#	custom.gplot(g0,col.att="Eccentricity")
	
	###########################
	# compute radius
	rad <- min(vals[vals>0])
	
	# add radius and diameter to the graph (as attributes) and record
	g0$diameter <- diam
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
	write.graph(graph=g0, file=file.path(NET_FOLDER,g0$name,"graph0.graphml"), format="graphml")
	
	return(g0)
}




#############################################################
# Computes degree and generates plots and CSV files.
#
# g: graph to process.
# g0: same graph without the main node.
#############################################################
analyze.net.degree <- function(g, g0)
{	cat("  Computing degree\n")
	# possibly create folder
	degree.folder <- file.path(NET_FOLDER,g$name,"degree")
	dir.create(path=degree.folder, showWarnings=FALSE, recursive=TRUE)
	
	lst <- list(g, g0)
	sufxx <- c("","0")
	for(i in 1:length(lst))
	{	cat("  Processing graph ",i,"/",length(lst),"\n",sep="")
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
		stat.file <- file.path(NET_FOLDER,g0$name,"stats.csv")
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
# g: graph to process.
# g0: same graph without the main node.
#############################################################
analyze.net.eigen <- function(g, g0)
{	cat("  Computing Eigencentrality\n")
	# possibly create folder
	eigen.folder <- file.path(NET_FOLDER,g$name,"eigen")
	dir.create(path=eigen.folder, showWarnings=FALSE, recursive=TRUE)
	
	lst <- list(g, g0)
	sufxx <- c("","0")
	for(i in 1:length(lst))
	{	cat("  Processing graph ",i,"/",length(lst),"\n",sep="")
		g <- lst[[i]]
		sufx <- sufxx[i]
		
		# Eigencentrality distribution
		vals <- eigen_centrality(graph=g, scale=FALSE)$vector
		custom.hist(vals, name="Eigencentrality", file=file.path(eigen.folder,paste0("eigen_histo",sufx)))
		
		# export CSV with Eigencentrality
		df <- data.frame(V(g)$name,V(g)$label,vals)
		colnames(df) <- c("Name","Label","Eigencentrality") 
		write.csv(df, file=file.path(eigen.folder,paste0("eigen_values",sufx,".csv")))
		
		# add results to the graph (as attributes) and record
		V(g)$Eigencentrality <- vals
		g$EigencentralityAvg <- mean(vals)
		g$EigencentralityStdv <- sd(vals)
		write.graph(graph=g, file=file.path(NET_FOLDER,g$name,paste0("graph",sufx,".graphml")), format="graphml")
		
		# plot graph using color for Eigencentrality
		custom.gplot(g,col.att="Eigencentrality",file=file.path(eigen.folder,paste0("eigen_graph",sufx)))
#		custom.gplot(g,col.att="Eigencentrality")
		
		# export CSV with average Eigencentrality
		stat.file <- file.path(NET_FOLDER,g0$name,"stats.csv")
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
# g: graph to process.
# g0: same graph without the main node.
#############################################################
analyze.net.betweenness <- function(g, g0)
{	cat("  Computing betweenness\n")
	# possibly create folder
	betweenness.folder <- file.path(NET_FOLDER,g$name,"betweenness")
	dir.create(path=betweenness.folder, showWarnings=FALSE, recursive=TRUE)
	
	lst <- list(g, g0)
	sufxx <- c("","0")
	for(i in 1:length(lst))
	{	cat("  Processing graph ",i,"/",length(lst),"\n",sep="")
		g <- lst[[i]]
		sufx <- sufxx[i]
		
		# betweenness distribution
		vals <- betweenness(graph=g, normalized=FALSE)
		custom.hist(vals, name="betweenness", file=file.path(betweenness.folder,paste0("betweenness_histo",sufx)))
		
		# export CSV with betweenness
		df <- data.frame(V(g)$name,V(g)$label,vals)
		colnames(df) <- c("Name","Label","betweenness") 
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
		stat.file <- file.path(NET_FOLDER,g0$name,"stats.csv")
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
# Computes average distances and generates plots and CSV files.
#
# g: graph to process.
# g0: same graph without the main node.
#############################################################
analyze.net.distance <- function(g, g0)
{	cat("  Computing average distances\n")
	# possibly create folder
	distance.folder <- file.path(NET_FOLDER,g$name,"distance")
	dir.create(path=distance.folder, showWarnings=FALSE, recursive=TRUE)
	
	lst <- list(g, g0)
	sufxx <- c("","0")
	for(i in 1:length(lst))
	{	cat("  Processing graph ",i,"/",length(lst),"\n",sep="")
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
		
		# plot graph using color for average distance
		custom.gplot(g,col.att="AverageDistance",file=file.path(distance.folder,paste0("distance_avg_graph",sufx)))
#		custom.gplot(g,col.att="AverageDistance")
		
		# export CSV with average distance
		stat.file <- file.path(NET_FOLDER,g0$name,"stats.csv")
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
		g0 <- g
#		g0 <- delete_vertices(g,1)		# delete trajan (not good, plot-wise)
		es <- incident(graph=g0, v=1, mode="all")
		g0 <- delete.edges(g0,es)	# delete trajan's links
		custom.gplot(g0, file=file.path(tmp.folder,"graph0"))
#		custom.gplot(g0)
		write.graph(graph=g0, file=file.path(tmp.folder,"graph0.graphml"), format="graphml")
		
		# compute diameters, eccentricity, radius
		g0 <- analyze.net.eccentricity(g0)
		
		# compute degree
		tmp <- analyze.net.degree(g, g0)
		g <- tmp[[1]]
		g0 <- tmp[[2]]
		
		# compute eigencentrality
		tmp <- analyze.net.eigen(g, g0)
		g <- tmp[[1]]
		g0 <- tmp[[2]]
		
		# compute betweenness
		tmp <- analyze.net.betweenness(g, g0)
		g <- tmp[[1]]
		g0 <- tmp[[2]]
		
		# compute average distances
		tmp <- analyze.net.distance(g, g0)
		g <- tmp[[1]]
		g0 <- tmp[[2]]
		
		# TODO		
	}
}
