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
	
	for(e in E(g))
	{	for(lt in link.types)
		{	if(get.edge.attribute(g,lt,e))
			{	uv = ends(g, E(g)[e], names=FALSE)
				res <- add_edges(res, edges=uv, attr=list(
								"Polarité"=get.edge.attribute(g,lt,e),
								"Type"=lt))
			}
		}
	}
	
	return(res)
}




#############################################################
# Computes the diameter, the corresponding paths, and plots them.
#
# g: graph to process.
#############################################################
analyze.net.diameter <- function(g)
{	# compute diameter
	diam <- diameter(g)						# get the network diameter
	dd <- distances(graph=g)				# compute all inter-node distances
	idx <- which(dd==diam, arr.ind=TRUE)	# retrieve pairs of node matching the diameter 
	idx <- idx[idx[,1]<idx[,2],]			# filter (each pair appears twice due to symmetric matrix)
	
	# possibly create folder
	diameter.folder <- file.path(NET_FOLDER,g$name,"diameter")
	dir.create(path=diameter.folder, showWarnings=FALSE, recursive=TRUE)
	
	# plot diameter
	diam.paths <- lapply(1:nrow(idx), function(r) all_shortest_paths(graph=g, from=idx[r,1], to=idx[r,2])$res)
	for(pp in 1:length(diam.paths))
	{	custom.gplot(g, paths=diam.paths[[pp]], file=file.path(diameter.folder,paste0("graph0_diam_",pp)))
		
		q <- 1
		for(p in 1:length(diam.paths[[pp]]))
		{	if(p==1 || !all(diam.paths[[pp]][[p]]==diam.paths[[pp]][[p-1]]))
			{	custom.gplot(g, paths=diam.paths[[pp]][[p]], file=file.path(diameter.folder,paste0("graph0_diam_",pp,"_",q)))
				q <- q + 1
			}
		}
	}
}




#############################################################
# Main method for the graph analysis. Uses a predefined layout.
# Generates a bunch of plots and CSV files.
#
# g: graph to process.
# g0: same graph without the main node.
#############################################################
analyze.net.degree <- function(g, g0)
{	# possibly create folder
	degree.folder <- file.path(NET_FOLDER,g$name,"degree")
	dir.create(path=degree.folder, showWarnings=FALSE, recursive=TRUE)
	
	lst <- list(g,g0)
	sufxx <- c("","0")
	for(i in 1:length(lst))
	{	g <- lst[[i]]
		sufx <- sufxx[i]
		
		# degree distribution
		vals <- degree(g)
		custom.hist(vals, name="Degree", file=file.path(degree.folder,paste0("degree_histo",sufx)))
			
		# export CSV with degree
		df <- data.frame(V(g)$name,V(g)$label,vals)
		colnames(df) <- c("Name","Label","Degree") 
		write.csv(df, file=file.path(degree.folder,paste0("degree_values",sufx,".csv")))
		
		# plot graph using color for degree
		custom.gplot(g,vvals=vals,file=file.path(degree.folder,paste0("degree_graph",sufx)))
		custom.gplot(g,vvals=vals)
		
		# also add (a node attribute) to the graph file
		V(g)$degree <- vals
		write.graph(graph=g, file=file.path(NET_FOLDER,g$name,paste0("graph",sufx,".graphml")), format="graphml")
	}
}




#############################################################
# Main method for the graph analysis. Uses a predefined layout.
# Generates a bunch of plots and CSV files.
#
# g: graph to process.
#############################################################
analyze.network <- function(g)
{	# read the original graph
#	graph.file <- file.path(NET_FOLDER,"all.graphml")
#	g <- read.graph(graph.file,format="graphml")
	
	# tentative of using predefined layouts
#	lay <- layout_with_fr(g)
#	lay <- layout_with_fr(g, kkconst=0)
	
	# old code used to setup the layout
#	tkplot(g)
#	coord <- tk_coords(3)
#	write.table(x=coord,file="data/nets/all_layout.txt")
	
	# read the layout
	lay <- as.matrix(read.table(file=file.path(NET_FOLDER,"all_layout.txt")))
	
	# set up list of graphs
	g.lst <- list()
	
	# extract various graphs depending on link types
	{	g.lst[["all"]] <- clean.links(g, link.types=c("Nature_Amicale","Nature_Familiale","Nature_Professionnelle"))
		g.lst[["all"]]$name <- "all"
		g.lst[["friend"]] <- clean.links(g, link.types="Nature_Amicale")
		g.lst[["friend"]]$name <- "friend"
		g.lst[["family"]] <- clean.links(g, link.types="Nature_Familiale")
		g.lst[["family"]]$name <- "family"
		g.lst[["pro"]] <- clean.links(g, link.types="Nature_Professionnelle")
		g.lst[["pro"]]$name <- "pro"
	}
	
	# process each graph
	for(g in g.lst)
	{	# create graph-specific folder
		tmp.folder <- file.path(NET_FOLDER, g$name)
		dir.create(path=tmp.folder, showWarnings=FALSE, recursive=TRUE)
		
		# record graph as a graphml file
		write.graph(graph=g, file=file.path(tmp.folder,"graph.graphml"), format="graphml")
		
		# plot full graph
		custom.gplot(g, file=file.path(tmp.folder,"graph"))
		custom.gplot(g)
		
		# delete trajan's links for better visibility
		# TODO maybe better to just draw them using a light color?
		g0 <- g
#		g0 <- delete_vertices(g,1)		# delete trajan (not good, plot-wise)
		es <- incident(graph=g0, v=1, mode="all")
		g0 <- delete.edges(g0,es)	# delete traj's links
		custom.gplot(g0, file=file.path(tmp.folder,"graph0"))
		custom.gplot(g0)
		write.graph(graph=g0, file=file.path(tmp.folder,"graph0.graphml"), format="graphml")
		
		# compute diameters
		analyze.net.diameter(g0)
		
		# compute eccentricity
		# TODO
		
		# compute degree
		analyze.net.degree(g, g0)
	}
}




# TODO
# analysis: focus on eccentricity rather than diameter?
