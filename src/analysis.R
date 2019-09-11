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
# Displays the specified graph in an appropraite way, taking
# into account the previously set link and node attributes.
#
# g: graph to plot.
# paths: (optional) paths to highlight while plotting. This parameter
# 		 is either a list of integer vectors (node sequences), or
# 		 an integer vector if there is only one path to plot.
# file: (optional) file name, to record the plot.
#############################################################
myplot <- function(g, paths, file)
{	
	# set edge colors
	ecols <- rep("BLACK", gsize(g))
	ecols[E(g)$Type=="Nature_Amicale"] <- "#1a8f39"			# green
	ecols[E(g)$Type=="Nature_Familiale"] <- "#9c1699"		# purple
	ecols[E(g)$Type=="Nature_Professionnelle"] <- "#c27604"	# orange
	# set edge style
	elty <- rep(1,gsize(g))										# solid
	elty[!is.na(E(g)$Polarite) & E(g)$"Polarité"=="Négative"] <- 3			# dotted
	# set edge width
	ewidth <- rep(1,gsize(g))
	# set node outline color
	outline.cols <- rep("BLACK",gorder(g))
	
	# possibly change the color of the highlighted path
	if(hasArg(paths))
	{	if(!is.list(paths))
			paths <- list(paths)
		for(path in paths)
		{	v <- NA
			for(n in path)
			{	if(is.na(v))
				{	v <- n
					outline.cols[v] <- "RED"
				}
				else
				{	u <- v
					v <- n
					idx <- as.integer(E(g)[u %--% v])
					ecols[idx] <- "RED"
					ewidth[idx] <- 2
				}
			}
			outline.cols[v] <- "RED"
		}
	}
	
	if(hasArg(file))
		pdf(file, width=25, height=25)
	plot(g,
		layout=lay,
		vertex.size=5, 
		vertex.color="GREY",
		vertex.frame.color=outline.cols,
		edge.color=ecols,
		edge.lty=elty,
		edge.width=ewidth
	)
	if(hasArg(file))
		dev.off()
}




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
# xxx
#
# xxx:xxx.
#############################################################
analyze.net.diameter <- function(g)
{	# work on the diameter
	diam <- diameter(g)					# get the network diameter
	dd <- distances(graph=g)				# compute all inter-node distances
	idx <- which(dd==diam, arr.ind=TRUE)	# retrieve pairs of node matching the diameter 
	idx <- idx[idx[,1]<idx[,2],]			# filter (each pair appears twice due to symmetric matrix)
	# plot diameter
	diameter.folder <- file.path(net.folder,"diameter")
	dir.create(path=diameter.folder, showWarnings=FALSE, recursive=TRUE)
	diam.paths <- lapply(1:nrow(idx), function(r) all_shortest_paths(graph=g, from=idx[r,1], to=idx[r,2])$res)
	for(pp in 1:length(diam.paths))
	{	for(p in 1:length(diam.paths[[pp]]))
			myplot(g, diam.paths[[pp]][[p]], file=file.path(diameter.folder,paste0("all0_diam_",pp,"_",p,".pdf")))
		myplot(g, diam.paths[[pp]], file=file.path(diameter.folder,paste0("all0_diam_",pp,".pdf")))
	}
}




#############################################################
# xxx
#
# xxx:xxx.
#############################################################
analyze.network <- function(g)
{	# read the original graph
	graph.file <- file.path(net.folder,"all.graphml")
	g <- read.graph(graph.file,format="graphml")
	
	# tentative of using predefined layouts
#	lay <- layout_with_fr(g)
#	lay <- layout_with_fr(g, kkconst=0)
	
	# old code used to setup the layout
#	tkplot(g)
#	coord <- tk_coords(3)
#	write.table(x=coord,file="data/nets/all_layout.txt")
	
	# only keep certain types of links / possibly duplicate certain links
	g <- clean.links(g, link.types=c("Nature_Amicale",
					"Nature_Familiale",
					"Nature_Professionnelle"))	# Nature_Amicale Nature_Familiale Nature_Professionnelle
	
	# read the layout and plot full graph
	lay <- as.matrix(read.table(file="data/nets/all_layout.txt"))
	myplot(g, file=file.path(net.folder,"all.pdf"))
	myplot(g)
	
	# delete trajan's links for better visibility
	# TODO maybe better to just draw them using a light color?
	g0 <- g
#	g0 <- delete_vertices(g,1)		# delete trajan (not good, plot-wise)
	es <- incident(graph=g0, v=1, mode="all")
	g0 <- delete.edges(g0,es)	# delete traj's links
	myplot(g0, file=file.path(net.folder,"all0.pdf"))
	myplot(g0)
	
	# plot diameters
	analyze.net.diameter(g0)
}




# TODO
# loop over all types of links in the main function (as well as with/without Trajan)
# maybe extract them in the main.R file instead, pass them to the analysis script 
# setup folders for each type of link+all
# analysis: focus on eccentricity rather than diameter?
