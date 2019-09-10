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
	elty[!is.na(E(g)$"Polarité") & E(g)$"Polarité"=="Négative"] <- 3			# dotted
	# set edge width
	ewidth <- rep(1,gsize(g))
	
	# possibly change the color of the highlighted path
	if(hasArg(paths))
	{	if(!is.list(paths))
			paths <- list(paths)
		for(path in paths)
		{	v <- NA
			for(n in path)
			{	if(is.na(v))
					v <- n
				else
				{	u <- v
					v <- n
					idx <- as.integer(E(g)[u %--% v])
					ecols[idx] <- "RED"
					ewidth[idx] <- 2
				}
			}
		}
	}
	
	if(hasArg(file))
		pdf(file, width=25, height=25)
	plot(g,
		layout=lay,
		vertex.size=5, 
		vertex.color="GREY",
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
analyze.network <- function(g)
{	
	
}


# read the graph
graph.file <- file.path(net.folder,"all.graphml")
g <- read.graph(graph.file,format="graphml")

# tentative of using predefined layouts
#lay <- layout_with_fr(g)
#lay <- layout_with_fr(g, kkconst=0)

# old code used to setup the layout
#tkplot(g)
#coord <- tk_coords(3)
#write.table(x=coord,file="data/nets/all_layout.txt")

# read the layout and plot
lay <- as.matrix(read.table(file="data/nets/all_layout.txt"))
myplot(g, file=file.path(net.folder,"all.pdf"))
myplot(g)

# only keep certain types of links / possibly duplicate certain links
g <- clean.links(g, link.types=c("Nature_Amicale",
				"Nature_Familiale",
				"Nature_Professionnelle"))	# Nature_Amicale Nature_Familiale Nature_Professionnelle

# delete trajan's links for better visibility
# TODO maybe better to just draw them using a light color?
#g0 <- delete_vertices(g,1)	# delete trajan
neis <- as.integer(ego(graph=g,nodes=1,mindist=1)[[1]])
es <- c(rbind(rep(1,length(neis)), neis))
g0 <- delete.edges(g,es)	# delete traj's links
myplot(g0, file=file.path(net.folder,"all_0.pdf"))
myplot(g0) # TODO pb: ne supprime pas tous les liens !

# work on the diameter
diam <- diameter(g0)					# get the network diameter
dd <- distances(graph=g0)				# compute all inter-node distances
idx <- which(dd==diam, arr.ind=TRUE)	# retrieve pairs of node matching the diameter 
idx <- idx[idx[,1]<idx[,2],]			# filter (each pair appears twice du to symmetric matrix)

diam.paths <- lapply(1:nrow(idx), function(r) all_shortest_paths(graph=g0, from=idx[r,1], to=idx[r,2])$res)
myplot(g0, diam.paths[[1]][[1]])
myplot(g0, diam.paths[[1]])

# TODO
# prétraitement
# - fonction qui extrait un graphe avec un seul type de lien
# - même chose mais avec plusieurs types, avec liens multiples pour qu'ils apparaissent séparément dans les graphiques
# analyse : regarder plutot l'excentricité ?
