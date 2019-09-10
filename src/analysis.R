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
# xxx
#
# xxx:xxx.
#############################################################
myplot <- function(g, paths)
{	
	# set edge colors
	ecols <- rep("BLACK", gsize(g))
	ecols[E(g)$Nature_Amicale] <- "#1a8f39"			# green
	ecols[E(g)$Nature_Familiale] <- "#9c1699"		# purple
	ecols[E(g)$Nature_Professionnelle] <- "#c27604"	# orange
	# set edge style
	elty <- 1										# solid
	elty[E(g)$Polarité=="Négative"] <- 3			# dotted
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
	
	plot(g,
		layout=lay,
		vertex.size=5, 
		vertex.color="GREY",
		edge.color=ecols,
		edge.lty=elty,
		edge.width=ewidth
	)
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
g <- read.graph("data/nets/all.graphml",format="graphml")

# tentative of using predefined layouts
#lay <- layout_with_fr(g)
#lay <- layout_with_fr(g, kkconst=0)

# old code used to setup the layout
#tkplot(g)
#coord <- tk_coords(3)
#write.table(x=coord,file="data/nets/all_layout.txt")

# read the layout and plot
lay <- as.matrix(read.table(file="data/nets/all_layout.txt"))
myplot(g)

# delete trajan's links for better visibility
# TODO maybe better to just draw them using a light color?
#g0 <- delete_vertices(g,1)	# delete trajan
neis <- as.integer(ego(graph=g,nodes=1,mindist=1)[[1]])
es <- c(rbind(rep(1,length(neis)), neis))
g0 <- delete.edges(g,es)	# delete traj's links
myplot(g0)

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
