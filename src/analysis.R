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
	myplot <- function(g)
	{	
		# edge colors
		ecols <- rep("BLACK", gsize(g))
		ecols[E(g)$Nature_Amicale] <- "#1a8f39"			# green
		ecols[E(g)$Nature_Familiale] <- "#9c1699"		# purple
		ecols[E(g)$Nature_Professionnelle] <- "#c27604"	# orange
		# edge style
		elty <- 1										# solid
		elty[E(g)$Polarité=="Négative"] <- 3			# dotted
		
		plot(g,
			layout=lay,
			vertex.size=5, 
			vertex.color="GREY",
			edge.color=ecols,
			edge.lty=elty,
			edge.width=2 
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


read.graph("data/nets/all.graphml",format="graphml")
lay <- layout_with_fr(g)
myplot(g)

#g0 <- delete_vertices(g,1)	# delete trajan
neis <- as.integer(ego(graph=g,nodes=1,mindist=1)[[1]])
es <- c(rbind(rep(1,length(neis)), neis))
g0 <- delete.edges(g,es)	# delete traj's links
myplot(g0)

diam <- diameter(g0)					# get the network diameter
dd <- distances(graph=g0)				# compute all inter-node distances
idx <- which(dd==diam, arr.ind=TRUE)	# retrieve pairs of node matching the diameter 
idx <- idx[idx[,1]<idx[,2],]			# filter (each pair appears twice du to symmetric matrix)


lapply(1:nrow(idx), function(r) all_shortest_paths(graph=g0, from=idx[r,1], to=idx[r,2]))

# voir comment diminuer le nbre de PCC pour chaque diamètre
# améliorer la visu pour éviter le chevauchement entre les noeuds/texte
