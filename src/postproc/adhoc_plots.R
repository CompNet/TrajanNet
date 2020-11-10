#############################################################################################
# Script used to generate the ad hoc plots used in the paper.
#
# 10/2020 Vincent Labatut
#############################################################################################
# load libraries
library("igraph")			# handles graphs
library("signnet")			# handles signed graphs
library("graphlayouts")		# additional graph layouts
library("ggraph")			# additional graph plotting features
library("SDMTools")			# ?
library("scales")			# convert colors
library("circlize")			# circos-type plots
library("TraMineR")			# sequence analysis
library('plot.matrix')		# plot matrices
library('alluvial')			# alluvial diagrams
library('cluster')			# cluster analysis
library("dendextend")		# additional dendrogram-related features




#############################################################################################
# load auxiliary scripts
source("src/common/constants.R")
source("src/graphs/graph_extraction.R")
source("src/graphs/graph_analysis.R")
source("src/graphs/signed_graph_functions.R")
source("src/common/plot.R")
source("src/sequences/sequence_analysis.R")




#############################################################################################
graph.file <- file.path(NET_FOLDER,"all.graphml")
# possibly extract the network from the tables
if(!file.exists(graph.file))
{	cat("Graph file \"",graph.file,"\" not found: extracting the network and recording it\n",sep="")
	og <- extract.network()
}
# load the extracted network
cat("Loading graph file \"",graph.file,"\"\n",sep="")
og <- read.network(graph.file)
og <- delete_vertex_attr(og, "x")
og <- delete_vertex_attr(og, "y")




#############################################################################################
# retrieve or compute graph layout
setup.graph.layout(og)




#############################################################################################
# get the different graphs
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




#############################################################################################
# process each graph
for(g in g.lst)
{	# g <- g.lst[[1]]
	cat("Processing graph '",g$name,"'\n",sep="")
	
	# create graph-specific folder
	tmp.folder <- file.path(NET_FOLDER, g$name)
	dir.create(path=tmp.folder, showWarnings=FALSE, recursive=TRUE)
	
	# plot full graph
	custom.gplot(g, file=file.path(tmp.folder,paste0(g$name,"_graph")))
	#custom.gplot(g)
	
	# delete trajan's links for better visibility
	g0 <- disconnect.nodes(g, nodes=1)
	custom.gplot(g0, file=file.path(tmp.folder,paste0(g$name,"_graph0")))
	#custom.gplot(g0)
}




#############################################################################################
# extract and process the signed graphs
sg.lst <- flatten.signed.graph(og)
for(sg in sg.lst)
{	#sg <- sg.lst[[1]]
	cat("Processing graph '",sg$name,"'\n",sep="")
	
	# create graph-specific folder
	tmp.folder <- file.path(SIGNED_FOLDER, sg$name)
	dir.create(path=tmp.folder, showWarnings=FALSE, recursive=TRUE)
	
	# plot the signed graph
	custom.gplot(sg, file=file.path(tmp.folder,paste0(sg$name,"_graph")))
	#custom.gplot(sg)
	
	# get the version without Trajan
	sg0 <- disconnect.nodes(sg, nodes=1)
	custom.gplot(sg0, file=file.path(tmp.folder,paste0(sg$name,"_graph0")))
	#custom.gplot(sg0)
}	
