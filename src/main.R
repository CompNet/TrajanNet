#############################################################################################
# Main function for loading the raw data and extract the networks.
# 
# 01/2019 Vincent Labatut
#
# setwd("C:/users/Vincent/Eclipse/workspaces/Networks/TrajanNet")
# setwd("~/eclipse/workspaces/Networks/NetVotes")
# source("src/main.R")
#############################################################################################
# load libraries
library("igraph")
library("graphlayouts")
library("ggraph")
library("SDMTools")




#############################################################################################
# load auxiliary scripts
source("src/constants.R")
source("src/extraction.R")
source("src/plot.R")
source("src/analysis.R")




#############################################################################################
# try to load the network if the file exists
graph.file <- file.path(NET_FOLDER,"all.graphml")
if(file.exists(graph.file))
{	cat("Loading graph file \"",graph.file,"\"\n",sep="")
	g <- read.network()
# otherwise, extract the network from the tables
}else
{	cat("Graph file \"",graph.file,"\" not found: extracting and recording the it\n",sep="")
	g <- extract.network()
}




#############################################################################################
# retrieve or compute graph layout
setup.graph.layout(g)




#############################################################################################
# analyze the graph
analyze.network(g)




#############################################################################################
# TODO
# - Check that the relationship comments in the big file are still at the right place (e.g. P5--P63)
# - Perform a basic descriptive analysis of the vertex attributes (>> add to assortativity function?)
# - Multiplex plot of the different types of links? (didn't find an appropriate tool)
# - Check the thorough analysis in Rochat2014, see which parts are relevant in our case.
# - Topological measures
#	- Correlation clustering (is it worth it, in this case?)
#	- Cluster analysis based on the sequences, then assortativity of the sequence classes? 
#############################################################################################
# Notes
# - All ;-separated values are in chronological order, but almost no dates are available.
# - We could merge PolitSenat/DerPolitSenat, same for PolitEques/DerPolitEques.
#############################################################################################
