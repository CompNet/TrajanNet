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
library("igraph")			# handles graphs
library("signnet")			# handles signed graphs
library("graphlayouts")		# additional graph layouts
#library("ggraph")
library("SDMTools")			# ?





#############################################################################################
# load auxiliary scripts
source("src/constants.R")
source("src/extraction.R")
source("src/plot.R")
source("src/analysis.R")




#############################################################################################
graph.file <- file.path(NET_FOLDER,"all.graphml")
# possibly extract the network from the tables
if(!file.exists(graph.file))
{	cat("Graph file \"",graph.file,"\" not found: extracting the network and recording it\n",sep="")
	extract.network()
}
# load the extracted network
cat("Loading graph file \"",graph.file,"\"\n",sep="")
g <- read.network(graph.file)



#############################################################################################
# retrieve or compute graph layout
setup.graph.layout(g)




#############################################################################################
# analyze the graph
analyze.network(g)




#############################################################################################
# TODO
# - Compute association measures between attributes
# - Topological measures
#	- Correlation clustering (is it worth it, in this case?)
#	- Cluster analysis based on the sequences, then assortativity of the sequence classes? 
# - Multiplex plot of the different types of links? (didn't find an appropriate tool)
#############################################################################################
# Notes
# - All ;-separated values are in chronological order, but almost no dates are available.
# - We could merge PolitSenat/DerPolitSenat, same for PolitEques/DerPolitEques.
#############################################################################################
