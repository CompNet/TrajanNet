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
# - multiplex plot of the different types of links?
# - Check the thorough analysis in Rochat2014, see if appropriate here.
# - Legend in plots :
#   - set only 2 decimal digits
#	- add edge color code (top left ?)
#   - add edge line type code
# - PROBLEM WITH THE CONVERSION, P0--P45 STILL MISSING IN ALL-GRAPH
#############################################################################################
# Notes
# - All ;-separated values are in chronological order, but almost no dates are available.
# - We could merge PolitSenat/DerPolitSenat, same for PolitEques/DerPolitEques.
#############################################################################################
