#############################################################################################
# Main function for loading the raw data, extracting the networks, analyzing the sequences.
# 
# 01/2019 Vincent Labatut
#
# setwd("C:/users/Vincent/Eclipse/workspaces/Networks/TrajanNet")
# setwd("~/eclipse/workspaces/Networks/TrajanNet")
# source("src/main.R")
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
library("CINNA")			# harmonic closeness




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
#custom.gplot(og, file="test.pdf")




#############################################################################################
# perform graph analysis
analyze.network(og)




#############################################################################################
# perform sequence analysis
analyze.sequences()
