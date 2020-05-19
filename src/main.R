#############################################################################################
# Main function for loading the raw data, extracting the networks, analyzing the sequences.
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
source("src/constants.R")
source("src/graph_extraction.R")
source("src/graph_analysis.R")
source("src/plot.R")
source("src/sequence_analysis.R")
source("src/signed_graph_functions.R")




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
# perform graph analyze
analyze.network(og)




#############################################################################################
# perform sequence analysis
analyze.sequences()

# TODO
# sequences:
# - séparer sénateurs de chevaliers, une fois confirmé par GV
#
# partitionnement des graphes signés:
# - communicabilité
#   - problème de numérotation des noeuds, qui ne matche pas l'ordre original

# séparer chev et sénateurs (graphe, matrice de trans)
