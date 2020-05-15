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
source("src/extraction.R")
source("src/plot.R")
source("src/analysis.R")
source("src/sequences.R")
source("src/signed_graph_functions.R")




#############################################################################################
graph.file <- file.path(NET_FOLDER,"all.graphml")
# possibly extract the network from the tables
if(!file.exists(graph.file))
{	cat("Graph file \"",graph.file,"\" not found: extracting the network and recording it\n",sep="")
	extract.network()
}
# load the extracted network
cat("Loading graph file \"",graph.file,"\"\n",sep="")
og <- read.network(graph.file)




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
# - fermeture : 
#   > faire ça de façon synchrone
#   > générer les différentes étapes (graphiques)
#
# 3.1: similarité structurelle, pour objectiver un peu ?
# - entre les espagnols vs. le reste du graphe
# - mais aussi homophilie

# TODO séquences
# - calculer la distance entre chaque séq et les 2 séq types
# - faire une représentation graphique 2D genre LLE

# TODO
# - séparer sénateurs de chevaliers, une fois confirmé par GV
