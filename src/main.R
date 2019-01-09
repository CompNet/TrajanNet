#############################################################################################
# Main function for loading the raw data and extract the networks.
# 
# 01/2019 Vincent Labatut
#
# setwd("D:/Eclipse/workspaces/Networks/NetVotes")
# setwd("~/eclipse/workspaces/Networks/NetVotes")
# source("src/main.R")
#############################################################################################
# load libraries
library("igraph")

# load other scripts
source("src/extraction.R")


# folder names
data.folder <- "data"
table.folder <- file.path(data.folder,"tables")
net.folder <- file.path(data.folder,"nets")


# extract network
g <- extract.network()


# analyze network

