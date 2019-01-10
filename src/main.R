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



# TODO
# Instead of assigning attributes to links, just create distinct links with a "type" attribute (?)
# Then extract separate graphs, as Gephi doesn't allow multiple links.

# Notes
# - All ;-separated values are in chronological order, but almost no dates are available.
# - Maybe we could merge PolitSenat/DerPolitSenat, same for PolitEques/DerPolitEques.
# - Cannot merge PolitSenat with PolitEques (or DerPolit variants) because some exceptional people can have both (successively).
# - Same for MilitSenat and MilitEques (probably, TBC).
# - Circles are not mutually exclusive.




















