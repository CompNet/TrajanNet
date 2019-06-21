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
library("graphlayouts")
library("ggraph")

# load other scripts
source("src/extraction.R")


# folder names
data.folder <- "data"
table.folder <- file.path(data.folder,"tables")
net.folder <- file.path(data.folder,"nets")


# extract network
g <- extract.network()

# plot network
p1 <- ggraph(g, layout="focus", v=1) +
		draw_circle(use = "focus", max.circle = 3)+
		geom_edge_link(edge_color="black",edge_width=0.3)+
		geom_node_point(aes(fill=as.factor(V(g)$RelTrajan)),size=2,shape=21)+
		scale_fill_manual(values=c("#8B2323", "#EEAD0E", "#34CB34", "#3366FF"))+
		theme_graph()+
		theme(legend.position = "none")+
		coord_fixed()+
		labs(title= "Trajan's ego-network")
# https://cran.r-project.org/web/packages/graphlayouts/vignettes/introduction.html
# https://cran.r-project.org/web/packages/graphlayouts/index.html
# https://cran.r-project.org/web/packages/ggraph/index.html


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




















