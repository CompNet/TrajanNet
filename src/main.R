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
{	g <- read.graph(graph.file,format="graphml")
# otherwise, extract the network from the tables
}else
	g <- extract.network()




#############################################################################################
# try to use ego-network layout, but not appropriate here
#p1 <- ggraph(g, layout="focus", v=1) +
#		draw_circle(use = "focus", max.circle = 3)+
#		geom_edge_link(edge_color="black",edge_width=0.3)+
#		geom_node_point(aes(fill=as.factor(V(g)$RelTrajan)),size=2,shape=21)+
#		scale_fill_manual(values=c("#8B2323", "#EEAD0E", "#34CB34", "#3366FF"))+
#		theme_graph()+
#		theme(legend.position = "none")+
#		coord_fixed()+
#		labs(title= "Trajan's ego-network")
# https://cran.r-project.org/web/packages/graphlayouts/vignettes/introduction.html
# https://cran.r-project.org/web/packages/graphlayouts/index.html
# https://cran.r-project.org/web/packages/ggraph/index.html

# try to read the layout if the file exists
lay.file <- file.path(NET_FOLDER,"all_layout.txt")
if(file.exists(lay.file))
{	lay <- as.matrix(read.table(file=lay.file))
# otherwise, compute the layout
}else
{	# use a  predefined layout
#	lay <- layout_with_fr(g)
	lay <- layout_with_fr(g, kkconst=0)
	
	# old code used to manually refine the layout
	#	tkplot(g, layout=lay)
	#	lay <- tk_coords(3)
	
	write.table(x=lay,file=lay.file)
}	




#############################################################################################
# analyze network
analyze.network(g)






#############################################################################################
# TODO
# Instead of assigning attributes to links, just create distinct links with a "type" attribute (?)
# Then extract separate graphs, as Gephi doesn't allow multiple links.
# Check the thorough analysis in Rochat2014, see if appropriate here.
#############################################################################################
# Notes
# - All ;-separated values are in chronological order, but almost no dates are available.
# - We could merge PolitSenat/DerPolitSenat, same for PolitEques/DerPolitEques.
#############################################################################################
