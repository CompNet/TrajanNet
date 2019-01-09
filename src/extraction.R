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
source("src/preprocess.R")


# folder names
data.folder <- "data"


# load node attributes
attr.file <- file.path(data.folder,"trajan_attributes.csv")
attr.data <- as.matrix(read.csv(file=attr.file,header=TRUE,check.names=FALSE))
trojan.data <- rep(NA,ncol(attr.data))
trojan.data[1] <- "P0"
attr.data <- rbind(trojan.data,attr.data)
rownames(attr.data) <- NULL


# create empty graph
g <- graph.empty(n=nrow(attr.data),directed=FALSE)
# add node attributes
V(g)$name <- attr.data[,"Id"]
g <- breakdown.attribute(g, attr.data[,"Senat"], name="Senat")
g <- breakdown.attribute(g, attr.data[,"SenatDer"], name="SenatDer")
g <- breakdown.attribute(g, attr.data[,"Eques"], name="Eques")
g <- breakdown.attribute(g, attr.data[,"DerEques"], name="DerEques")
V(g)$Adelectio <- sapply(attr.data[,"Adelectio"], function(val) val=="Oui")
g <- breakdown.attribute(g, attr.data[,"MilitSenat"], name="MilitSenat")
g <- breakdown.attribute(g, attr.data[,"MilitEques"], name="MilitEques")
V(g)$NbrVoy <- as.integer(attr.data[,"NbrVoy"])
g <- breakdown.attribute(g, attr.data[,"DestVoy"], name="DestVoy")
g <- breakdown.attribute(g, attr.data[,"MotifVoy"], name="MotifVoy")
V(g)$RelTrajan <- attr.data[,"RelTrajan"]
V(g)$SoutHadrien <- sapply(attr.data[,"SoutHadrien"], function(val) val=="Oui")
g <- breakdown.attribute(g, attr.data[,"Cercles"], name="Cercles")
V(g)$Espagnol <- sapply(attr.data[,"Espagnol"], function(val) val=="Oui")


# load relations
rel.file <- file.path(data.folder,"trajan_relations.csv")
rel.data <- as.matrix(read.csv(file=rel.file,header=TRUE,check.names=FALSE))


# add links
g <- add_edges(g, edges=c(t(rel.data[,c("Id1","Id2")])))
# add link attributes


# record graph
graph.file <- file.path(data.folder,"all.graphml")
write.graph(graph=g,file=graph.file,format="graphml")

# TODO
# plutot extraire les valeurs multiples sous forme d'autant d'attributs ?
# nécessaire de convertir les oui/non en T/F ?