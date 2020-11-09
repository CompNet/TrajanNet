#############################################################################################
# Script performing post-processing over the previously processed centrality values, in order
# to automatically find relevant groups of nodes.
# 
# 11/2020 Vincent Labatut
#
# setwd("D:/users/Vincent/Eclipse/workspaces/Networks/TrajanNet")
# setwd("~/eclipse/workspaces/Networks/TrajanNet")
# source("src/postproc.R")
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
source("src/common/constants.R")
source("src/graphs/graph_extraction.R")
source("src/graphs/graph_analysis.R")
source("src/graphs/signed_graph_functions.R")
source("src/common/plot.R")
source("src/sequences/sequence_analysis.R")




#############################################################################################
# load data
relations <- c(GRAPH_TYPE_ALL, GRAPH_TYPE_FAMILY, GRAPH_TYPE_FRIEND, GRAPH_TYPE_PRO)
measures <- c(MEAS_DEGREE, MEAS_EIGEN, MEAS_BETWEENNESS, MEAS_CLOSENESS)

attr.file <- file.path(TABLE_FOLDER,"trajan_attributes.csv")
atts <- as.matrix(read.csv(file=attr.file,header=TRUE,check.names=FALSE))
data <- matrix(NA, nrow=nrow(atts), ncol=length(relations)*length(measures))




#############################################################################################
# build overall matrix
col <- 1
names <- c()
for(relation in relations)
{	for(measure in measures)
	{	file <- file.path(NET_FOLDER, relation, tolower(measure), paste0(tolower(measure),"_values0.csv"))
		tab <- read.csv(file=file, header=TRUE, check.names=FALSE)
		names <- c(names, paste0(relation,":",measure))
		data[,col] <- tab[,measure]
		col <- col + 1
	}
}
colnames(data) <- names




#############################################################################################
# standardize data
rownames(data) <- atts[,ATT_NODE_NAME_SHORT]	# ATT_NODE_NAME_SHORT ATT_NODE_NAME
data <- data[-1,]
data[is.na(data)] <- 0
data <- apply(data,2,function(col) (col-mean(col))/sd(col))



#############################################################################################
# do the pca
library(factoextra)
pca <- prcomp(data)
plot(pca$x[,1], pca$x[,2])
# variance explained by the components
fviz_eig(pca)
# instance representation
fviz_pca_ind(pca,
		col.ind = "cos2", # Color by the quality of representation
		gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
		repel = TRUE     # Avoid text overlapping
)
# variable representation
fviz_pca_var(pca,
		col.var = "contrib", # Color by contributions to the PC
		gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
		repel = TRUE     # Avoid text overlapping
)
# compare to spanish
groups <- as.factor(atts[-1,ATT_NODE_SPANISH])
fviz_pca_ind(pca,
		col.ind = groups, # color by groups
		palette = CAT_COLORS[1:length(unique(groups))],
		addEllipses = TRUE, # Concentration ellipses
		ellipse.type = "confidence",
		legend.title = "Espagnol",
		label="none",
		repel = TRUE
)
# compare to entourage group
groups <- as.factor(atts[-1,ATT_NODE_REL_TRAJ])
fviz_pca_ind(pca,
		col.ind = groups, # color by groups
		palette = CAT_COLORS[1:length(unique(groups))],
		addEllipses = TRUE, # Concentration ellipses
		ellipse.type = "confidence",
		legend.title = "Groupe d'entourage",
		label="none",
		repel = TRUE
)




#############################################################################################
# cluster analysis
#dd <- as.matrix(dist(data))
#sil <- rep(NA,39)
#for(k in 2:40)
#{	cat("Processing k=",k,"\n")
#	res <- kmeans(data, k)
#	sil[k-1] <- summary(silhouette(x=res$cluster, dmatrix=dd))$avg.width
#}
res <- kmeans(data, 3)
print(table(res$cluster))
# plot groups using PCA
groups <- as.factor(res$cluster)
fviz_pca_ind(pca,
		axes=c(3,4),
		col.ind = groups, # color by groups
		palette = CAT_COLORS[1:length(unique(groups))],
		addEllipses = TRUE, # Concentration ellipses
		ellipse.type = "confidence",
		legend.title = "Cluster",
#		label="none",
		repel = TRUE
)
