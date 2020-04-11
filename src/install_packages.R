#############################################################################################
# Script installing all the required packages.
# 
# 11/2019 Vincent Labatut
#
# setwd("C:/users/Vincent/Eclipse/workspaces/Networks/TrajanNet")
# setwd("~/eclipse/workspaces/Networks/TrajanNet")
# source("src/install_packages.R")
#############################################################################################




install.packages("devtools")
install.packages("igraph")
devtools::install_github("schochastics/signnet")
install.packages("graphlayouts")
#install.packages("ggraph")
install.packages("SDMTools")
install.packages("scales")
install.packages("circlize")
