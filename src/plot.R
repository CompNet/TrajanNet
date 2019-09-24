#############################################################################################
# Functions used to plot graphs and figures.
# 
# 09/2019 Vincent Labatut
#
# setwd("C:/users/Vincent/Eclipse/workspaces/Networks/TrajanNet")
# setwd("~/eclipse/workspaces/Networks/TrajanNet")
# source("src/analysis.R")
#############################################################################################
FORMAT <- "png"	# pdf png
LAYOUT <- NA	# graph layout




#############################################################
# Displays the specified graph in an appropraite way, taking
# into account the previously set link and node attributes.
#
# g: graph to plot.
# paths: (optional) paths to highlight while plotting. This parameter
# 		 is either a list of integer vectors (node sequences), or
# 		 an integer vector if there is only one path to plot.
# vvals: (optional) vertex values, used to determine node color.
# file: (optional) file name, to record the plot.
#############################################################
setup.graph.layout <- function(g)
{
	# try to use ego-network layout, but not appropriate here
#	p1 <- ggraph(g, layout="focus", v=1) +
#		draw_circle(use = "focus", max.circle = 3)+
#		geom_edge_link(edge_color="black",edge_width=0.3)+
#		geom_node_point(aes(fill=as.factor(V(g)$RelTrajan)),size=2,shape=21)+
#		scale_fill_manual(values=c("#8B2323", "#EEAD0E", "#34CB34", "#3366FF"))+
#		theme_graph()+
#		theme(legend.position = "none")+
#		coord_fixed()+
#		labs(title= "Trajan's ego-network")
#		# https://cran.r-project.org/web/packages/graphlayouts/vignettes/introduction.html
#		# https://cran.r-project.org/web/packages/graphlayouts/index.html
#		# https://cran.r-project.org/web/packages/ggraph/index.html
	
	# try to read the layout if the file exists
	lay.file <- file.path(NET_FOLDER,"all_layout.txt")
	if(file.exists(lay.file))
	{	cat("Loading layout file \"",lay.file,"\"\n",sep="")
		LAYOUT <<- as.matrix(read.table(file=lay.file))
	}
	
	# otherwise, compute the layout
	else
	{	cat("Layout file \"",lay.file,"\" not found: computing and recording it\n",sep="")
		
		# use a  predefined layout
#		LAYOUT <<- layout_with_fr(g)
		LAYOUT <<- layout_with_fr(g, kkconst=0)
		
		# old code used to manually refine the layout
#		tkplot(g, layout=LAYOUT)
#		LAYOUT <<- tk_coords(3)
		
		write.table(x=LAYOUT,file=lay.file)
	}	
	
}




#############################################################
# Displays the specified graph in an appropraite way, taking
# into account the previously set link and node attributes.
#
# g: graph to plot.
# paths: (optional) paths to highlight while plotting. This parameter
# 		 is either a list of integer vectors (node sequences), or
# 		 an integer vector if there is only one path to plot.
# vvals: (optional) vertex values, used to determine node color.
# file: (optional) file name, to record the plot.
#############################################################
custom.gplot <- function(g, paths, vvals, file)
{	
	# set edge colors
	ecols <- rep("BLACK", gsize(g))
	ecols[E(g)$Type=="Nature_Amicale"] <- "#1a8f39"			# green
	ecols[E(g)$Type=="Nature_Familiale"] <- "#9c1699"		# purple
	ecols[E(g)$Type=="Nature_Professionnelle"] <- "#c27604"	# orange
	# set edge style
	elty <- rep(1,gsize(g))										# solid
	elty[!is.na(E(g)$Polarite) & E(g)$"Polarité"=="Négative"] <- 3			# dotted
	# set edge width
	ewidth <- rep(1,gsize(g))
	# set node outline color
	outline.cols <- rep("BLACK",gorder(g))
	
	# possibly change the color of the highlighted path
	if(hasArg(paths))
	{	if(!is.list(paths))
			paths <- list(paths)
		for(path in paths)
		{	v <- NA
			for(n in path)
			{	if(is.na(v))
				{	v <- n
					outline.cols[v] <- "RED"
				}
				else
				{	u <- v
					v <- n
					idx <- as.integer(E(g)[u %--% v])
					ecols[idx] <- "RED"
					ewidth[idx] <- 2
				}
			}
			outline.cols[v] <- "RED"
		}
	}
	
	# vertex color
	if(hasArg(vvals))
	{	fine = 500 # this will adjust the resolving power.
		pal = colorRampPalette(c("yellow",'red'))
		vcols = pal(fine)[as.numeric(cut(vvals,breaks=fine))]
		# see https://stackoverflow.com/questions/27004167/coloring-vertexes-according-to-their-centrality
	}
	else
		vcols <- rep("GREY",gorder(g))
	
	if(hasArg(file))
	{	if(FORMAT=="pdf")
			pdf(paste0(file,".pdf"), width=25, height=25)
		else if(FORMAT=="png")
			png(paste0(file,".png"), width=1024, height=1024)
	}
	plot(g,
		layout=LAYOUT,
		vertex.size=5, 
		vertex.color=vcols,
		vertex.frame.color=outline.cols,
		edge.color=ecols,
		edge.lty=elty,
		edge.width=ewidth
	)
	if(hasArg(file))
		dev.off()
}




#############################################################
# Custom histogram.
#
# vals: raw values.
# name: name of the values (used for the x-axis label).
# file: (optional) file name, to record the histogram plot.
#############################################################
custom.hist <- function(vals, name, file)
{	if(hasArg(file))
	{	if(FORMAT=="pdf")
			pdf(paste0(file,".pdf"), width=25, height=25)
		else if(FORMAT=="png")
			png(paste0(file,".png"), width=1024, height=1024)
	}
#	par(mar=c(5,3,1,2)+0.1)	# remove the title space Bottom Left Top Right
	par(mar=c(5.1, 4.1, 4.1, 2.1))
	hist(
			vals,			# data
			col="#ffd6d6",	# bar color
			main=NA,		# no main title
			prob=TRUE,		# frenquency density
			breaks=20,		# number of bars
			xlab=name		# x-axis label
	)
	lines(
			density(vals), 	# density estimate
			lwd=2, 			# line thickness
			col="RED"		# line color
	)
	stripchart(
			vals, 			# data
			at=0.02, 		# central position of points (y)
			pch=21, 		# point shape
			col="BLACK", 	# point color
			method="jitter",# noise to avoid overlaps
			jitter=0.02, 	# noise magnitude
			add=TRUE		# add to current plot
	)
	if(hasArg(file))
		dev.off()
}