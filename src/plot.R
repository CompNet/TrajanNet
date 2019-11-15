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
# see http://colorbrewer2.org/#type=qualitative&scheme=Set1&n=9
CAT_COLORS <- c( 
	"#E41A1C",
	"#377EB8",
	"#4DAF4A",
	"#984EA3",
	"#FF7F00",
	"#FFFF33",
	"#A65628",
	"#F781BF",
	"#999999"
)
CAT_COLORS_18 <- c(
	rgb(228,26,28,maxColorValue=255),		# red
	rgb(55,126,184,maxColorValue=255),		# light blue
	rgb(113,219,110,maxColorValue=255),		# light green
	rgb(152,78,163,maxColorValue=255),		# purple
	rgb(255,127,0,maxColorValue=255),		# orange
	rgb(166,86,40,maxColorValue=255),		# brown
	rgb(247,129,191,maxColorValue=255),		# pink
	rgb(153,153,153,maxColorValue=255),		# light grey
	rgb(23,89,143,maxColorValue=255),		# dark blue
	rgb(16,125,12,maxColorValue=255),		# dark green
	rgb(30,30,30,maxColorValue=255),		# dark grey
	rgb(255,255,51,maxColorValue=255),		# yellow
	rgb(143,11,13,maxColorValue=255),		# dark red
	rgb(0,255,255,maxColorValue=255),		# cyan
	rgb(14,161,161,maxColorValue=255),		# dark cyan
	rgb(255,187,120,maxColorValue=255),		# light orange
	rgb(0,0,255,maxColorValue=255),			# straight blue
	rgb(0,255,0,maxColorValue=255)			# straight green
)




#############################################################
# Displays the specified graph in an appropriate way, taking
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
# col.att: (optional) name of a vertex attribute, used to determine node color.
#          It is also possible to pass just the beginning of the name, common
#		   to several binary attributes, in which case the plot will use piecharts 
#		   to represent nodes.
# cat.att: (optional) if there is a vertex attribute, indicates whether
#		   it is categorical or not.
# v.hl: vertices to highlight (these are represented as squares).
# e.hl: edges to highlight (these are represented as thick lines).
# color.isolates: force isolates to be colored (by default they are not)
# file: (optional) file name, to record the plot.
#############################################################
custom.gplot <- function(g, paths, col.att, cat.att=FALSE, v.hl, e.hl, color.isolates=FALSE, file)
{	pie.values <- NA
	lgd.col <- NA
	
	# vertex shapes
	vshapes <- rep("circle",gorder(g))
	if(hasArg(v.hl))
		vshapes[v.hl] <- "csquare"
	# vertex outline color
	outline.cols <- rep("BLACK",gorder(g))
	
	# set edge colors
	nature <- edge_attr(g,ATT_EDGE_NAT)
	only.signed <- length(nature)==0
	if(!only.signed)
	{	ecols <- rep("BLACK", gsize(g))
		ecols[nature==ATT_VAL_FRIEND] <- "#1A8F39"		# green
		ecols[nature==ATT_VAL_FAMILY] <- "#9C1699"		# purple
		ecols[nature==ATT_VAL_PRO] <- "#C27604"			# orange
		ecols[nature==ATT_VAL_UNK] <- "#222222"			# dark grey
		# set edge style
		polarity <- edge_attr(g,ATT_EDGE_POL)
		elty <- rep(1,gsize(g))							# positive=solid
		elty[!is.na(polarity) 
				& polarity==ATT_VAL_NEGATIVE] <- 3		# negative=dotted
		elty[is.na(polarity)] <- 5						# unknown=long-dashed
		# set edge width
		ewidth <- rep(1,gsize(g))
	}
	else
	{	signs <- edge_attr(g,ATT_EDGE_SIGN)
		ecols <- rep("#1A8F39", gsize(g))				# positive=green
		ecols[signs<0] <- "RED"							# negative=red
		elty <- rep(1,gsize(g))							# only solid line
		ewidth <- rep(1,gsize(g))						# same edge width
	}
	
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
					vshapes[v] <- "csquare"
				}
				else
				{	u <- v
					v <- n
					outline.cols[v] <- "RED"
					idx <- as.integer(E(g)[u %--% v])
					ecols[idx] <- "RED"
					ewidth[idx] <- 2
				}
			}
			outline.cols[v] <- "RED"
			vshapes[v] <- "csquare"
		}
	}
	
	# possibly highlight certain links
	if(hasArg(e.hl))
	{	if(length(e.hl)>0)
			ewidth[e.hl] <- ewidth[e.hl]*3
	}
	
	# vertex color
	if(hasArg(col.att))
	{	# isolates have no color
		vcols <- rep("WHITE",gorder(g))
		if(color.isolates)
			connected <- rep(TRUE, gorder(g))
		else
			connected <- degree(g)>0
		# get the attribute values
		vvals <- get.vertex.attribute(graph=g, name=col.att)
		
		# just one attribute
		if(length(vvals)>0)
		{	# categorical attribute
			if(cat.att)
			{	tmp <- factor(vvals[connected])
				vcols[connected] <- CAT_COLORS[(as.integer(tmp)-1) %% length(CAT_COLORS) + 1]
				lgd.txt <- levels(tmp)
				lgd.col <- CAT_COLORS[(1:length(lgd.txt)-1) %% length(CAT_COLORS) + 1]
			}
			# numerical attribute
			else
			{	fine = 500 									# granularity of the color gradient
				pal = colorRampPalette(c("yellow",'red'))	# extreme colors of the gradient
				finite <- !is.infinite(vvals)
				vcols[connected & finite] <- pal(fine)[as.numeric(cut(vvals[connected & finite],breaks=fine))]
				vcols[connected & !finite] <- "#575757"		# infinite values are grey
				# see https://stackoverflow.com/questions/27004167/coloring-vertexes-according-to-their-centrality
			}
		}
		# several attributes, supposedly binary ones
		else
		{	cat.att <- TRUE
			att.list <- list.vertex.attributes(g)							# list of all vertex attributes
			atts <- att.list[grepl(att.list,pattern=col.att)]				# look for the ones starting appropriately
			m <- sapply(atts, function(att) vertex_attr(g, att))			# get attribute values as a matrix
			are.nas <- apply(m,1,function(r) all(is.na(r)))					# detect individuals with only NAs
			are.pie <- apply(m,1,function(r) length(r[!is.na(r)])>1)		# detect individuals with several non-NA values
			uvals <- sort(unique(c(m)))										# get unique attribute values
			pie.matrix <- NA
			for(uval in uvals)												# build a column for each of them
			{	vals <- as.integer(apply(m, 1, function(v) uval %in% v[!is.na(v)]))
				if(all(is.na(pie.matrix)))
					pie.matrix <- as.matrix(vals, ncol=1)
				else
					pie.matrix <- cbind(pie.matrix, vals)
				colnames(pie.matrix)[ncol(pie.matrix)] <- uval
			}
			lgd.txt <- colnames(pie.matrix)
			if(length(lgd.txt)<=length(CAT_COLORS))
				lgd.col <- CAT_COLORS[(1:length(lgd.txt)-1) %% length(CAT_COLORS) + 1]
			else
				lgd.col <- CAT_COLORS_18[(1:length(lgd.txt)-1) %% length(CAT_COLORS_18) + 1]
			pie.values <- unlist(apply(pie.matrix, 1, function(v) list(v)), recursive=FALSE)
			pie.values[!are.pie | !connected] <- NA
			vshapes[are.pie & connected] <- rep("pie",length(which(are.pie)))
			vcols[are.pie & connected] <- NA
			vcols[!are.nas & !are.pie & connected] <- apply(pie.matrix[!are.nas & !are.pie & connected,,drop=FALSE], 1, 
					function(v) lgd.col[which(v>0)])
		}
	}
	else
		vcols <- rep("GREY",gorder(g))
	
	# main plot
	if(hasArg(file))
	{	if(FORMAT=="pdf")
			pdf(paste0(file,".pdf"), width=25, height=25)
		else if(FORMAT=="png")
			png(paste0(file,".png"), width=1024, height=1024)
	}
	plot(g,
#		axes=TRUE,
		layout=LAYOUT,
		vertex.size=5, 
		vertex.color=vcols,
		vertex.pie=pie.values,
		vertex.pie.color=list(lgd.col),
		vertex.shape=vshapes,
		vertex.frame.color=outline.cols,
		edge.color=ecols,
		edge.lty=elty,
		edge.width=ewidth
	)
	if(!only.signed)
	{	legend(
			title="Nature de la relation",					# title of the legend box
			x="topright",									# position
			legend=c(ATT_VAL_FRIEND,ATT_VAL_FAMILY,			# text of the legend
					ATT_VAL_PRO,ATT_VAL_UNK),
			col=c("#1A8F39","#9C1699","#C27604","#222222"),	# color of the lines
			lty=1,											# type of lines
			lwd=2,											# line thickness
			bty="n",										# no box around the legend
			cex=0.8
		)
		legend(
			title="Polarite de la relation",				# title of the legend box
			x="bottomright",								# position
			legend=c(ATT_VAL_POSITIVE,ATT_VAL_NEGATIVE,		# text of the legend
					ATT_VAL_UNK),
			col="BLACK",									# color of the lines
			lty=c(1,3,5),									# type of lines
			lwd=2,											# line thickness
			bty="n",										# no box around the legend
			cex=0.8,										# size of the text in the legend
			seg.len=3										# length of the line in the legend
		)
	}
	else
	{	legend(
			title="Polarite de la relation",				# title of the legend box
			x="bottomright",								# position
			legend=c(ATT_VAL_POSITIVE,ATT_VAL_NEGATIVE),	# text of the legend
			col=c("GREEN","RED"),							# color of the lines
			lty=c(1,1),										# type of lines
			lwd=2,											# line thickness
			bty="n",										# no box around the legend
			cex=0.8,										# size of the text in the legend
			seg.len=3										# length of the line in the legend
		)
	}
	if(hasArg(col.att))
	{	# categorical attributes
		if(cat.att)
		{	legend(
				title=LONG_NAME[col.att],				# title of the legend box
				x="bottomleft",							# position
				legend=lgd.txt,							# text of the legend
				fill=lgd.col,							# color of the nodes
				bty="n",								# no box around the legend
				cex=0.8									# size of the text in the legend
			)
		}
		# numerical attributes
		else
		{	width <- 0.05
			height <- 0.3
			x1 <- -1
			x2 <- x1 + width
			y2 <- -1
			y1 <- y2 + height
			leg.loc <- cbind(x=c(x1, x2, x2, x1), y=c(y1, y1, y2, y2))
			legend.gradient(
					pnts=leg.loc,
					cols=pal(25),
					#limits=format(range(vvals[connected],na.rm=TRUE), digits=2, nsmall=2),	# pb: uses scientific notation when numbers too small
					limits=sprintf("%.2f", range(vvals[connected & finite],na.rm=TRUE)),
					title=LONG_NAME[col.att], 
					cex=0.8
			)
		}
	}
	# legend for vertex sizes: https://stackoverflow.com/questions/38451431/add-legend-in-igraph-to-annotate-difference-vertices-size
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




#############################################################
# Custom barplot.
#
# vals: raw values.
# text: name of the bars.
# xlab: label of the x-axis.
# ylab: label of the y-axis.
# file: (optional) file name, to record the histogram plot.
# ...: additional parameters, fetched to the barplot function.
#############################################################
custom.barplot <- function(vals, text, xlab, ylab, file, ...)
{	idx <- which(is.na(text))
	if(length(idx)>0)
		text[idx] <- ATT_VAL_UNK0
	wide <- length(text) > 8
	if(hasArg(file))
	{	if(FORMAT=="pdf")
			pdf(paste0(file,".pdf"), width=25, height=25)
		else if(FORMAT=="png")
			png(paste0(file,".png"), width=1024, height=1024)
	}
#	par(mar=c(5,3,1,2)+0.1)	# remove the title space Bottom Left Top Right
	if(wide)
		par(mar=c(9, 4, 1, 0)+0.1)
	else
		par(mar=c(5, 4, 1, 0)+0.1)
	if(length(dim(vals))<=1)
	{	barplot(
			height=vals,				# data
			names.arg=text,				# bar names
			col="#ffd6d6",				# bar color
			main=NA,					# no main title
			xlab=if(wide) NA else xlab,	# x-axis label
			ylab=ylab,					# y-axis label
			las=if(wide) 2 else 0,		# vertical label if too many bars
			...
		)
	}
	else
	{	barcols <- CAT_COLORS[(1:nrow(vals)-1) %% length(CAT_COLORS)+1]
		barplot(
			height=vals,				# data
			names.arg=text,				# bar names
			beside=TRUE,				# grouped bars
			col=barcols,				# bar colors
			main=NA,					# no main title
			xlab=if(wide) NA else xlab,	# x-axis label
			ylab=ylab,					# y-axis label
			las=if(wide) 2 else 0,		# vertical label if too many bars
			...
		)
		text2 <- rownames(vals)
		idx <- which(is.na(text2))
		if(length(idx)>0)
			text2[idx] <- ATT_VAL_UNK0
		legend(
			x="topleft",
			fill=barcols,
			title=names(dimnames(vals))[1],
			legend=text2
		)
	}
	if(hasArg(file))
		dev.off()
}
