#############################################################################################
# Functions used during sequence analysis.
# 
# 04/2020 Vincent Labatut
#
# setwd("C:/users/Vincent/Eclipse/workspaces/Networks/TrajanNet")
# setwd("~/eclipse/workspaces/Networks/TrajanNet")
# source("src/sequences.R")
#############################################################################################




#############################################################
# Creates a new plot using the appropriate parameters.
#
# file: name of the plot file.
#############################################################
create.plot <- function(file)
{	if(FORMAT=="pdf")
		pdf(paste0(file,".pdf"), width=25, height=25)
	else if(FORMAT=="png")
		png(paste0(file,".png"), width=1024, height=1024)
}




#############################################################
# Builds the transition graph based on the list of sequences,
# and records it as a plot and a graphml file.
#
# seq.tab: table containing the sequences.
# pos.tab: table containing the positions and their details.
# folder: output folder.
# seq.col: name of the column containing the sequences in seq.tab.
#############################################################
build.transition.graph <- function(seq.tab, pos.tab, folder, seq.col)
{	# output folder
	tr.folder <- file.path(folder, "transitions")
	
	# compute the ajacency matrix
	lst <- strsplit(as.character(seq.tab[,seq.col]),";",fixed=TRUE)
	adj <- matrix(0, nrow=nrow(pos.tab), ncol=nrow(pos.tab), dimnames=list(pos.tab[,SEQ_IDENTIFIER],pos.tab[,SEQ_IDENTIFIER]))
	size <- rep(0, nrow(pos.tab))
	names(size) <- pos.tab[,SEQ_IDENTIFIER]
	for(traj in lst)
	{	traj[traj=="NA"] <- "*"
		size[traj[1]] <- size[traj[1]] + 1
		if(length(traj)>1)
		{	for(i in 2:length(traj))
			{	if(traj[i]!=traj[i-1])
				{	size[traj[i]] <- size[traj[i]] + 1
					adj[traj[i-1],traj[i]] <- adj[traj[i-1],traj[i]] + 1
				}
			}
		}
	}
	
	if("*" %in% names(size))
	{	names(size)[names(size)=="*"] <- SEQ_MISSING
		colnames(adj)[colnames(adj)=="*"] <- SEQ_MISSING		
		rownames(adj)[rownames(adj)=="*"] <- SEQ_MISSING		
	}
	
	# build the graph based on the adjacency matrix
	cols <- pos.tab[,SEQ_COLOR]
	g <- graph_from_adjacency_matrix(
		adjmatrix=adj,
		mode ="directed",
		weighted=TRUE
	)
	
	# possibly load the layout
	lay.file <- file.path(tr.folder, "transition_graph_layout.txt")
	if(file.exists(lay.file))
		lay <- as.matrix(read.table(file=lay.file))
	else
		lay <- layout_with_fr(g)
	
	# plot the graph
	plot.file <- file.path(tr.folder, "transition_graph")
	create.plot(plot.file)
		plot(g,											# graph to plot
			layout=lay,									# layout
			vertex.size=4+1.25*size,					# node size
			vertex.color=cols,							# node color
			vertex.label=pos.tab[,SEQ_POSTE],			# node labels
			vertex.label.cex=1.2,						# label size
			vertex.label.family="sans",					# font type
			vertex.label.font=2,						# 1 is plain text, 2 is bold face, 3 is italic, 4 is bold and italic
			vertex.label.label.dist=0,					# label distance to node center (0=center)
			vertex.label.color="BLACK",					# label color
#			edge.color=rgb(0,0,0,max=255,alpha=125),	# edge color
#			edge.arrow.size=E(g)$weight,				# size of the arrows
			edge.width=3+3*E(g)$weight					# link thickness
		)
	dev.off()
	
#	# record the layout
#	lay.file <- file.path(tr.folder, "transition_graph_layout.txt")
#	write.table(x=lay, file=lay.file)
	
	# record the graph as a graphml file
	V(g)$fullname <- pos.tab[,SEQ_POSTE]
	V(g)$weight <- size
	V(g)$color <- cols
	graph.file <- file.path(tr.folder, "transition_graph.graphml")
	write.graph(graph=g, file=graph.file, format="graphml")
}




#############################################################
# Produces the alluvial diagarms representing the the transition 
# between positions.
#
# seq.tab: table containing the sequences.
# pos.tab: table containing the postions and their details.
# folder: output folder.
# seq.col: name of the column containing the sequences in seq.tab.
# attr.data: table containing all the attributes of the historical characters.
#############################################################
plot.alluvial.diagrams <- function(seq.tab, pos.tab, folder, seq.col, attr.data)
{	# output folder
	tr.folder <- file.path(folder, "transitions")
	
	# compute the data frame
	lst <- strsplit(as.character(seq.tab[,seq.col]),";",fixed=TRUE)
	nc <- max(sapply(lst,length))
	mat <- matrix(0, nrow=nrow(seq.tab), ncol=nc, dimnames=list(seq.tab[,SEQ_ID],paste("[",1:nc,"]",sep="")))
	for(i in 1:length(lst))
	{	traj <- lst[[i]]
#		traj[traj=="NA"] <- SEQ_MISSING
		if(length(traj)<nc)
			traj <- c(traj, rep("NA",nc-length(traj)))
		mat[i,] <- traj
	}
	df <- as.data.frame(mat, levels=levels(c(pos.tab[,SEQ_IDENTIFIER]),"NA"))
	att.names <- c(ATT_NODE_LACTICLAVIUS, ATT_NODE_TRAV_NBR, ATT_NODE_REL_TRAJ, ATT_NODE_REL_HADR, ATT_NODE_SPANISH)
	for(i in 0:length(att.names))
	{	# setup alluvium colors
		if(i==0)
		{	plot.file <- file.path(tr.folder, "transition_alluvial")
			cols <- "DARKGREY"
		}
		else
		{	plot.file <- file.path(tr.folder, paste0("transition_alluvial_",att.names[i]))
			cols <- CAT_COLORS[as.integer(attr.data[,att.names[i]])]
			cols[is.na(attr.data[,att.names[i]])] <- "GREY"
		}
		
		# setup file format
		if(FORMAT=="pdf")
			pdf(paste0(plot.file,".pdf"), width=50, height=25)
		else if(FORMAT=="png")
			png(paste0(plot.file,".png"), width=2048, height=1024)
		# draw plot
		alluvial(df, freq=1,			# data
			col=cols,					# alluvium fill color
			border=NA,					# alluvium border color
			blocks=TRUE,				# whether to draw variable values as rectangles
			axis_labels=colnames(df),	# time labels
			cex=1.0						# text size
		)
		dev.off()
	}
	
#	# ggalluvial
##	mat[mat=="NA"] <- NA
#	df0 <- as.data.frame(mat, levels=levels(c(pos.tab[,SEQ_IDENTIFIER]),"NA"))
#	colnames(df0) <- paste("t", 1:nc, sep="")
#	df0 <- cbind(df0, rep(1,nrow(df0)))
#	colnames(df0)[ncol(df0)] <- "Freq"
#	df0 <- cbind(attr.data, df0)
#	ggplot(df0, 
#			aes(y=Freq, axis1=t1, axis2=t2, axis3=t3, axis4=t4, axis5=t5, axis6=t6, axis7=t7, axis8=t8, axis9=t9)) +
#		geom_alluvium(aes(fill=Espagnol), width=1/12) +
#		geom_stratum(width=1/12, fill="black", color="grey") +
#		geom_label(stat="stratum", infer.label=TRUE) +
#		scale_x_discrete(limits = c("t1","t2","t3","t4","t5","t6","t7","t8","t9"), expand = c(.05, .05)) +
#		scale_fill_brewer(type="qual", palette="Set1") +
#		ggtitle("Carrieres")
#
#	ggplot(df0, 
#			aes(y=Freq, axis1=t1, axis2=t2, axis3=t3, axis4=t4, axis5=t5, axis6=t6, axis7=t7, axis8=t8, axis9=t9)) +
#		geom_alluvium(width=1/12) +
#		geom_stratum(width=1/12, fill="black", color="grey") +
#		geom_label(stat="stratum", infer.label=TRUE) +
#		scale_x_discrete(limits = c("t1","t2","t3","t4","t5","t6","t7","t8","t9"), expand = c(.05, .05)) +
#		ggtitle("Carrieres")
}




#############################################################
# Computes transition rates and plots the related matrix.
#
# sd: sequence data.
# missing.option: option to represent missing values in sequences.
# folder: main output folder.
#############################################################
compute.transition.rates <- function(sd, missing.option, folder)
{	# output folder
	tr.folder <- file.path(folder, "transitions")
	
	# consider two types of transition matrices, by removing empty rows/cols
	sfx <- c("all","nonempty")
	
	for(i in 1:length(sfx))
	{	# compute transition matrix
		trate.mat <- seqtrate(sd,				# data
			with.missing=is.na(missing.option)	# whether to take missing states into account
		)		
		idxr <- which(apply(trate.mat, 1, sum)>0)
		idxc <- which(apply(trate.mat, 2, sum)>0)
		if(sfx[i]!="all")
			trate.mat <- trate.mat[idxr,idxc]	# remove empty transitions
		trate.mat[which(trate.mat==0)] <- NA
		
		# plot transition matrix
		plot.file <- file.path(tr.folder, paste0("transition_rates_",sfx[i]))
		create.plot(plot.file)
			par(mar=c(5.4, 5.4, 2.6, 4.1))	# margins B L T R 
			plot(trate.mat,		# matrix
				col=colorRampPalette(c("yellow",'red')),
				las=2,
				xlab=NA,	#"Etat posterieur",
				ylab=NA,	#"Etat anterieur",
				main="Taux de transition"
			)
		dev.off()
	}
	
	# record transition rates
	file.name <- file.path(tr.folder, paste0("transition_rates.txt"))
	write.table(trate.mat, file.name, quote=FALSE, sep="\t")
	#print(round(trate.mat),2)
}




#############################################################
# Loads and prepares the data required to handle the sequence analysis.
#
# seq.col: name of the column containing the sequence to treat.
# missing.option: option to represent missing values in sequences.
#
# returns: a list containing a table with all the date, and a
#		   a traminer object representing the sequences.
#############################################################
prepare.seq.data <- function(seq.col, missing.option)
{	# load attributes
	attr.file <- file.path(TABLE_FOLDER,"trajan_attributes.csv")
	attr.data <- read.csv(file=attr.file,header=TRUE,check.names=FALSE)
	
	# load seq table
	seq.file <- file.path(TABLE_FOLDER,"trajan_careers.csv")
	seq.tab <- read.csv(file=seq.file, header=TRUE, check.names=FALSE)
	
	# filter out people with no documented career
	idx <- which(!is.na(seq.tab[,seq.col]))
	seq.tab <- seq.tab[idx,]
	attr.data <- attr.data[idx,]
	
	# load position table
	pos.file <- file.path(TABLE_FOLDER,"trajan_positions.csv")
	pos.tab <- read.csv(file=pos.file, header=TRUE, check.names=FALSE, stringsAsFactors=FALSE)
	
	# get the states actually present in the data
	states <- unique(unlist(strsplit(x=as.character(seq.tab[,seq.col]),split=";",fixed=TRUE)))
	idx <- sort(match(states,pos.tab[,SEQ_IDENTIFIER]))
	pos.tab <- pos.tab[idx,]
	
	# select the predefined palette
	cols <- pos.tab[,SEQ_COLOR]
	
	# break down career sequence
	ids <- as.character(seq.tab[,SEQ_ID])
	car <- gsub(";", "-", seq.tab[,seq.col])
	
	# build the whole table
	main.tab <- cbind(attr.data, car)
	colnames(main.tab)[ncol(main.tab)] <- SEQ_SEQ
	
	# convert data
	sd <- seqdef(
		data=main.tab,						# data to process
		left=missing.option,				# how to handle missing data at the beginning of the sequence 
		gap=missing.option,					# how to handle missing data inside the sequence 
		right=missing.option,				# how to handle missing data at the end of the sequence 
		var=SEQ_SEQ,						# name of the columns containing the formatted sequences
		id=ids,								# ids of the characters
		alphabet=pos.tab[,SEQ_IDENTIFIER],	# list of position codes
		labels=pos.tab[,SEQ_POSTE],			# names of these positions
		cpal=cols[1:nrow(pos.tab)],			# colors of these positions
		missing.color="#AAAAAA"				# color of missing values
	)
	
	# possibly add a missing state
	if(is.na(missing.option))
	{	tmp <- data.frame(SEQ_MISSING, NA, NA, "*", NA, NA, "#AAAAAA")
		colnames(tmp) <- colnames(pos.tab)
		pos.tab <- rbind(pos.tab, tmp)
	}
	
	result <- list(attr.data=attr.data, seq.tab=seq.tab, main.tab=main.tab, pos.tab=pos.tab, ids=ids, sd=sd)
	return(result)
}




#############################################################
# Generate the main plots related to sequence analysis.
# 
# sd: sequence data.
# main.tab: table containing all the data.
# missing.option: option to represent missing values in sequences.
# folder: main output folder.
#############################################################
generate.main.seq.plots <- function(sd, main.tab, missing.option, folder)
{	# plot legend apart
	plot.file <- file.path(folder, "caption")
	create.plot(plot.file)
		seqlegend(sd)
	dev.off()
	
	# all sequences sorted by start state
	plot.file <- file.path(folder, "all_seq")
	create.plot(plot.file)
		seqIplot(sd,							# data
			sortv="from.start", 				# how to sort the sequences
			with.legend=FALSE,					# whether and where to put the legend ("right")
			with.missing=is.na(missing.option),	# whether to take missing states into account
			xlab="Chronologie des etats",		# x-axis title
			ylab="Personnage",					# y-axis title
			ytlab="id",							# character codes
			ylas=1,								# orientation of these codes
			main="Ensemble des sequences"		# plot title
		)
	dev.off()
	
	# sequences separated depending on a categorical variable
	att.names <- c(ATT_NODE_LACTICLAVIUS, ATT_NODE_TRAV_NBR, ATT_NODE_REL_TRAJ, ATT_NODE_REL_HADR, ATT_NODE_SPANISH)
	for(att.name in att.names)
	{	att.folder <- file.path(folder, "attributes", att.name)
		dir.create(path=att.folder, showWarnings=FALSE, recursive=TRUE)
		plot.file <- file.path(att.folder, "all_seq")
		create.plot(plot.file)
			seqIplot(sd,							# data
				group=main.tab[,att.name],			# variable used to group sequences
				sortv="from.start", 				# how to sort the sequences
				with.legend=FALSE,					# whether and where to put the legend ("right")
				with.missing=is.na(missing.option),	# whether to take missing states into account
				xlab="Chronologie des etats",		# x-axis title
				ylab="Personnage",					# y-axis title
				ytlab="id",							# character codes
				ylas=1,								# orientation of these codes
				main=LONG_NAME[att.name]			# plot title
			)
		dev.off()
	}
	
	# specific case of the circles attribute
	circ.folder <- file.path(folder, "attributes", ATT_NODE_CIRCLES)
	dir.create(path=circ.folder, showWarnings=FALSE, recursive=TRUE)
	circle.str <- as.character(main.tab[,ATT_NODE_CIRCLES])
	for(i in 1:length(ATT_NODE_CIRCLES_VALS))
	{	circle <- ATT_NODE_CIRCLES_VALS[i]
		idx <- which(grepl(circle, circle.str, fixed=TRUE))
		plot.file <- file.path(circ.folder, paste0(circle,"_seq"))
		create.plot(plot.file)
			seqiplot(sd,								# data
				idxs=idx,								# index of the concerned sequences
				sortv="from.start", 					# how to sort the sequences
				with.legend=FALSE,						# whether and where to put the legend ("right")
				with.missing=is.na(missing.option),		# whether to take missing states into account
				xlab="Chronologie des etats",			# x-axis title
				ylab="Personnage",						# y-axis title
				ytlab="id",								# character codes
				ylas=1,									# orientation of these codes
				main=LONG_NAME[ATT_VAL_CIRCLE_VALS[i]]	# plot title
			)
		dev.off()
	}
	
	# state distribution plot
	plot.file <- file.path(folder, "state_distrib")
	create.plot(plot.file)
		par(mar=c(5.1, 4.1, 4.1, 2.1))	# Bottom Left Top Right
		seqdplot(sd, 							# data
			border=NA,							# disable borders 
			with.legend=FALSE,					# whether and where to put the legend ("right")
			with.missing=is.na(missing.option),	# whether to take missing states into account
			xlab="Chronologie des etats",		# x-axis title
			ylab="Frequence",					# y-axis title
			main="Distribution des etats"		# plot title
		)
	dev.off()
	# record corresponding data
	tmp <- seqstatd(sd)
	#print(tmp)
	file.name <- paste0(plot.file, "_frequencies.txt")
	write.table(tmp$Frequencies, file.name, quote=FALSE, sep="\t")
	file.name <- paste0(plot.file, "_states.txt")
	tab <- cbind(tmp$ValidStates,tmp$Entropy)
	colnames(tab) <- c("ValidStates","Entropy")
	write.table(tab, file.name, quote=FALSE, sep="\t")
	
	# most frequent sequences
	plot.file <- file.path(folder, "frequent_seq")
	create.plot(plot.file)
		seqfplot(sd, 							# data
			idxs=1:20,							# which sequences (ranked by freq) to display
			with.legend=FALSE,					# whether and where to put the legend ("right")
			with.missing=is.na(missing.option),	# whether to take missing states into account
			yaxis="pct",						# cum=cumulative freq, pct=freq of each seq
			xlab="Chronologie des etats",		# x-axis title
			ylab="Frequence (%)",				# y-axis title
			main="Sequences frequentes"			# plot title
		)
	dev.off()
	# corresponding data
	tmp <- seqtab(sd, idxs=0)
	#print(tmp)
	file.name <- paste0(plot.file, "_list.txt")
	write.table(print(tmp), file.name, quote=FALSE, sep="\t")
}




#############################################################
# Compute inter-sequence dissimilarity, perform cluster analysis
# and generates the related plots and files.
#
# sd: sequence data.
# ids: codes of the characters.
# missing.option: option to represent missing values in sequences.
# folder: main output folder.
#############################################################
cluster.analysis.seq <- function(sd, ids, missing.option, folder)
{	# set up distance functions
	dist.meths <- c(
		"LCS",			# longest common subsequence
		"OM"			# optimal matching distance
	)
	
	# set up normalization methodes
	norm.meths <- c("none","auto")
	
	# set distance parmeters
	# substitution cost matrix with constant cost 1
	subcost <- seqsubm(sd,					# data
		with.missing=is.na(missing.option),	# whether to take missing values into account
		method="CONSTANT", 					# use constant values
		cval=1								# constant value is 1
	)
	# substitution cost matrix based on transition rates
	#subcost <- seqsubm(sd, method="TRATE")
	
	# process each dissimilarity function
	for(dist.meth in dist.meths)
	{	# process each normalization method
		for(norm.meth in norm.meths)
		{	# possibly create folder
			dd.folder <- file.path(folder, paste0("dist_",dist.meth), paste0("norm_",norm.meth))
			dir.create(path=dd.folder, showWarnings=FALSE, recursive=TRUE)
			
			# compute distance matrix
			dd <- seqdist(
				seqdata=sd,			# data 
				method=dist.meth,	# distance function 
				refseq=NULL,		# sequence of reference 
				norm=norm.meth,		# normalization method 
				indel=1.0,			# insertion/deletion cost (edit dist)
				sm=subcost,			# substitution cost matrix (edit dist)
				with.missing=TRUE	# handle missing values 
			)
			colnames(dd) <- ids
			rownames(dd) <- ids
			# record it
			mat.file <- file.path(dd.folder, "distance_matrix.txt")
			write.table(dd, mat.file, quote=FALSE, sep="\t", col.names=TRUE, row.names=TRUE)
			
			# perform cluster analysis
			dendro <- agnes(dd, diss=TRUE, method="ward")
#			dendro <- as.dendrogram(dendro)
			
			# compute silhouette
			dend.k <- find_k(dendro, krange=2:(nrow(dd)-1))
			plot.file <- file.path(dd.folder, "silhouette")
			create.plot(plot.file)
			plot(dend.k)
			dev.off()
			sil.file <- file.path(dd.folder, "silhouette.txt")
			sil.res <- cbind(1:(nrow(dd)-1), dend.k$crit)
			colnames(sil.res) <- c("k", "Silhouette")
			write.table(sil.res, sil.file, quote=FALSE, sep="\t")
			
			# select best cut
			best.k <- dend.k$nc
			bestk.file <- file.path(dd.folder, "manual_bestk.txt")
			if(file.exists(bestk.file))
				best.k <- read.table(bestk.file)[1,1]
			best.cut <- cutree(dendro, k=best.k)
			cut.file <- file.path(dd.folder, "best_cut.txt")
			write.table(best.cut, cut.file, quote=FALSE, sep="\t", col.names=FALSE)
			
			# plot dendrogram
			cols <- CAT_COLORS[-6]
			cols <- cols[1:best.k]
			plot.file <- file.path(dd.folder, "dendrogram")
			create.plot(plot.file)
				par(mar=c(5.1, 4.1, 2.1, 2.1))	# margins B L T R 
				color_branches(as.hclust(dendro), k=best.k, col=cols)  %>%	
					set("branches_lwd", 2) %>% 
					color_labels(as.hclust(dendro), k=best.k, col=cols)  %>% 
					plot(xlab="Personnage", ylab="Dissimilarite inter-clusters")
				legend(x="topright",legend=paste0("C",1:best.k), fill=cols)
			dev.off()
			
			# plot all sequences by cluster
			plot.file <- file.path(dd.folder, "all_seq")
			create.plot(plot.file)
				par(mar=c(5.1, 4.1, 4.1, 2.1))
				seqIplot(sd,						# data
					group=best.cut,					# clusters
					sortv="from.start", 			# how to sort the sequences
					with.legend=FALSE,				# whether and where to put the legend ("right")
					with.missing=TRUE,				# handle missing values 
					xlab="Chronologie des etats",	# x-axis title
					ylab="Personnage",				# y-axis title
					ytlab="id",						# character codes
					ylas=1,							# orientation of these codes
					main="Cluster"					# plot title
				)
			dev.off()
			
			# plot most frequent sequences by cluster
			plot.file <- file.path(dd.folder, "frequent_seq")
				create.plot(plot.file)
				seqfplot(sd, 						# data
					group=best.cut,					# clusters
					idxs=1:15,						# which sequences (ranked by freq) to display
					with.legend=FALSE,				# whether and where to put the legend ("right")
					with.missing=TRUE,				# handle missing values 
					yaxis="pct",					# cum=cumulative freq, pct=freq of each seq
					xlab="Chronologie des etats",	# x-axis title
					ylab="Frequence (%)",			# y-axis title
					main="Cluster"					# plot title
				)
			dev.off()
			
			# plot state distribution by cluster
			plot.file <- file.path(dd.folder, "state_distrib")
				create.plot(plot.file)
				seqdplot(sd, 						# data
					group=best.cut,					# clusters
					border=NA,						# disable borders 
					with.legend=FALSE,				# whether and where to put the legend ("right")
					with.missing=TRUE,				# handle missing values 
					xlab="Chronologie des etats",	# x-axis title
					ylab="Frequence",				# y-axis title
					main="Cluster"					# plot title
				)
			dev.off()
			
			# plot typical sequence by cluster
			plot.file <- file.path(dd.folder, "typical_seq")
				create.plot(plot.file)
				seqrplot(sd, 						# data
					diss=dd,						# dissimilarity matrix
					group=best.cut,					# clusters
					border=NA,						# border color
					with.legend=FALSE,				# whether and where to put the legend ("right")
					with.missing=TRUE,				# handle missing values 
					#xlab="Chronologie des etats",	# x-axis title
					#ylab="Frequence",				# y-axis title
					main="Cluster"					# plot title
				)
			dev.off()
		}
	}
}




#############################################################
# Main method for the sequence analysis.
#
# Check 
# http://mephisto.unige.ch/pub/TraMineR/doc/TraMineR-Users-Guide.pdf
#############################################################
analyze.sequences <- function()
{	cat("Retrieve main table\n")
	
	suffixes <- c("withoutNAs","withNAs")
	data.cols <- c(SEQ_CAREER, SEQ_CAREER_NA)
	missing.options <- c("DEL", NA)
	for(s in 1:length(suffixes))
	{	na.folder <- file.path(SEQ_FOLDER, suffixes[s])
		dir.create(path=na.folder, showWarnings=FALSE, recursive=TRUE)
		seq.col <- data.cols[s]
		missing.option <- missing.options[s]
		
		# load and convert the sequences
		tmp <- prepare.seq.data(seq.col, missing.option)
		attr.data <- tmp$attr.data
		seq.tab <- tmp$seq.tab
		main.tab <- tmp$main.tab
		pos.tab <- tmp$pos.tab
		ids <- tmp$ids
		sd <- tmp$sd
		
		# generate standard sequence plots
		generate.main.seq.plots(sd, main.tab, missing.option, na.folder)
			
		# compute and plot transition rates
		compute.transition.rates(sd, missing.option, na.folder)
		# build and record transition graph
		build.transition.graph(seq.tab, pos.tab, na.folder, seq.col)
		# same with alluvial plots
		plot.alluvial.diagrams(seq.tab, pos.tab, na.folder, seq.col, attr.data)
		
		# compare to sequences of reference
		#TODO
		
		# compute clusters of sequences
		cluster.analysis.seq(sd, ids, missing.option, na.folder)
	}
}
