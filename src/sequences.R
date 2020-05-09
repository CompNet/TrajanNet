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
# pos.tab: table containing the postions and their details.
# folder: output folder.
# seq.col: name of the column containing the sequences in seq.tab.
#############################################################
build.transition.graph <- function(seq.tab, pos.tab, folder, seq.col)
{	# compute the ajacency matrix
	lst <- strsplit(as.character(seq.tab[,seq.col]),";",fixed=TRUE)
	adj <- matrix(0, nrow=nrow(pos.tab), ncol=nrow(pos.tab), dimnames=list(pos.tab[,SEQ_IDENTIFIER],pos.tab[,SEQ_IDENTIFIER]))
	size <- rep(0, nrow(pos.tab))
	names(size) <- pos.tab[,SEQ_IDENTIFIER]
	for(traj in lst)
	{	traj[traj=="NA"] <- SEQ_MISSING
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
	
	# build the graph based on the adjacency matrix
	cols <- pos.tab[,SEQ_COLOR]
	g <- graph_from_adjacency_matrix(
		adjmatrix=adj,
		mode ="directed",
		weighted=TRUE
	)
	
	# possibly load the layout
	lay.file <- file.path(folder, "transition_rates_graph_layout.txt")
	if(file.exists(lay.file))
		lay <- as.matrix(read.table(file=lay.file))
	else
		lay <- layout_with_fr(g)
	
	# plot the graph
	plot.file <- file.path(folder, "transition_rates_graph")
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
#		edge.color=rgb(0,0,0,max=255,alpha=125),	# edge color
#		edge.arrow.size=E(g)$weight,				# size of the arrows
		edge.width=3+3*E(g)$weight					# link thickness
	)
	dev.off()
	
#	# record the layout
#	lay.file <- file.path(folder, "transition_rates_graph_layout.txt")
#	write.table(x=lay, file=lay.file)
	
	# record the graph as a graphml file
	V(g)$fullname <- pos.tab[,SEQ_POSTE]
	V(g)$weight <- size
	V(g)$color <- cols
	graph.file <- file.path(folder, "transition_rates_graph.graphml")
	write.graph(graph=g, file=graph.file, format="graphml")
}




#############################################################
# Main method for the sequence analysis.
#
# Check 
# http://mephisto.unige.ch/pub/TraMineR/doc/TraMineR-Users-Guide.pdf
#############################################################
analyze.sequences <- function()
{	cat("Retrieve main table\n")
	# load attributes
	attr.file <- file.path(TABLE_FOLDER,"trajan_attributes.csv")
	attr.data <- read.csv(file=attr.file,header=TRUE,check.names=FALSE)
	
	# load seq table
	seq.file <- file.path(TABLE_FOLDER,"trajan_careers.csv")
	seq.tab <- read.csv(file=seq.file, header=TRUE, check.names=FALSE)
	
	suffixes <- c("withoutNAs","withNAs")
	data.cols <- c(SEQ_CAREER, SEQ_CAREER_NA)
	missing.option <- c("DEL", SEQ_MISSING)
	for(s in 1:length(suffixes))
	{	folder <- file.path(SEQ_FOLDER, suffixes[s])
		dir.create(path=folder, showWarnings=FALSE, recursive=TRUE)
		seq.col <- data.cols[s]
		
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
		
		# possibly add a missing state
		if(s==2)
		{	tmp <- data.frame("Inconnu",NA,NA,SEQ_MISSING,NA,NA,"#AAAAAA")
			colnames(tmp) <- colnames(pos.tab)
			pos.tab <- rbind(pos.tab, tmp)
		}
		
		# select the correct palette
		#if(nrow(pos.tab)>18)
		#	cols <- CAT_COLORS_26
		#else
		#	cols <- CAT_COLORS_18
		cols <- pos.tab[,SEQ_COLOR]
		
		# break down career sequence
		ids <- as.character(seq.tab[,SEQ_ID])
		car <- gsub(";", "-", seq.tab[,seq.col])
		
		# build the whole table
		main.tab <- cbind(attr.data, car)
		colnames(main.tab)[ncol(main.tab)] <- SEQ_SEQ
		
		# convert data
		sd <- seqdef(
			data=main.tab,							# data to process
			left=missing.option[s],					# how to handle missing data at the beginning of the sequence 
			gap=missing.option[s],					# how to handle missing data inside the sequence 
			right=missing.option[s],				# how to handle missing data at the end of the sequence 
			var=SEQ_SEQ,							# name of the columns containing the formatted sequences
			id=ids,									# ids of the characters
			alphabet=pos.tab[,SEQ_IDENTIFIER],		# list of position codes
			labels=pos.tab[,SEQ_POSTE],				# names of these positions
			cpal=cols[1:nrow(pos.tab)]				# colors of these positions
		)
		
		# plot legend apart
		plot.file <- file.path(folder, "caption")
		create.plot(plot.file)
			seqlegend(sd)
		dev.off()
		
		# all sequences sorted by start state
		plot.file <- file.path(folder, "all_seq")
		create.plot(plot.file)
			seqIplot(sd,						# data
				sortv="from.start", 			# how to sort the sequences
				with.legend=FALSE,				# whether and where to put the legend ("right")
				xlab="Chronologie des etats",	# x-axis title
				ylab="Personnage",				# y-axis title
				ytlab="id",						# character codes
				ylas=1,							# orientation of these codes
				main="Ensemble des sequences"	# plot title
			)
		dev.off()
		
		# sequences separated depending on a categorical variable
		att.names <- c(ATT_NODE_LACTICLAVIUS, ATT_NODE_TRAV_NBR, ATT_NODE_REL_TRAJ, ATT_NODE_REL_HADR, ATT_NODE_SPANISH)
		for(att.name in att.names)
		{	att.folder <- file.path(folder, "attributes", att.name)
			dir.create(path=att.folder, showWarnings=FALSE, recursive=TRUE)
			plot.file <- file.path(att.folder, "all_seq")
			create.plot(plot.file)
				seqIplot(sd,						# data
					group=main.tab[,att.name],		# variable used to group sequences
					sortv="from.start", 			# how to sort the sequences
					with.legend=FALSE,				# whether and where to put the legend ("right")
					xlab="Chronologie des etats",	# x-axis title
					ylab="Personnage",				# y-axis title
					ytlab="id",						# character codes
					ylas=1,							# orientation of these codes
					main=LONG_NAME[att.name]		# plot title
				)
			dev.off()
		}
		
		# specific case of the circles
		circ.folder <- file.path(folder, "attributes", ATT_NODE_CIRCLES)
		dir.create(path=circ.folder, showWarnings=FALSE, recursive=TRUE)
		circle.str <- as.character(main.tab[,ATT_NODE_CIRCLES])
		for(i in 1:length(ATT_NODE_CIRCLES_VALS))
		{	circle <- ATT_NODE_CIRCLES_VALS[i]
			idx <- which(grepl(circle, circle.str, fixed=TRUE))
			plot.file <- file.path(circ.folder, paste0(circle,"_seq"))
			create.plot(plot.file)
			seqiplot(sd,									# data
					idxs=idx,								# index of the concerned sequences
					sortv="from.start", 					# how to sort the sequences
					with.legend=FALSE,						# whether and where to put the legend ("right")
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
			seqdplot(sd, 						# data
				border=NA,						# disable borders 
				with.legend=FALSE,				# whether and where to put the legend ("right")
				xlab="Chronologie des etats",	# x-axis title
				ylab="Frequence",				# y-axis title
				main="Distribution des etats"	# plot title
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
			seqfplot(sd, 						# data
				idxs=1:20,						# which sequences (ranked by freq) to display
				with.legend=FALSE,				# whether and where to put the legend ("right")
				yaxis="pct",					# cum=cumulative freq, pct=freq of each seq
				xlab="Chronologie des etats",	# x-axis title
				ylab="Frequence (%)",			# y-axis title
				main="Sequences frequentes"		# plot title
			)
		dev.off()
		# corresponding data
		tmp <- seqtab(sd, idxs=0)
		#print(tmp)
		file.name <- paste0(plot.file, "_list.txt")
		write.table(print(tmp), file.name, quote=FALSE, sep="\t")
	
		# transition rates
		sfx <- c("all","nonempty")
		for(i in 1:length(sfx))
		{	plot.file <- file.path(folder, paste0("transition_rates_",sfx[i]))
			trate.mat <- seqtrate(sd)
			idxr <- which(apply(trate.mat, 1, sum)>0)
			idxc <- which(apply(trate.mat, 2, sum)>0)
			if(sfx[i]!="all")
				trate.mat <- trate.mat[idxr,idxc]	# remove empty transitions
			trate.mat[which(trate.mat==0)] <- NA
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
		file.name <- paste0(plot.file, ".txt")
		write.table(trate.mat, file.name, quote=FALSE, sep="\t")
		#print(round(trate.mat),2)
		build.transition.graph(seq.tab, pos.tab, folder, seq.col)
	}
}
