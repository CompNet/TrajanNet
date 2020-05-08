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
	
	# filter out people with no documented career
	idx <- which(!is.na(seq.tab[,SEQ_CAREER]))
	seq.tab <- seq.tab[idx,]
	attr.data <- attr.data[idx,]
	
	# load alphabet table
	pos.file <- file.path(TABLE_FOLDER,"trajan_positions.csv")
	pos.tab <- read.csv(file=pos.file, header=TRUE, check.names=FALSE, stringsAsFactors=FALSE)
	
	# get the states actually present in the data
	states <- unique(unlist(strsplit(x=as.character(seq.tab[,SEQ_CAREER]),split=";",fixed=TRUE)))
	idx <- sort(match(states,pos.tab[,SEQ_IDENTIFIER]))
	pos.tab <- pos.tab[idx,]
	
	# select the correct palette
	#if(nrow(pos.tab)>18)
	#	cols <- CAT_COLORS_26
	#else
	#	cols <- CAT_COLORS_18
	cols <- pos.tab[,SEQ_COLOR]
	
	# break down career sequence
	ids <- as.character(seq.tab[,SEQ_ID])
	car <- gsub(";", "-", seq.tab[,SEQ_CAREER])
	
	# build the whole table
	main.tab <- cbind(attr.data, car)
	colnames(main.tab)[ncol(main.tab)] <- SEQ_SEQ
	
	# convert data
	sd <- seqdef(main.tab,
		var=SEQ_SEQ,
		id=ids,
		alphabet=pos.tab[,SEQ_IDENTIFIER],
		labels=pos.tab[,SEQ_POSTE],
		cpal=cols[1:nrow(pos.tab)]
	)
	
	# plot legend apart
	plot.file <- file.path(SEQ_FOLDER, "caption")
	create.plot(plot.file)
		seqlegend(sd)
	dev.off()
	
	# all sequences sorted by start state
	plot.file <- file.path(SEQ_FOLDER, "all_seq")
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
	{	folder <- file.path(SEQ_ATT_FOLDER, att.name)
		dir.create(path=folder, showWarnings=FALSE, recursive=TRUE)
		plot.file <- file.path(folder, "all_seq")
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
	folder <- file.path(SEQ_ATT_FOLDER, ATT_NODE_CIRCLES)
	dir.create(path=folder, showWarnings=FALSE, recursive=TRUE)
	circle.str <- as.character(main.tab[,ATT_NODE_CIRCLES])
	for(i in 1:length(ATT_NODE_CIRCLES_VALS))
	{	circle <- ATT_NODE_CIRCLES_VALS[i]
		idx <- which(grepl(circle, circle.str, fixed=TRUE))
		plot.file <- file.path(folder, paste0(circle,"_seq"))
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
	plot.file <- file.path(SEQ_FOLDER, "state_distrib")
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
	plot.file <- file.path(SEQ_FOLDER, "frequent_seq")
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
	{	plot.file <- file.path(SEQ_FOLDER, paste0("transition_rates_",sfx[i]))
		trate.mat <- seqtrate(sd)
		idxr <- which(apply(trate.mat, 1, sum)>0)
		idxc <- which(apply(trate.mat, 2, sum)>0)
		if(sfx[i]!="all")
			trate.mat <- trate.mat[idxr,idxc]	# remove empty transitions
		trate.mat[which(trate.mat==0)] <- NA
		create.plot(plot.file)
			par(mar=c(5.1, 5.1, 2.6, 4.1))	# margins B L T R 
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
	
	
}
