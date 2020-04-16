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
	
	# load seq table
	seq.file <- file.path(TABLE_FOLDER,"trajan_careers.csv")
	seq.tab <- read.csv(file=seq.file, header=TRUE, check.names=FALSE)
	
	# filter out people with no documented career
	idx <- which(!is.na(seq.tab[,SEQ_CAREER]))
	seq.tab <- seq.tab[idx,]
	
	# load alphabet table
	pos.file <- file.path(TABLE_FOLDER,"trajan_positions.csv")
	pos.tab <- read.csv(file=pos.file, header=TRUE, check.names=FALSE, stringsAsFactors=FALSE)
	
	# break down career sequence
	ids <- as.character(seq.tab[,SEQ_ID])
	car <- gsub(";", "-", seq.tab[,SEQ_CAREER])
	
	# convert data
	sd <- seqdef(car, 
		alphabet=pos.tab[,SEQ_IDENTIFIER],
		labels=pos.tab[,SEQ_POSTE]
	)
	
	# plot legend apart
	plot.file <- file.path(SEQ_FOLDER, paste0("caption.",FORMAT))
	create.plot(plot.file)
		seqlegend(sd)
	dev.off()
	
	# all sequences sorted by start state
	plot.file <- file.path(SEQ_FOLDER, paste0("all_seq.",FORMAT))
	create.plot(plot.file)
		seqIplot(sd, 						# data
			sortv="from.start", 			# how to sort the sequences
			with.legend=FALSE,				# whether and where to put the legend ("right")
			main="Toutes les sequences"		# plot title
		)
	dev.off()
		
	# state distribution plot
	plot.file <- file.path(SEQ_FOLDER, paste0("state_distrib.",FORMAT))
	create.plot(plot.file)
		seqdplot(sd, 						# data
			border=NA,						# disable borders 
			with.legend=FALSE,				# whether and where to put the legend ("right")
			main="Distribution des etats"	# plot title
		)
	dev.off()
	# corresponding data
	print(seqstatd(sd))
	
	# most frequent sequences
	plot.file <- file.path(SEQ_FOLDER, paste0("frequent_seq.",FORMAT))
	create.plot(plot.file)
		seqfplot(sd, 						# data
			idxs=1:20,						# which sequences (ranked by freq) to display
			with.legend=FALSE,				# whether and where to put the legend ("right")
			main="Sequences frequentes"		# plot title
		)
	dev.off()
	# corresponding data
	print(seqtab(sd,
		tlim=1:20,						# which sequences (ranked by freq) to display
	))

	# transition rates
	plot.file <- file.path(SEQ_FOLDER, paste0("transition_rates.",FORMAT))
	create.plot(plot.file)
		trate.mat <- seqtrate(sd)
		plot(trate.mat)
	dev.off()
	print(round(trate.mat),2)
	
	
}
