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
{	prfx <- c("","s","c")
	sufx <- c("", "/sphere_senateurs", "/sphere_chevaliers")
	for(j in 1:length(sufx))
	{	# output folder
		tr.folder <- file.path(folder, paste0("transitions", sufx[j]))
		
		# select sequences
		lst <- strsplit(seq.tab[,seq.col],";",fixed=TRUE)
		if(j==1)
			idx <- 1:length(lst)
		else
			idx <- which(sapply(1:length(lst), function(r) all(lst[[r]]=="NA" | startsWith(lst[[r]],prfx[j]))))
		lst <- lst[idx]
		
		# select positions
		pos <- names(table(unlist(lst)))
		if("NA" %in% pos)
			pos <- c(pos[pos!="NA"],"*")
		pos.idx <- sort(match(pos,pos.tab[,SEQ_IDENTIFIER]))
		pos <- pos.tab[pos.idx,SEQ_IDENTIFIER]
		
		# compute the ajacency matrix
		adj <- matrix(0, nrow=length(pos), ncol=length(pos), dimnames=list(pos,pos))
		size <- rep(0, length(pos))
		names(size) <- pos
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
		cols <- pos.tab[pos.idx,SEQ_COLOR]
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
				vertex.label=pos.tab[pos.idx,SEQ_POSTE],	# node labels
				vertex.label.cex=1.2,						# label size
				vertex.label.family="sans",					# font type
				vertex.label.font=2,						# 1 is plain text, 2 is bold face, 3 is italic, 4 is bold and italic
				vertex.label.label.dist=0,					# label distance to node center (0=center)
				vertex.label.color="BLACK",					# label color
#				edge.color=rgb(0,0,0,max=255,alpha=125),	# edge color
#				edge.arrow.size=E(g)$weight,				# size of the arrows
				edge.width=3+3*E(g)$weight					# link thickness
			)
		dev.off()
		
		# record the layout
		if(!file.exists(lay.file))
			write.table(x=lay, file=lay.file)
		
		# record the transition graph as a graphml file
		V(g)$fullname <- pos.tab[pos.idx,SEQ_POSTE]
		V(g)$weight <- size
		V(g)$color <- cols
		graph.file <- file.path(tr.folder, "transition_graph.graphml")
		write.graph(graph=g, file=graph.file, format="graphml")
	}
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
	lst <- strsplit(seq.tab[,seq.col],";",fixed=TRUE)
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
	
	# plot the alluvial diagrams
	prfx <- c("","s","c")
	sufx <- c("", "sphere_senateurs/", "sphere_chevaliers/")
	att.names <- c(ATT_NODE_LACTICLAVIUS, ATT_NODE_TRAV_NBR, ATT_NODE_REL_TRAJ, ATT_NODE_REL_HADR, ATT_NODE_SPANISH)
	for(j in 1:length(sufx))
	{	for(i in 0:length(att.names))
		{	# select sequences
			if(j==1)
				idx <- 1:nrow(df)
			else
			{	mdf <- as.matrix(df)
				mdf[mdf=="NA"] <- NA
				idx <- which(sapply(1:nrow(df), function(r) all(is.na(mdf[r,]) | startsWith(mdf[r,],prfx[j]))))
			}
			
			# setup alluvium colors
			if(i==0)
			{	plot.file <- file.path(tr.folder, paste0(sufx[j],"transition_alluvial"))
				cols <- "DARKGREY"
			}
			else
			{	plot.file <- file.path(tr.folder, paste0(sufx[j],"transition_alluvial_",att.names[i]))
				cols <- CAT_COLORS[as.integer(attr.data[,att.names[i]])]
				cols[is.na(attr.data[,att.names[i]])] <- "GREY"
				cols <- cols[idx]
			}
			
			# setup file format
			if(FORMAT=="pdf")
				pdf(paste0(plot.file,".pdf"), width=50, height=25)
			else if(FORMAT=="png")
				png(paste0(plot.file,".png"), width=2048, height=1024)
			# draw plot (tryCatch to hide some warnings)
			suppressWarnings(alluvial(df[idx,], freq=1,		# data
				col=cols,									# alluvium fill color
				border=NA,									# alluvium border color
				blocks=TRUE,								# whether to draw variable values as rectangles
				axis_labels=colnames(df),					# time labels
				cex=1.0										# text size
			))
			dev.off()
		}
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
	dir.create(path=tr.folder, showWarnings=FALSE, recursive=TRUE)
	
	# consider two types of transition matrices, by removing empty rows/cols
	sfx <- c("all","nonempty")
	for(i in 1:length(sfx))
	{	# compute transition matrix
		trate.mat <- seqtrate(sd,				# data
			with.missing=is.na(missing.option)	# whether to take missing states into account
		)		
		idxr <- which(apply(trate.mat, 1, sum)>0)
		idxc <- which(apply(trate.mat, 2, sum)>0)
		trate.mat[which(trate.mat==0)] <- NA
		
		# plot transition matrix
		ssfx <- c("","_vals")
		for(j in 1:length(ssfx))
		{	plot.file <- file.path(tr.folder, paste0("transition_rates_",sfx[i],ssfx[j]))
			create.plot(plot.file)
				par(mar=c(5.4, 5.4, 2.6, 4.1))	# margins B L T R 
				plot(if(sfx[i]=="all") trate.mat else trate.mat[idxr,idxc],		# matrix
					col=colorRampPalette(c("yellow",'red')),					# colors
					digits=if(j==1) NA else 2,									# display values, with 2 decimal digits
					las=2,														# labels orientation
					xlab=NA,	#"Etat posterieur",								# x axis title
					ylab=NA,	#"Etat anterieur",								# y axis title
					main="Taux de transition"									# main title
				)
			dev.off()
		}
	}
	
	# record transition rates
	file.name <- file.path(tr.folder, paste0("transition_rates.txt"))
	write.table(trate.mat, file.name, quote=FALSE, sep="\t")
	#print(round(trate.mat),2)
	
	# plot knights and senators separately
	sfx <- c("[s", "[c")
	sfx.n <- c("senateurs", "chevaliers")
	sfx.p <- c("senatoriaux", "equestres")
	for(i in 1:length(sfx))
	{	# filter and renormalize matrix
		idx <- which(startsWith(rownames(trate.mat),sfx[i]))
		mat <- trate.mat[idx,idx]
		mat[is.na(mat)] <- 0
		sums <- t(sapply(apply(mat, 1, sum), function(val) rep(val, ncol(mat)))) 
		mat <- mat / sums
		mat[is.nan(mat) | mat==0] <- NA
		
		# plot resulting matrix
		ssfx <- c("","_vals")
		for(j in 1:length(ssfx))
		{	sp.folder <- file.path(tr.folder, paste0("sphere_",sfx.n[i]))
			dir.create(path=sp.folder, showWarnings=FALSE, recursive=TRUE)
			
			plot.file <- file.path(sp.folder, paste0("transition_rates",ssfx[j]))
			create.plot(plot.file)
				par(mar=c(5.4, 5.4, 2.6, 4.1))	# margins B L T R 
				plot(mat,														# matrix
					col=colorRampPalette(c("yellow",'red')),					# colors
					digits=if(j==1) NA else 2,									# display values, with 2 decimal digits
					las=2,														# labels orientation
					xlab=NA,	#"Etat posterieur",								# x axis title
					ylab=NA,	#"Etat anterieur",								# y axis title
					main=paste0("Taux de transition internes ",sfx.p[i])		# main title
				)
			dev.off()
			
			# record transition rates
			file.name <- file.path(sp.folder, paste0("transition_rates.txt"))
			write.table(mat, file.name, quote=FALSE, sep="\t")
			#print(round(trate.mat),2)
		}
	}
}




#############################################################
# Loads and prepares the data required to handle the sequence analysis.
#
# seq.col: name of the column containing the sequence to treat.
# missing.option: option to represent missing values in sequences.
# add.ref: whether or not to include the reference sequences.
#
# returns: a list containing a table with all the date, and a
#		   a traminer object representing the sequences.
#############################################################
prepare.seq.data <- function(seq.col, missing.option, add.ref)
{	# load attributes
	attr.file <- file.path(TABLE_FOLDER,"trajan_attributes.csv")
	attr.data <- read.csv(file=attr.file,header=TRUE,check.names=FALSE)
	
	# load seq table
	seq.file <- file.path(TABLE_FOLDER,"trajan_careers.csv")
	seq.tab <- as.matrix(read.csv(file=seq.file, header=TRUE, check.names=FALSE))
	
	# filter out people with no documented career
	idx <- which(!is.na(seq.tab[,seq.col]))
	seq.tab <- seq.tab[idx,]
	attr.data <- attr.data[idx,]
	
	# if include reference sequences
	if(add.ref)
	{	# load reference sequences
		ref.file <- file.path(TABLE_FOLDER,"trajan_typical_careers.csv")
		ref.data <- as.matrix(read.csv(file=ref.file, header=TRUE, check.names=FALSE))
		# add to data structures
		seq.tab <- rbind(seq.tab, cbind(ref.data,ref.data[,SEQ_SEQ]))
		mat <- matrix(NA, nrow=nrow(ref.data), ncol=ncol(attr.data))
		colnames(mat) <- colnames(attr.data)
		attr.data <- rbind(attr.data, mat)
	}
	
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
	ids <- seq.tab[,SEQ_ID]
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
# att.name: attribute of interest, or none to process all the sequences.
#############################################################
generate.main.seq.plots <- function(sd, main.tab, missing.option, folder, att.name)
{	# setup output folder and other parameters
	flag.circ <- FALSE
	if(hasArg(att.name))
	{	seq.folder <- file.path(folder, "attributes", att.name)
		dir.create(path=seq.folder, showWarnings=FALSE, recursive=TRUE)
		if(att.name==ATT_NODE_CIRCLES)
		{	flag.circ <- TRUE
			circle.str <- as.character(main.tab[,ATT_NODE_CIRCLES])
			group <- NULL
		}
		else
		{	group <- main.tab[,att.name]
		}
	}
	else
	{	seq.folder <- folder
		group <- NULL
	}
	
	# plot legend apart
	plot.file <- file.path(seq.folder, "caption")
	create.plot(plot.file)
		seqlegend(sd)
	dev.off()
	
	# init counter
	if(flag.circ)
		i <- 1
	else
		i <- length(ATT_NODE_CIRCLES_VALS)
	
	# possibly repeat for each circle (otherwise, loop only once)
	while(i<=length(ATT_NODE_CIRCLES_VALS))
	{	# plot all sequences sorted by start state
		if(flag.circ)
		{	circle <- ATT_NODE_CIRCLES_VALS[i]
			idx <- which(grepl(circle, circle.str, fixed=TRUE))
			sortv <- NULL	# NOTE "from.start" for parameter sortv is apparently incompatible with paramete ridxs
			plot.file <- file.path(seq.folder, paste0(circle,"_seq"))
			main.title <- LONG_NAME[ATT_VAL_CIRCLE_VALS[i]]
		}
		else
		{	plot.file <- file.path(seq.folder, "all_seq")
			idx <- 1:nrow(main.tab)
			sortv <- "from.start"
			if(hasArg(att.name))
				main.title <- LONG_NAME[att.name]
			else
				main.title <- "Ensemble des sequences"
		}
		create.plot(plot.file)
			seqIplot(sd,							# data
				idxs=idx,							# index of the concerned sequences
				group=group,						# variable used to group sequences, or NULL for none
				sortv=sortv,		 				# how to sort the sequences
				with.legend=FALSE,					# whether and where to put the legend ("right")
				with.missing=is.na(missing.option),	# whether to take missing states into account
				xlab="Chronologie des etats",		# x-axis title
				ylab="Personnage",					# y-axis title
				ytlab="id",							# character codes
				ylas=1,								# orientation of these codes
				main=main.title						# plot title
			)
		dev.off()
		
		# state distribution plot
		if(flag.circ)
		{	plot.file <- file.path(seq.folder, paste0(circle,"_state_distrib"))
			main.title <- LONG_NAME[ATT_VAL_CIRCLE_VALS[i]]
		}
		else
		{	plot.file <- file.path(seq.folder, "state_distrib")
			if(hasArg(att.name))
				main.title <- LONG_NAME[att.name]
			else
				main.title <- "Distribution des etats"
		}
		create.plot(plot.file)
			par(mar=c(5.1, 4.1, 4.1, 2.1))	# Bottom Left Top Right
			seqdplot(sd[idx,], 						# data
				group=group,						# variable used to group sequences, or NULL for none
				border=NA,							# disable borders 
				with.legend=FALSE,					# whether and where to put the legend ("right")
				with.missing=is.na(missing.option),	# whether to take missing states into account
				xlab="Chronologie des etats",		# x-axis title
				ylab="Frequence",					# y-axis title
				main=main.title						# plot title
			)
		dev.off()
		# record corresponding data
		if(flag.circ || !hasArg(att.name))
		{	tmp <- seqstatd(sd[idx,])
			#print(tmp)
			file.name <- paste0(plot.file, "_frequencies.txt")
			write.table(tmp$Frequencies, file.name, quote=FALSE, sep="\t")
			file.name <- paste0(plot.file, "_states.txt")
			tab <- cbind(tmp$ValidStates,tmp$Entropy)
			colnames(tab) <- c("ValidStates","Entropy")
			write.table(tab, file.name, quote=FALSE, sep="\t")
		}
		
		# most frequent sequences
		if(flag.circ)
		{	plot.file <- file.path(seq.folder, paste0(circle,"_frequent_seq"))
			main.title <- LONG_NAME[ATT_VAL_CIRCLE_VALS[i]]
		}
		else
		{	plot.file <- file.path(seq.folder, "frequent_seq")
			if(hasArg(att.name))
			{	main.title <- LONG_NAME[att.name]
#				tt <- table(main.tab[,att.name])
#				vals <- names(tt)[which(tt==1)]
#				idx <- idx[!(main.tab[idx,att.name] %in% vals)]
			}
			else
				main.title <- "Sequences frequentes"
		}
		create.plot(plot.file)
			# some cases cause an error with axis, but the plot is still produced (could not find the exact cause)
			tryCatch(seqfplot(sd[idx,],				# data
				idxs=1:20,							# which sequences (ranked by freq) to display
				group=group,						# variable used to group sequences, or NULL for none
				with.legend=FALSE,					# whether and where to put the legend ("right")
				with.missing=is.na(missing.option),	# whether to take missing states into account
				yaxis="pct",						# cum=cumulative freq, pct=freq of each seq
				xlab="Chronologie des etats",		# x-axis title
				ylab="Frequence (%)",				# y-axis title
				main=main.title						# plot title
			),error=function(e) {})
		dev.off()
		# record corresponding data
		if(flag.circ || !hasArg(att.name))
		{	tmp <- seqtab(sd[idx,], idxs=0)
			#print(tmp)
			file.name <- paste0(plot.file, "_list.txt")
			write.table(print(tmp), file.name, quote=FALSE, sep="\t")
		}
		
		i <- i + 1
	}
}




#############################################################
# Compares the sequences of the dataset with two sequences of reference.
# 
# main.tab: table containing all the data.
# ids: codes of the characters.
# missing.option: option to represent missing values in sequences.
# folder: main output folder.
#############################################################
compare.with.ref.seq <- function(seq.col, missing.option, folder)
{	# load and convert the sequences
	tmp <- prepare.seq.data(seq.col, missing.option, add.ref=TRUE)
#	attr.data <- tmp$attr.data
#	seq.tab <- tmp$seq.tab
#	main.tab <- tmp$main.tab
#	pos.tab <- tmp$pos.tab
	ids <- tmp$ids
	sd <- tmp$sd
	
	# set up distance functions
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
	
	# position of reference sequences
	ref.idx <- which(startsWith(ids,"Chevalier") | startsWith(ids,"Senateur"))
	
	# process each dissimilarity function
	for(dist.meth in dist.meths)
	{	# process each normalization method
		for(norm.meth in norm.meths)
		{	# possibly create folder
			dd.folder <- file.path(folder, paste0("dist_",dist.meth), paste0("norm_",norm.meth))
			dir.create(path=dd.folder, showWarnings=FALSE, recursive=TRUE)
			
			# init distance matrix
			dd <- matrix(nrow=(length(ids)-length(ref.idx)), ncol=0)
			rownames(dd) <- ids[1:(length(ids)-length(ref.idx))]
			
			# compute distances to references
			for(i in 1:length(ref.idx))
			{	# compute distance matrix
				dv <- seqdist(
					seqdata=sd,			# data 
					method=dist.meth,	# distance function 
					refseq=ref.idx[i],	# sequence of reference 
					norm=norm.meth,		# normalization method 
					indel=1.0,			# insertion/deletion cost (edit dist)
					sm=subcost,			# substitution cost matrix (edit dist)
					with.missing=TRUE	# handle missing values 
				)
				dd <- cbind(dd, dv[1:(length(ids)-length(ref.idx))])
				colnames(dd)[ncol(dd)] <- ids[ref.idx[i]]
			}
			
			# plot distance matrix
			for(i in 1:2)
			{	plot.file <- file.path(dd.folder, "refseq_dist")
				if(i==2)
					plot.file <- paste0(plot.file, "_vals")
				create.plot(plot.file)
					par(mar=c(5.4, 5.4, 2.6, 4.1))	# margins B L T R 
					plot(dd,						# matrix
						col=colorRampPalette(c("yellow",'red')),
						digits=if(i==1) NA else 2,	# display values, with 2 decimal digits
						las=2,
						breaks=10,
						xlab=NA,	#"Sequences types",
						ylab="Sequences des personnages",
						main="Comparaison entre personnages et carrieres-types"
					)
				dev.off()
			}
			
			# record distance matrix
			mat.file <- file.path(dd.folder, "refseq_dist.txt")
			write.table(dd, mat.file, quote=FALSE, sep="\t", col.names=TRUE, row.names=TRUE)
			
			# identify the closest ref seq for each character
			mat <- cbind(
					ids[1:(length(ids)-length(ref.idx))],
					ids[ref.idx[apply(dd, 1, which.min)]],
					apply(dd, 1, min)
			)
			colnames(mat) <- c("Id","Carriere type la plus proche","Distance")
			mat.file <- file.path(dd.folder, "refseq_closest.txt")
			write.table(mat, mat.file, quote=FALSE, sep="\t", col.names=TRUE, row.names=FALSE)
			
			# compute all distances
			dd <- seqdist(
				seqdata=sd,			# data 
				method=dist.meth,	# distance function 
				refseq=NULL,		# sequence of reference 
				norm=norm.meth,		# normalization method 
				indel=1.0,			# insertion/deletion cost (edit dist)
				sm=subcost,			# substitution cost matrix (edit dist)
				with.missing=TRUE	# handle missing values 
			)
			# plot MDS
			coords <- cmdscale(dd, eig=FALSE, k=2)
			coords[ref.idx,1:2] <- coords[ref.idx,1:2] + matrix(runif(2*length(ref.idx), min=0, max=1.5), ncol=2)
			plot.file <- file.path(dd.folder, "refseq_mds")
			create.plot(plot.file)
				par(mar=c(1.1, 1.1, 2.1, 1.1))	# margins B L T R 
				plot(
					coords[-ref.idx,1], coords[-ref.idx,2], 
					xlim=range(coords[,1]), ylim=range(coords[,2]),
					xlab=NA, ylab=NA,
					xaxt="n", yaxt="n",
					main="Sequences et sequences-types"
				)
				points(
					x=coords[ref.idx,1], y=coords[ref.idx,2], 
					pch=4, lwd=3,
					col=CAT_COLORS_18[1:length(ref.idx)]
				)
				for(i in 1:(length(ids)-length(ref.idx)))
				{	ref <- which(ids==mat[i,2])
					segments(
						x0=coords[i,1], y0=coords[i,2], 
						x1=coords[ref,1], y1=coords[ref,2],
						col=CAT_COLORS_18[ref-length(ids)+length(ref.idx)], 
						lty=3, 
						lwd=1
					)
				}
				text(
					coords[-ref.idx,1], coords[-ref.idx,2], 
					labels=ids[-ref.idx],
					pos=3
				)
				legend(
					x="bottomleft", 
					legend=ids[ref.idx], 
					fill=CAT_COLORS_18[1:length(ref.idx)],
					title="Sequences-types"
				)
			dev.off() 
		}
	}
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
			# plot it
			for(i in 1:2)
			{	plot.file <- file.path(dd.folder, "distance_matrix")
				if(i==2)
					plot.file <- paste0(plot.file,"_vals")
				create.plot(plot.file)
					par(mar=c(3.4, 3.4, 2.6, 4.1))	# margins B L T R 
					plot(dd,						# matrix
						col=colorRampPalette(c("yellow",'red')),
						digits=if(i==1) NA else 2,	# display values (2=with 2 decimal digits)
						las=2,
						breaks=10,
						xlab=NA,
						ylab=NA,
						cex=0.5,
						main="Dissimilarites entre sequences de personnages"
					)
				dev.off()
			}
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
			
			# plot distance matrix with clusters
			ord <- dendro$order
			cls <- best.cut[ord]
			pos <- c(which(cls - c(0,cls[-length(cls)]) != 0), length(cls)+1) - 0.5
			rec <- cbind(pos[-length(pos)], pos[-1], length(ord)-pos[-length(pos)]+1, length(ord)-pos[-1]+1)
			plot.file <- file.path(dd.folder, "clusters_distmat")
			create.plot(plot.file)
			par(mar=c(3.4, 3.4, 2.6, 4.1))	# margins B L T R 
				plot(dd[ord,ord],						# matrix
					col=colorRampPalette(c("yellow",'red')),
					digits=NA,
					las=2,
					breaks=10,
					xlab=NA,
					ylab=NA,
					cex=0.5,
					main="Dissimilarites entre sequences de personnages"
				)
				rect(xleft=rec[,1], ybottom=rec[,3], xright=rec[,2], ytop=rec[,4], lwd=4)
			dev.off()
			
			# plot MDS
			cols <- CAT_COLORS[-6]
			coords <- cmdscale(dd, eig=FALSE, k=2)
			plot.file <- file.path(dd.folder, "clusters_mds")
			create.plot(plot.file)
				par(mar=c(1.1, 1.1, 2.1, 1.1))	# margins B L T R 
				plot(
					NULL,
					xlim=range(coords[,1]), ylim=range(coords[,2]),
					xlab=NA, ylab=NA,
					xaxt="n", yaxt="n",
					main="Sequences et clusters"
				)
				cls <- sort(unique(best.cut))
				for(i in cls)
				{	cls.idx <- which(best.cut==i)
					points(
						coords[cls.idx,1], coords[cls.idx,2],
						pch=16, cex=2,
						col=cols[i]
					)
				}
				text(
					coords[,1], coords[,2], 
					labels=ids,
					pos=3
				)
				legend(
					x="bottomleft", 
					legend=paste0("Cluster ",cls), 
					fill=cols[cls],
					title="Clusters"
				)
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
		cat("  Processing folder ",na.folder,"\n",sep="")
		dir.create(path=na.folder, showWarnings=FALSE, recursive=TRUE)
		seq.col <- data.cols[s]
		missing.option <- missing.options[s]
		
		# load and convert the sequences
		tmp <- prepare.seq.data(seq.col, missing.option, add.ref=FALSE)
		attr.data <- tmp$attr.data
		seq.tab <- tmp$seq.tab
		main.tab <- tmp$main.tab
		pos.tab <- tmp$pos.tab
		ids <- tmp$ids
		sd <- tmp$sd
		
		# generate standard sequence plots
		generate.main.seq.plots(sd, main.tab, missing.option, na.folder)
		att.names <- c(ATT_NODE_LACTICLAVIUS, ATT_NODE_TRAV_NBR, ATT_NODE_REL_TRAJ, ATT_NODE_REL_HADR, ATT_NODE_SPANISH, ATT_NODE_CIRCLES)
		for(att.name in att.names)
		{	cat("    Processing attribute ",att.name,"\n",sep="")
			generate.main.seq.plots(sd, main.tab, missing.option, na.folder, att.name)
		}
			
		# compute and plot transition rates
		compute.transition.rates(sd, missing.option, na.folder)
		# build and record transition graph
		build.transition.graph(seq.tab, pos.tab, na.folder, seq.col)
		# same with alluvial plots
		plot.alluvial.diagrams(seq.tab, pos.tab, na.folder, seq.col, attr.data)
		
		# compare to sequences of reference
		compare.with.ref.seq(seq.col, missing.option, na.folder)
		
		# compute clusters of sequences
		cluster.analysis.seq(sd, ids, missing.option, na.folder)
	}
}
