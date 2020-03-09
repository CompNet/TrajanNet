#############################################################################################
# Functions used to check the raw data.
# 
# 01/2019 Vincent Labatut
#
# setwd("C:/users/Vincent/Eclipse/workspaces/Networks/TrajanNet")
# setwd("~/eclipse/workspaces/Networks/TrajanNet")
# source("src/verification.R")
#############################################################################################
# load other scripts
source("src/constants.R")




#################################
# loading data
#################################
cat(">>>>>>>>>>> Loading data\n")

# load node attributes
attr.file <- file.path(TABLE_FOLDER,"trajan_attributes.csv")
attr.data <- as.matrix(read.csv(file=attr.file,header=TRUE,check.names=FALSE))

# load relations
rel.file <- file.path(TABLE_FOLDER,"trajan_relations.csv")
rel.data <- as.matrix(read.csv(file=rel.file,header=TRUE,check.names=FALSE))




#################################
# checking individual attributes
#################################
cat(">>>>>>>>>>> Checking individual attributes\n")

# unique values in PolitSenat after splitting multiple values
u.vals <- sort(unique(unlist(strsplit(x=attr.data[,ATT_NODE_SEN_POL], split=";"))))
cat(ATT_NODE_SEN_POL,":\n",sep="");print(u.vals);cat("\n")

# unique values in DerPolitSenat after splitting multiple values
u.vals <- sort(unique(unlist(strsplit(x=attr.data[,ATT_NODE_SEN_POLDER], split=";"))))
cat(ATT_NODE_SEN_POLDER,":\n",sep="");print(u.vals);cat("\n")

# unique values in PolitEques after splitting multiple values
u.vals <- sort(unique(unlist(strsplit(x=attr.data[,ATT_NODE_EQU_POL], split=";"))))
cat(ATT_NODE_EQU_POL,":\n",sep="");print(u.vals);cat("\n")

# unique values in DerPolitEques after splitting multiple values
u.vals <- sort(unique(unlist(strsplit(x=attr.data[,ATT_NODE_EQU_POLDER], split=";"))))
cat(ATT_NODE_EQU_POLDER,":\n",sep="");print(u.vals);cat("\n")

# compare DerPolitSenat and DerPolitEques (supposedly mutually exclusive, except if Adelectio)
m <- cbind(attr.data[,ATT_NODE_SEN_POLDER],attr.data[,ATT_NODE_EQU_POLDER],
		(is.na(attr.data[,ATT_NODE_SEN_POLDER]) & is.na(attr.data[,ATT_NODE_EQU_POLDER])) 
				| xor(is.na(attr.data[,ATT_NODE_SEN_POLDER]),is.na(attr.data[,ATT_NODE_EQU_POLDER])),
		attr.data[,ATT_NODE_LACTICLAVIUS]
	)
cat("Comparison Polit:\n");print(m);cat("\n")

# unique values in Adelectio after splitting multiple values
u.vals <- sort(unique(unlist(strsplit(x=attr.data[,ATT_NODE_LACTICLAVIUS], split=";"))))
cat(ATT_NODE_LACTICLAVIUS,":\n",sep="");print(u.vals);cat("\n")

# unique values in MilitSenat after splitting multiple values
u.vals <- sort(unique(unlist(strsplit(x=attr.data[,ATT_NODE_SEN_MILIT], split=";"))))
cat(ATT_NODE_SEN_MILIT,":\n",sep="");print(u.vals);cat("\n")

# unique values in MilitEques after splitting multiple values
u.vals <- sort(unique(unlist(strsplit(x=attr.data[,ATT_NODE_EQU_MILIT], split=";"))))
cat(ATT_NODE_EQU_MILIT,":\n",sep="");print(u.vals);cat("\n")

# compare MilitSenat and MilitEques (supposedly mutually exclusive, except if Adelectio)
m <- cbind(attr.data[,ATT_NODE_SEN_MILIT],attr.data[,ATT_NODE_EQU_MILIT],
		(is.na(attr.data[,ATT_NODE_SEN_MILIT]) & is.na(attr.data[,ATT_NODE_EQU_MILIT])) 
				| xor(is.na(attr.data[,ATT_NODE_SEN_MILIT]),is.na(attr.data[,ATT_NODE_EQU_MILIT])),
		attr.data[,ATT_NODE_LACTICLAVIUS]
)
cat("Comparison Milit:\n");print(m);cat("\n")

# unique values in NbrVoy after splitting multiple values
u.vals <- sort(unique(unlist(strsplit(x=attr.data[,ATT_NODE_TRAV_NBR], split=";"))))
cat(ATT_NODE_TRAV_NBR,":\n",sep="");print(u.vals);cat("\n")

# unique values in DestVoy after splitting multiple values
u.vals <- sort(unique(unlist(strsplit(x=attr.data[,ATT_NODE_TRAV_DEST], split=";"))))
cat(ATT_NODE_TRAV_DEST,":\n",sep="");print(u.vals);cat("\n")

# unique values in MotifVoy after splitting multiple values
u.vals <- sort(unique(unlist(strsplit(x=attr.data[,ATT_NODE_TRAV_REAS], split=";"))))
cat(ATT_NODE_TRAV_REAS,":\n",sep="");print(u.vals);cat("\n")

# compare NbrVoy with multiplicity of DestVoy and MotifVoy
nbr.voy <- as.integer(attr.data[,ATT_NODE_TRAV_NBR])
nbr.loc <- sapply(strsplit(x=attr.data[,ATT_NODE_TRAV_DEST], split=";"),length)
nbr.mot <- sapply(strsplit(x=attr.data[,ATT_NODE_TRAV_REAS], split=";"),length)
m <- cbind(
	nbr.voy,
	nbr.loc,
	nbr.loc==nbr.voy,
	nbr.mot,
	nbr.mot==nbr.voy
	)
cat("Comparison:\n");print(m);cat("\n")

# unique values in RelTrajan after splitting multiple values
u.vals <- sort(unique(unlist(strsplit(x=attr.data[,ATT_NODE_REL_TRAJ], split=";"))))
cat(ATT_NODE_REL_TRAJ,":\n",sep="");print(u.vals);cat("\n")

# unique values in SoutHadrien after splitting multiple values
u.vals <- sort(unique(unlist(strsplit(x=attr.data[,ATT_NODE_REL_HADR], split=";"))))
cat(ATT_NODE_REL_HADR,":\n",sep="");print(u.vals);cat("\n")

# unique values in Cercles after splitting multiple values
u.vals <- sort(unique(unlist(strsplit(x=attr.data[,ATT_NODE_CIRCLES], split=";"))))
cat(ATT_NODE_CIRCLES,":\n",sep="");print(u.vals);cat("\n")

# unique values in Espagnol after splitting multiple values
u.vals <- sort(unique(unlist(strsplit(x=attr.data[,ATT_NODE_SPANISH], split=";"))))
cat(ATT_NODE_SPANISH,":\n",sep="");print(u.vals);cat("\n")




#################################
# checking interaction attributes
#################################
cat(">>>>>>>>>>> Checking interaction attributes\n")

# unique node ids
u.vals <- sort(unique(c(rel.data[,c(ATT_EDGE_ID1,ATT_EDGE_ID2)])))
cat("Ids:\n");print(u.vals);cat("\n")

# unique link natures
u.vals <- sort(unique(unlist(strsplit(x=rel.data[,ATT_EDGE_NAT], split=";"))))
cat(ATT_EDGE_NAT,":\n",sep="");print(u.vals);cat("\n")

# unique link polarities
u.vals <- sort(unique(c(rel.data[,ATT_EDGE_POL])))
cat(ATT_EDGE_POL,":\n",sep="");print(u.vals);cat("\n")

