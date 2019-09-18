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
u.vals <- sort(unique(unlist(strsplit(x=attr.data[,"PolitSenat"], split=";"))))
cat("PolitSenat:\n");print(u.vals);cat("\n")

# unique values in DerPolitSenat after splitting multiple values
u.vals <- sort(unique(unlist(strsplit(x=attr.data[,"DerPolitSenat"], split=";"))))
cat("DerPolitSenat:\n");print(u.vals);cat("\n")

# unique values in PolitEques after splitting multiple values
u.vals <- sort(unique(unlist(strsplit(x=attr.data[,"PolitEques"], split=";"))))
cat("PolitEques:\n");print(u.vals);cat("\n")

# unique values in DerPolitEques after splitting multiple values
u.vals <- sort(unique(unlist(strsplit(x=attr.data[,"DerPolitEques"], split=";"))))
cat("DerPolitEques:\n");print(u.vals);cat("\n")

# compare DerPolitSenat and DerPolitEques (supposedly mutually exclusive, except if Adelectio)
m <- cbind(attr.data[,"DerPolitSenat"],attr.data[,"DerPolitEques"],
		(is.na(attr.data[,"DerPolitSenat"]) & is.na(attr.data[,"DerPolitEques"])) | xor(is.na(attr.data[,"DerPolitSenat"]),is.na(attr.data[,"DerPolitEques"])),
		attr.data[,"Adelectio"]
	)
cat("Comparison Polit:\n");print(m);cat("\n")

# unique values in Adelectio after splitting multiple values
u.vals <- sort(unique(unlist(strsplit(x=attr.data[,"Adelectio"], split=";"))))
cat("Adelectio:\n");print(u.vals);cat("\n")

# unique values in MilitSenat after splitting multiple values
u.vals <- sort(unique(unlist(strsplit(x=attr.data[,"MilitSenat"], split=";"))))
cat("MilitSenat:\n");print(u.vals);cat("\n")

# unique values in MilitEques after splitting multiple values
u.vals <- sort(unique(unlist(strsplit(x=attr.data[,"MilitEques"], split=";"))))
cat("MilitEques:\n");print(u.vals);cat("\n")

# compare MilitSenat and MilitEques (supposedly mutually exclusive, except if Adelectio)
m <- cbind(attr.data[,"MilitSenat"],attr.data[,"MilitEques"],
		(is.na(attr.data[,"MilitSenat"]) & is.na(attr.data[,"MilitEques"])) | xor(is.na(attr.data[,"MilitSenat"]),is.na(attr.data[,"MilitEques"])),
		attr.data[,"Adelectio"]
)
cat("Comparison Milit:\n");print(m);cat("\n")

# unique values in NbrVoy after splitting multiple values
u.vals <- sort(unique(unlist(strsplit(x=attr.data[,"NbrVoy"], split=";"))))
cat("NbrVoy:\n");print(u.vals);cat("\n")

# unique values in DestVoy after splitting multiple values
u.vals <- sort(unique(unlist(strsplit(x=attr.data[,"DestVoy"], split=";"))))
cat("DestVoy:\n");print(u.vals);cat("\n")

# unique values in MotifVoy after splitting multiple values
u.vals <- sort(unique(unlist(strsplit(x=attr.data[,"MotifVoy"], split=";"))))
cat("MotifVoy:\n");print(u.vals);cat("\n")

# compare NbrVoy with multiplicity of DestVoy and MotifVoy
nbr.voy <- as.integer(attr.data[,"NbrVoy"])
nbr.loc <- sapply(strsplit(x=attr.data[,"DestVoy"], split=";"),length)
nbr.mot <- sapply(strsplit(x=attr.data[,"MotifVoy"], split=";"),length)
m <- cbind(
	nbr.voy,
	nbr.loc,
	nbr.loc==nbr.voy,
	nbr.mot,
	nbr.mot==nbr.voy
	)
cat("Comparison:\n");print(m);cat("\n")

# unique values in RelTrajan after splitting multiple values
u.vals <- sort(unique(unlist(strsplit(x=attr.data[,"RelTrajan"], split=";"))))
cat("RelTrajan:\n");print(u.vals);cat("\n")

# unique values in SoutHadrien after splitting multiple values
u.vals <- sort(unique(unlist(strsplit(x=attr.data[,"SoutHadrien"], split=";"))))
cat("SoutHadrien:\n");print(u.vals);cat("\n")

# unique values in Cercles after splitting multiple values
u.vals <- sort(unique(unlist(strsplit(x=attr.data[,"Cercles"], split=";"))))
cat("Cercles:\n");print(u.vals);cat("\n")

# unique values in Espagnol after splitting multiple values
u.vals <- sort(unique(unlist(strsplit(x=attr.data[,"Espagnol"], split=";"))))
cat("Espagnol:\n");print(u.vals);cat("\n")




#################################
# checking interaction attributes
#################################
cat(">>>>>>>>>>> Checking interaction attributes\n")

# unique node ids
u.vals <- sort(unique(c(rel.data[,c("Id1","Id2")])))
cat("Ids:\n");print(u.vals);cat("\n")

# unique link natures
u.vals <- sort(unique(unlist(strsplit(x=rel.data[,"Nature"], split=";"))))
cat("Nature:\n");print(u.vals);cat("\n")

# unique link polarities
u.vals <- sort(unique(c(rel.data[,"Polarité"])))
cat("Polarité:\n");print(u.vals);cat("\n")

