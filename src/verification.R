#############################################################################################
# Functions used to check the raw data.
# 
# 01/2019 Vincent Labatut
#
# setwd("D:/Eclipse/workspaces/Networks/TrajanNet")
# setwd("~/eclipse/workspaces/Networks/TrajanNet")
# source("src/verification.R")
#############################################################################################


#################################
# checking individual attributes
#################################
# unique values in Senat after splitting multiple values
u.vals <- sort(unique(unlist(strsplit(x=attr.data[,"Senat"], split=";"))))
cat("Senat:\n");print(u.vals);cat("\n")

# unique values in DerSenat after splitting multiple values
u.vals <- sort(unique(unlist(strsplit(x=attr.data[,"DerSenat"], split=";"))))
cat("DerSenat:\n");print(u.vals);cat("\n")

# unique values in Eques after splitting multiple values
u.vals <- sort(unique(unlist(strsplit(x=attr.data[,"Eques"], split=";"))))
cat("Eques:\n");print(u.vals);cat("\n")

# unique values in DerEques after splitting multiple values
u.vals <- sort(unique(unlist(strsplit(x=attr.data[,"DerEques"], split=";"))))
cat("DerEques:\n");print(u.vals);cat("\n")

# compare DerSenat and DerEques (supposedly mutually exclusive)
m <- cbind(attr.data[,"DerSenat"],attr.data[,"DerEques"],
		(is.na(attr.data[,"DerSenat"]) & is.na(attr.data[,"DerEques"])) | xor(is.na(attr.data[,"DerSenat"]),is.na(attr.data[,"DerEques"])))
cat("Comparison:\n");print(m);cat("\n")

# unique values in Adelectio after splitting multiple values
u.vals <- sort(unique(unlist(strsplit(x=attr.data[,"Adelectio"], split=";"))))
cat("Adelectio:\n");print(u.vals);cat("\n")

# unique values in MilitSenat after splitting multiple values
u.vals <- sort(unique(unlist(strsplit(x=attr.data[,"MilitSenat"], split=";"))))
cat("MilitSenat:\n");print(u.vals);cat("\n")

# unique values in MilitEques after splitting multiple values
u.vals <- sort(unique(unlist(strsplit(x=attr.data[,"MilitEques"], split=";"))))
cat("MilitEques:\n");print(u.vals);cat("\n")

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
# unique node ids
u.vals <- sort(unique(c(rel.data[,c("Id1","Id2")])))
cat("Ids:\n");print(u.vals);cat("\n")

# unique link natures
u.vals <- sort(unique(unlist(strsplit(x=rel.data[,"Nature"], split=";"))))
cat("Nature:\n");print(u.vals);cat("\n")

# unique link polarities
u.vals <- sort(unique(c(rel.data[,"Polarité"])))
cat("Polarité:\n");print(u.vals);cat("\n")

