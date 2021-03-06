#############################################################################################
# File- and folder-related constants.
# 
# 09/2019 Vincent Labatut
#
# setwd("C:/users/Vincent/Eclipse/workspaces/Networks/TrajanNet")
# setwd("~/eclipse/workspaces/Networks/TrajanNet")
# source("src/common/analysis.R")
#############################################################################################




#############################################################################################
# folder names
DATA_FOLDER <- "data"
	TABLE_FOLDER <- file.path(DATA_FOLDER,"tables")
	NET_FOLDER <- file.path(DATA_FOLDER,"nets")
		SIGNED_FOLDER <- file.path(NET_FOLDER,"signed")
	ATT_FOLDER <- file.path(DATA_FOLDER,"attributes")
		COMP_FOLDER <- file.path(ATT_FOLDER,"comparisons")
	SEQ_FOLDER <- file.path(DATA_FOLDER,"sequences")
	



#############################################################################################
# vertex attribute names
ATT_NODE_ID <- "Id"
ATT_NODE_NAME <- "NomComp"
ATT_NODE_NAME_SHORT <- "NomCourt"
ATT_NODE_SEN_MILIT <- "MilitSenat"
ATT_NODE_SEN_POL <- "PolitSenat"
ATT_NODE_SEN_POLDER <- "DerPolitSenat"
ATT_NODE_EQU_MILIT <- "MilitEques"
ATT_NODE_EQU_POL <- "PolitEques"
ATT_NODE_EQU_POLDER <- "DerPolitEques"
#ATT_NODE_ADELECTIO <- "Adelectio"
ATT_NODE_LATICLAVIUS <- "Laticlavius"
ATT_NODE_TRAV_NBR <- "NbrVoy"
ATT_NODE_TRAV_DEST <- "DestVoy"
ATT_NODE_TRAV_REAS <- "MotifVoy"
ATT_NODE_REL_TRAJ <- "RelTrajan"
ATT_NODE_REL_HADR <- "SoutHadrien"
ATT_NODE_CIRCLES <- "Cercles"
ATT_NODE_CIRCLES_VALS <- c("Antonins","Nigrinus","Pline","Regulus","Clarus")
ATT_NODE_SPANISH <- "Espagnol"
ATT_NODE_IMAGE <- "Image"

# edge attribute names
ATT_EDGE_ID1 <- "Id1"
ATT_EDGE_ID2 <- "Id2"
ATT_EDGE_NAT <- "Nature"
ATT_EDGE_POL <- "Polarite"
ATT_EDGE_SIGN <- "sign"

# attribute values
ATT_VAL_TRUE <- "Oui"
ATT_VAL_FALSE <- "Non"
ATT_VAL_POSITIVE <- "Positive"
ATT_VAL_NEGATIVE <- "Negative"
ATT_VAL_FAMILY <- "Familiale"
ATT_VAL_FRIEND <- "Amicale"
ATT_VAL_PRO <- "Professionnelle"
ATT_VAL_UNK <- "Inconnue"
ATT_VAL_UNK0 <- "Inconnu(e)"
ATT_VAL_OTHER <- "Autre"

# circle values
ATT_VAL_CIRCLE_ANTONINS <- "Cercles_Antonins"
ATT_VAL_CIRCLE_NIGRINUS <- "Cercles_Nigrinus"
ATT_VAL_CIRCLE_PLINE <- "Cercles_Pline"
ATT_VAL_CIRCLE_REGULUS <- "Cercles_Regulus"
ATT_VAL_CIRCLE_CLARUS <- "Cercles_Septicius Clarus"
ATT_VAL_CIRCLE_VALS <- c(ATT_VAL_CIRCLE_ANTONINS,ATT_VAL_CIRCLE_NIGRINUS,ATT_VAL_CIRCLE_PLINE,ATT_VAL_CIRCLE_REGULUS,ATT_VAL_CIRCLE_CLARUS)

# country values
ATT_VAL_DEST_ACHA <- "Achaie"
ATT_VAL_DEST_AFRI <- "Afrique"
ATT_VAL_DEST_ARAB <- "Arabie"
ATT_VAL_DEST_ARME <- "Armenie"
ATT_VAL_DEST_ASIE <- "Asie"
ATT_VAL_DEST_BABY <- "Babylone"
ATT_VAL_DEST_BELG <- "Belgique"
ATT_VAL_DEST_BETI <- "Betique"
ATT_VAL_DEST_BITH <- "Bithynie"
ATT_VAL_DEST_BRET <- "Bretagne"
ATT_VAL_DEST_CAPP <- "Cappadoce"
ATT_VAL_DEST_CILI <- "Cilicie"
ATT_VAL_DEST_CYRE <- "Cyrenaique"
ATT_VAL_DEST_DACI <- "Dacie"
ATT_VAL_DEST_DAML <- "Dalmatie"
ATT_VAL_DEST_EGYP <- "Egypte"
ATT_VAL_DEST_GALA <- "Galatie"
ATT_VAL_DEST_GAUL <- "Gaule cisalpine"
ATT_VAL_DEST_GERM <- "Germanie"
ATT_VAL_DEST_GERI <- "Germanie inferieure"
ATT_VAL_DEST_GERS <- "Germanie superieure"
ATT_VAL_DEST_GREC <- "Grece"
ATT_VAL_DEST_HISP <- "Hispanie citerieure"
ATT_VAL_DEST_JUDE <- "Judee"
ATT_VAL_DEST_LIBY <- "Libye"
ATT_VAL_DEST_MACE <- "Macedoine"
ATT_VAL_DEST_MAUR <- "Mauritanie cesarienne"
ATT_VAL_DEST_MESE <- "Mesie"
ATT_VAL_DEST_MESI <- "Mesie inferieure"
ATT_VAL_DEST_MESS <- "Mesie superieure"
ATT_VAL_DEST_MESO <- "Mesopotamie"
ATT_VAL_DEST_NARB <- "Narbonnaise"
ATT_VAL_DEST_NUMI <- "Numidie"
ATT_VAL_DEST_PALE <- "Palestine"
ATT_VAL_DEST_PAMP <- "Pamphylie"
ATT_VAL_DEST_PANN <- "Pannonie"
ATT_VAL_DEST_PANS <- "Pannonie superieure"
ATT_VAL_DEST_PART <- "Parthie"
ATT_VAL_DEST_SYRI <- "Syrie"
ATT_VAL_DEST_TARR <- "Tarraconaise"

# travel reason values
ATT_VAL_REAS_MILI <- "Militaire"
ATT_VAL_REAS_PERS <- "Personnel"
ATT_VAL_REAS_POMI <- "Politico-militaire"
ATT_VAL_REAS_POLI <- "Politique"

# position values
ATT_VAL_POS_CONS <- "Consul"
ATT_VAL_POS_COSS <- "Consul suffect"
ATT_VAL_POS_EDIL <- "Edile"
ATT_VAL_POS_GARN <- "Garnison de Rome"
ATT_VAL_POS_LEAP <- "Legat d'Auguste propreteur"
ATT_VAL_POS_LEDL <- "Legat de legion"
ATT_VAL_POS_PREG <- "Prefet d'Egypte"
ATT_VAL_POS_PRAI <- "Prefet d'ailes"
ATT_VAL_POS_PRCO <- "Prefet de cohorte"
ATT_VAL_POS_PRAN <- "Prefet de l'annone"
ATT_VAL_POS_PRFL <- "Prefet de la flotte"
ATT_VAL_POS_PRVI <- "Prefet de la ville"
ATT_VAL_POS_PRVG <- "Prefet des vigiles"
ATT_VAL_POS_PRPR <- "Prefet du pretoire"
ATT_VAL_POS_PRET <- "Preteur"
ATT_VAL_POS_PROC <- "Proconsul"
ATT_VAL_POS_PROU <- "Procuratele"
ATT_VAL_POS_PROP <- "Propreteur"
ATT_VAL_POS_QUES <- "Questeur"
ATT_VAL_POS_TRAN <- "Tribun angusticlave"
ATT_VAL_POS_TRCP <- "Tribun d'une cohorte pr�torienne"
ATT_VAL_POS_TRCU <- "Tribun d'une cohorte urbaine"
ATT_VAL_POS_TRPL <- "Tribun de la plebe"
ATT_VAL_POS_TRLE <- "Tribun de legion"
ATT_VAL_POS_TRVI <- "Tribun des vigiles"
ATT_VAL_POS_TRLA <- "Tribun laticlave"




#############################################################################################
# graph types
GRAPH_TYPE_ALL <- "all"
GRAPH_TYPE_FAMILY <- "family"
GRAPH_TYPE_FRIEND <- "friend"
GRAPH_TYPE_PRO <- "pro"
GRAPH_TYPE_UNK <- "unknown"




#############################################################################################
# measure names
MEAS_ECCENTRICITY <- "Eccentricity"
MEAS_DIAMETER <- "Diameter"
MEAS_RADIUS <- "Radius"
MEAS_DENSITY <- "Density"
MEAS_DEGREE <- "Degree"
MEAS_DEGREE_POS <- "DegreePos"
MEAS_DEGREE_NEG <- "DegreeNeg"
MEAS_EIGEN <- "Eigencentrality"
MEAS_BETWEENNESS <- "Betweenness"
MEAS_CLOSENESS <- "Closeness"
MEAS_CLOSENESS_HARM <- "ClosenessHarm"
MEAS_TRANSITIVITY <- "Transitivity"
MEAS_COMMUNITY_ONLYPOS <- "CommunityOnlyPositive"
MEAS_COMMUNITY_NONEG <- "CommunityNoNegative"
MEAS_MODULARITY_ONLYPOS <- "ModularityOnlyPositive"
MEAS_MODULARITY_NONEG <- "ModularityNoNegative"
MEAS_NBR_NODES <- "NbrNodes"
MEAS_NBR_LINKS <- "NbrLinks"
MEAS_ARTICULATION <- "Articulation"
MEAS_DISTANCE <- "Distance"
MEAS_DISTANCE_AVG <- "AverageDistance"
MEAS_CONNECTIVITY <- "Connectivity"
MEAS_CONNECTIVITY_AVG <- "AverageConnectivity"
MEAS_STR_STRUCT_BAL <- "StrictStructuralBalance"
MEAS_GEN_STRUCT_BAL <- "GeneralizedStructuralBalance"
MEAS_TRI_SIGNED <- "TriangleSigned"
MEAS_SIGN_CENTR <- "PNindex"
MEAS_COR_CLUST <- "CorrelationClustering"




#############################################################################################
# attribute long names
LONG_NAME <- c()
LONG_NAME[ATT_NODE_ID] <- "Id"
LONG_NAME[ATT_NODE_NAME] <- "Nom complet"
LONG_NAME[ATT_NODE_NAME_SHORT] <- "Nom court"
LONG_NAME[ATT_NODE_SEN_MILIT] <- "Poste militaire senatorial"
LONG_NAME[ATT_NODE_SEN_POL] <- "Poste politique senatorial connu"
LONG_NAME[ATT_NODE_SEN_POLDER] <- "Dernier poste politique senatorial"
LONG_NAME[ATT_NODE_EQU_MILIT] <- "Poste militaire equestre"
LONG_NAME[ATT_NODE_EQU_POL] <- "Poste politique equestre"
LONG_NAME[ATT_NODE_EQU_POLDER] <- "Dernier poste politique equestre"
#LONG_NAME[ATT_NODE_ADELECTIO] <- "Adelectio"
LONG_NAME[ATT_NODE_LATICLAVIUS] <- "Laticlavius"
LONG_NAME[ATT_NODE_TRAV_NBR] <- "Nombre de voyages"
LONG_NAME[ATT_NODE_TRAV_DEST] <- "Destination du voyage"
LONG_NAME[ATT_NODE_TRAV_REAS] <- "Motif du voyage"
LONG_NAME[ATT_NODE_REL_TRAJ] <- "Relation avec Trajan"
LONG_NAME[ATT_NODE_REL_HADR] <- "Soutien de Hadrien"
LONG_NAME[ATT_NODE_CIRCLES] <- "Cercles"
LONG_NAME[ATT_NODE_SPANISH] <- "Espagnol"
LONG_NAME[ATT_EDGE_NAT] <- "Nature"
LONG_NAME[ATT_EDGE_POL] <- "Polarite"

# attribute value long names
LONG_NAME[ATT_VAL_TRUE] <- "Oui"
LONG_NAME[ATT_VAL_FALSE] <- "Non"
LONG_NAME[ATT_VAL_POSITIVE] <- "Positive"
LONG_NAME[ATT_VAL_NEGATIVE] <- "Negative"
LONG_NAME[ATT_VAL_FAMILY] <- "Familiale"
LONG_NAME[ATT_VAL_FRIEND] <- "Amicale"
LONG_NAME[ATT_VAL_PRO] <- "Professionnelle"
LONG_NAME[ATT_VAL_UNK] <- "Inconnue"
LONG_NAME[ATT_VAL_UNK0] <- "Inconnu(e)"
LONG_NAME[ATT_VAL_CIRCLE_ANTONINS] <- "Cercle d'Antonins"
LONG_NAME[ATT_VAL_CIRCLE_NIGRINUS] <- "Cercle de Nigrinus"
LONG_NAME[ATT_VAL_CIRCLE_PLINE] <- "Cercle de Pline"
LONG_NAME[ATT_VAL_CIRCLE_REGULUS] <- "Cercle de Regulus"
LONG_NAME[ATT_VAL_CIRCLE_CLARUS] <- "Cercle de Septicius Clarus"
LONG_NAME[paste0(ATT_NODE_TRAV_DEST,"_",ATT_VAL_DEST_ACHA)] <- "Voyage en Achaie"
LONG_NAME[paste0(ATT_NODE_TRAV_DEST,"_",ATT_VAL_DEST_AFRI)] <- "Voyage en Afrique"
LONG_NAME[paste0(ATT_NODE_TRAV_DEST,"_",ATT_VAL_DEST_ARAB)] <- "Voyage en Arabie"
LONG_NAME[paste0(ATT_NODE_TRAV_DEST,"_",ATT_VAL_DEST_ARME)] <- "Voyage en Armenie"
LONG_NAME[paste0(ATT_NODE_TRAV_DEST,"_",ATT_VAL_DEST_ASIE)] <- "Voyage en Asie"
LONG_NAME[paste0(ATT_NODE_TRAV_DEST,"_",ATT_VAL_DEST_BABY)] <- "Voyage en Babylone"
LONG_NAME[paste0(ATT_NODE_TRAV_DEST,"_",ATT_VAL_DEST_BELG)] <- "Voyage en Belgique"
LONG_NAME[paste0(ATT_NODE_TRAV_DEST,"_",ATT_VAL_DEST_BETI)] <- "Voyage en Betique"
LONG_NAME[paste0(ATT_NODE_TRAV_DEST,"_",ATT_VAL_DEST_BITH)] <- "Voyage en Bithynie"
LONG_NAME[paste0(ATT_NODE_TRAV_DEST,"_",ATT_VAL_DEST_BRET)] <- "Voyage en Bretagne"
LONG_NAME[paste0(ATT_NODE_TRAV_DEST,"_",ATT_VAL_DEST_CAPP)] <- "Voyage en Cappadoce"
LONG_NAME[paste0(ATT_NODE_TRAV_DEST,"_",ATT_VAL_DEST_CILI)] <- "Voyage en Cilicie"
LONG_NAME[paste0(ATT_NODE_TRAV_DEST,"_",ATT_VAL_DEST_CYRE)] <- "Voyage en Cyrenaique"
LONG_NAME[paste0(ATT_NODE_TRAV_DEST,"_",ATT_VAL_DEST_DACI)] <- "Voyage en Dacie"
LONG_NAME[paste0(ATT_NODE_TRAV_DEST,"_",ATT_VAL_DEST_DAML)] <- "Voyage en Dalmatie"
LONG_NAME[paste0(ATT_NODE_TRAV_DEST,"_",ATT_VAL_DEST_EGYP)] <- "Voyage en Egypte"
LONG_NAME[paste0(ATT_NODE_TRAV_DEST,"_",ATT_VAL_DEST_GALA)] <- "Voyage en Galatie"
LONG_NAME[paste0(ATT_NODE_TRAV_DEST,"_",ATT_VAL_DEST_GAUL)] <- "Voyage en Gaule cisalpine"
LONG_NAME[paste0(ATT_NODE_TRAV_DEST,"_",ATT_VAL_DEST_GERM)] <- "Voyage en Germanie"
LONG_NAME[paste0(ATT_NODE_TRAV_DEST,"_",ATT_VAL_DEST_GERI)] <- "Voyage en Germanie inferieure"
LONG_NAME[paste0(ATT_NODE_TRAV_DEST,"_",ATT_VAL_DEST_GERS)] <- "Voyage en Germanie superieure"
LONG_NAME[paste0(ATT_NODE_TRAV_DEST,"_",ATT_VAL_DEST_GREC)] <- "Voyage en Grece"
LONG_NAME[paste0(ATT_NODE_TRAV_DEST,"_",ATT_VAL_DEST_HISP)] <- "Voyage en Hispanie citerieure"
LONG_NAME[paste0(ATT_NODE_TRAV_DEST,"_",ATT_VAL_DEST_JUDE)] <- "Voyage en Judee"
LONG_NAME[paste0(ATT_NODE_TRAV_DEST,"_",ATT_VAL_DEST_LIBY)] <- "Voyage en Libye"
LONG_NAME[paste0(ATT_NODE_TRAV_DEST,"_",ATT_VAL_DEST_MACE)] <- "Voyage en Macedoine"
LONG_NAME[paste0(ATT_NODE_TRAV_DEST,"_",ATT_VAL_DEST_MAUR)] <- "Voyage en Mauritanie cesarienne"
LONG_NAME[paste0(ATT_NODE_TRAV_DEST,"_",ATT_VAL_DEST_MESE)] <- "Voyage en Mesie"
LONG_NAME[paste0(ATT_NODE_TRAV_DEST,"_",ATT_VAL_DEST_MESI)] <- "Voyage en Mesie inferieure"
LONG_NAME[paste0(ATT_NODE_TRAV_DEST,"_",ATT_VAL_DEST_MESS)] <- "Voyage en Mesie superieure"
LONG_NAME[paste0(ATT_NODE_TRAV_DEST,"_",ATT_VAL_DEST_MESO)] <- "Voyage en Mesopotamie"
LONG_NAME[paste0(ATT_NODE_TRAV_DEST,"_",ATT_VAL_DEST_NARB)] <- "Voyage en Narbonnaise"
LONG_NAME[paste0(ATT_NODE_TRAV_DEST,"_",ATT_VAL_DEST_NUMI)] <- "Voyage en Numidie"
LONG_NAME[paste0(ATT_NODE_TRAV_DEST,"_",ATT_VAL_DEST_PALE)] <- "Voyage en Palestine"
LONG_NAME[paste0(ATT_NODE_TRAV_DEST,"_",ATT_VAL_DEST_PAMP)] <- "Voyage en Pamphylie"
LONG_NAME[paste0(ATT_NODE_TRAV_DEST,"_",ATT_VAL_DEST_PANN)] <- "Voyage en Pannonie"
LONG_NAME[paste0(ATT_NODE_TRAV_DEST,"_",ATT_VAL_DEST_PANS)] <- "Voyage en Pannonie superieure"
LONG_NAME[paste0(ATT_NODE_TRAV_DEST,"_",ATT_VAL_DEST_PART)] <- "Voyage en Parthie"
LONG_NAME[paste0(ATT_NODE_TRAV_DEST,"_",ATT_VAL_DEST_SYRI)] <- "Voyage en Syrie"
LONG_NAME[paste0(ATT_NODE_TRAV_DEST,"_",ATT_VAL_DEST_TARR)] <- "Voyage en Tarraconaise"
LONG_NAME[paste0(ATT_NODE_SEN_POL,"_",ATT_VAL_POS_CONS)] <- "Poste de Consul"
LONG_NAME[paste0(ATT_NODE_SEN_POL,"_",ATT_VAL_POS_COSS)] <- "Poste de Consul suffect"
LONG_NAME[paste0(ATT_NODE_SEN_POL,"_",ATT_VAL_POS_EDIL)] <- "Poste d'Edile"
LONG_NAME[paste0(ATT_NODE_EQU_POL,"_",ATT_VAL_POS_GARN)] <- "Poste � la Garnison de Rome"
LONG_NAME[paste0(ATT_NODE_SEN_MILIT,"_",ATT_VAL_POS_LEAP)] <- "Poste de Legat d'Auguste propreteur"
LONG_NAME[paste0(ATT_NODE_SEN_MILIT,"_",ATT_VAL_POS_LEDL)] <- "Poste de Legat de legion"
LONG_NAME[paste0(ATT_NODE_EQU_POL,"_",ATT_VAL_POS_PREG)] <- "Poste de Prefet d'Egypte"
LONG_NAME[paste0(ATT_NODE_EQU_MILIT,"_",ATT_VAL_POS_PRAI)] <- "Poste de Prefet d'ailes"
LONG_NAME[paste0(ATT_NODE_EQU_MILIT,"_",ATT_VAL_POS_PRCO)] <- "Poste de Prefet de cohorte"
LONG_NAME[paste0(ATT_NODE_EQU_POL,"_",ATT_VAL_POS_PRAN)] <- "Poste de Prefet de l'annone"
LONG_NAME[paste0(ATT_NODE_EQU_POL,"_",ATT_VAL_POS_PRFL)] <- "Poste de Prefet de la flotte"
LONG_NAME[paste0(ATT_NODE_SEN_POL,"_",ATT_VAL_POS_PRVI)] <- "Poste de Prefet de la ville"
LONG_NAME[paste0(ATT_NODE_EQU_POL,"_",ATT_VAL_POS_PRVG)] <- "Poste de Prefet des vigiles"
LONG_NAME[paste0(ATT_NODE_EQU_POL,"_",ATT_VAL_POS_PRPR)] <- "Poste de Prefet du pretoire"
LONG_NAME[paste0(ATT_NODE_SEN_POL,"_",ATT_VAL_POS_PRET)] <- "Poste de Preteur"
LONG_NAME[paste0(ATT_NODE_EQU_POL,"_",ATT_VAL_POS_PROU)] <- "Poste de Procuratele"
LONG_NAME[paste0(ATT_NODE_SEN_POL,"_",ATT_VAL_POS_PROP)] <- "Poste de Propreteur"
LONG_NAME[paste0(ATT_NODE_SEN_POL,"_",ATT_VAL_POS_QUES)] <- "Poste de Questeur"
LONG_NAME[paste0(ATT_NODE_EQU_MILIT,"_",ATT_VAL_POS_TRAN)] <- "Poste de Tribun angusticlave"
LONG_NAME[paste0(ATT_NODE_EQU_MILIT,"_",ATT_VAL_POS_TRCP)] <- "Poste de Tribun d'une cohorte pr�torienne"
LONG_NAME[paste0(ATT_NODE_EQU_MILIT,"_",ATT_VAL_POS_TRCU)] <- "Poste de Tribun d'une cohorte urbaine"
LONG_NAME[paste0(ATT_NODE_SEN_POL,"_",ATT_VAL_POS_TRPL)] <- "Poste de Tribun de la plebe"
LONG_NAME[paste0(ATT_NODE_SEN_MILIT,"_",ATT_VAL_POS_PROC)] <- "Poste de Proconsul"
LONG_NAME[paste0(ATT_NODE_SEN_MILIT,"_",ATT_VAL_POS_TRLE)] <- "Poste de Tribun de legion"
LONG_NAME[paste0(ATT_NODE_EQU_MILIT,"_",ATT_VAL_POS_TRVI)] <- "Poste de Tribun des vigiles"
LONG_NAME[paste0(ATT_NODE_SEN_MILIT,"_",ATT_VAL_POS_TRLA)] <- "Poste de Tribun laticlave"
LONG_NAME[paste0(ATT_NODE_TRAV_REAS,"_",ATT_VAL_REAS_MILI)] <- "Voyage a but Militaire"
LONG_NAME[paste0(ATT_NODE_TRAV_REAS,"_",ATT_VAL_REAS_PERS)] <- "Voyage a but Personnel"
LONG_NAME[paste0(ATT_NODE_TRAV_REAS,"_",ATT_VAL_REAS_POMI)] <- "Voyage a but Politico-militaire"
LONG_NAME[paste0(ATT_NODE_TRAV_REAS,"_",ATT_VAL_REAS_POLI)] <- "Voyage a but Politique"

# measure long names
LONG_NAME[MEAS_ECCENTRICITY] <- "Excentricite"
LONG_NAME[MEAS_DIAMETER] <- "Diametre"
LONG_NAME[MEAS_RADIUS] <- "Rayon"
LONG_NAME[MEAS_DEGREE] <- "Degre"
LONG_NAME[MEAS_DEGREE_POS] <- "Degre positif"
LONG_NAME[MEAS_DEGREE_NEG] <- "Degre negatif"
LONG_NAME[MEAS_EIGEN] <- "Centralite spectrale"
LONG_NAME[MEAS_BETWEENNESS] <- "Intermediarite"
LONG_NAME[MEAS_CLOSENESS] <- "Proximite"
LONG_NAME[MEAS_CLOSENESS_HARM] <- "Proximite harmonique"
LONG_NAME[MEAS_TRANSITIVITY] <- "Transitivite"
LONG_NAME[MEAS_COMMUNITY_ONLYPOS] <- "Communaute"
LONG_NAME[MEAS_COMMUNITY_NONEG] <- "Communaute"
LONG_NAME[MEAS_MODULARITY_ONLYPOS] <- "Modularite"
LONG_NAME[MEAS_MODULARITY_NONEG] <- "Modularite"
LONG_NAME[MEAS_ARTICULATION] <- "Points d'articulation"
LONG_NAME[MEAS_DISTANCE] <- "Distance"
LONG_NAME[MEAS_DISTANCE_AVG] <- "Distance moyenne"
LONG_NAME[MEAS_CONNECTIVITY] <- "Connectivite"
LONG_NAME[MEAS_CONNECTIVITY_AVG] <- "Connectivite moyenne"
LONG_NAME[MEAS_STR_STRUCT_BAL] <- "Equilibre structurel strict"
LONG_NAME[MEAS_GEN_STRUCT_BAL] <- "Equilibre structurel generalise"
LONG_NAME[MEAS_TRI_SIGNED] <- "Triangles signes"
LONG_NAME[MEAS_SIGN_CENTR] <- "Centralite signee"
LONG_NAME[MEAS_COR_CLUST] <- "Factions antagonistes"




#############################################################################################
# sequence-related names
SEQ_ID <- "Id"
SEQ_CAREER <- "Carriere"
SEQ_CAREER_NA <- "CarriereNA"
SEQ_IDENTIFIER <- "Identifiant"
SEQ_POSTE <- "Poste"
SEQ_SEQ <- "Sequence"
SEQ_COLOR <- "Couleur"
SEQ_MISSING <- "Inconnu"
#SEQ_TYPE <- "Type"
SEQ_SPHERE <- "Sphere"
