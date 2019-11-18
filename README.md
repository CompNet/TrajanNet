TrajanNet
=======
*Extraction and analysis of a [Trajan](https://en.wikipedia.org/wiki/Trajan)-related social network*

* Copyright 2019 Vincent Labatut 

TrajanNet is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation. For source availability and license information see `licence.txt`

* Lab site: http://lia.univ-avignon.fr/
* GitHub repo: https://github.com/CompNet/NetVotes
* Contact: Vincent Labatut <vincent.labatut@univ-avignon.fr>

Currently in development

-----------------------------------------------------------------------

# Description
This set of R scripts aims at analyzing a historical dataset describing the relationships between the Roman emperor Trajan and his entourage. It does the following:

1. Extract various networks based on some tabular data containing individual and relational attributes.
2. Compute a number of statistics and generate the corresponding plots, for both the tables and graphs.
3. Perform a sequence analysis of certain chronological attributes.


# Data
The raw dataset was manually elaborated by Gaetane Vallet during her Master's thesis. See her thesis (in French) for more information [V'18,V'20].  


# Organization
Here are the folders composing the project:
* Folder `data`: contains the data used by the R scripts, as well as produced by them.
  * Folder `tables`: input data presented as three CSV tables
    * `trajan_attributes.csv`: individual description of each historical character in the dataset.  
    * `trajan_positions.csv`: list of professional positions a character can hold.  
    * `trajan_relations.csv`: connections between the characters.  
  * Folder `nets`: networks procudes by the scripts, and the associated plots and tables.
    * Folder `all`: network containing all the types of links at once (multiplex network).
    * Folder `family`: network with only the family ties (uniplex network).
    * Folder `personal`: network with only the personal ties (friendship/enmity) (uniplex network).
    * Folder `pro`: network with only the professional ties (uniplex network).
    * Folder `unknown`: network with relationships whose exact nature is unknown (uniplex network). 
    * Folder `signed`: signed network with no distinction between relationship types (collapsed multiplex network).
  * Folder `attributes`: results obtained for the individual attributes.


# Installation
1. Install the [`R` language](https://www.r-project.org/)
2. Download this project from GitHub and unzip.
3. Install the required packages: 
   1. Open the `R` console.
   2. Set the current directory as the working directory, using `setwd("<my directory>")`.
   3. Run the install script `src/install_packages.R`.


# Use
In order to extract the networks from the raw data, compute the statistics, and generate the plots:

1. Open the `R` console.
2. Set the current directory as the working directory, using `setwd("<my directory>")`.
3. Run the main script `src/main.R`.

The script will produce a number of files in the subfolders of folder `nets`. They are grouped in subsubfolders, each one corresponding to a specific topological measure (degree, closeness, etc.). 

The `verification.R` was used to check the consistency of the raw data. The rest of the scripts are just secondary functions called by `main.R`.


# Dependencies
* [`igraph`](http://igraph.org/r/) package: build and handle graphs.
* [`signnet`](https://github.com/schochastics/signnet): analysis of signed graphs.
* [`graphlayouts`](https://cran.rstudio.com/web/packages/graphlayouts): plot graphs.
* [`ggraph`](https://cran.rstudio.com/web/packages/ggraph): plot graphs.
* [`TraMineR`](http://traminer.unige.ch/): sequence analysis.
* [`SDMTools`](https://cran.rstudio.com/web/packages/SDMTools): misc.


# To-do List
* Change friend to personnal (links)
* Signed nets: consider the evolution of the relationships
* Perform sequence analysis
  * Cluster analysis based on the sequences, then assortativity of the sequence classes? 
* Compute association measures between attributes
* Multiplex plot of the different types of links (Didn't find an appropriate tool)


# References
* **[V'20]** Vallet, G. *Title to be determined*, G. Second part of the Master's thesis, Avignon University, Human and Social Sciences Faculty, History Department, Avignon, France.
* **[V'18]** Vallet, G. *L'entourage de Trajan : étude prosopographique de l'entourage du prince de 98-117 apr. J.-C.*, First part of the Master's thesis, Avignon University, Human and Social Sciences Faculty, History Department, Avignon, France.
