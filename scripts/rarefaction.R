#RAREFACTION

#load required packages
library(tidyverse)
library(vegan)
library(fossil)

#STEP 1: DATA WRANGLING

#import dataset
trawl_raw_data <- read.csv("./data/trawl_species_data_raw.csv")

#convert into community matrix using create.matrix function (from 'fossil' package)
trawl_new_matrix <- create.matrix(trawl_raw_data, tax.name = "SP_CODE", locality = "STATION_ID", abund = T, abund.col = "ABUNDANCE")

#transpose rows and columns so sites are rows and species are columns 
trawl_comm_matrix <- t(trawl_new_matrix)

#STEP 2: RAREFACTION

#use quantile and mean functions to get an idea of abundances per station in data
quantile(rowSums(trawl_comm_matrix))
mean(rowSums(trawl_comm_matrix))

#I chose to rarefy species richness at 15 (close to lower 25% quantile), 40 (close to median), and 95 (close to mean) individuals.
rarefied_15 <- as.data.frame(rarefy(trawl_comm_matrix, 15))
rarefied_40 <- as.data.frame(rarefy(trawl_comm_matrix, 40))
rarefied_95 <- as.data.frame(rarefy(trawl_comm_matrix, 95))

#assemble resulting values into new dataframe
trawl_rarefied <- cbind(rarefied_15, rarefied_40, rarefied_95)
trawl_rarefied$STATION_ID <- rownames(trawl_comm_matrix)
names(trawl_rarefied) <- c("n15", "n40", "n95", "STATION_ID")
trawl_rarefied <- trawl_rarefied[c(4,1,2,3)]
row.names(trawl_rarefied) <- NULL
trawl_rarefied
