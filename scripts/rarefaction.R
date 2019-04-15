#RAREFACTION (in progress)
##use 'rarefaction' to compare species richness across stations with different trawl lengths

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

#I removed stations with total abundances lower than target rarefaction value
trawl_comm_matrix05 <- subset(trawl_comm_matrix, (rowSums(trawl_comm_matrix) > 4))
trawl_comm_matrix10 <- subset(trawl_comm_matrix, (rowSums(trawl_comm_matrix) > 9))
trawl_comm_matrix15 <- subset(trawl_comm_matrix, (rowSums(trawl_comm_matrix) > 15))
trawl_comm_matrix40 <- subset(trawl_comm_matrix, (rowSums(trawl_comm_matrix) > 40))
trawl_comm_matrix95 <- subset(trawl_comm_matrix, (rowSums(trawl_comm_matrix) > 95))

#see how many stations are left that match criteria
nrow(trawl_comm_matrix)
nrow(trawl_comm_matrix05)
nrow(trawl_comm_matrix10)
nrow(trawl_comm_matrix15)
nrow(trawl_comm_matrix40)
nrow(trawl_comm_matrix95)

#I chose to rarefy species richness at 15 (close to lower 25% quantile), 40 (close to median), and 95 (close to mean) individuals.
rarefied_05 <- as.data.frame(rarefy(trawl_comm_matrix05, 5))
rarefied_10 <- as.data.frame(rarefy(trawl_comm_matrix10, 10))
rarefied_15 <- as.data.frame(rarefy(trawl_comm_matrix15, 15))
rarefied_40 <- as.data.frame(rarefy(trawl_comm_matrix40, 40))
rarefied_95 <- as.data.frame(rarefy(trawl_comm_matrix95, 95))

rarefied_05$STATION_ID <- rownames(trawl_comm_matrix05)
rarefied_10$STATION_ID <- rownames(trawl_comm_matrix10)
rarefied_15$STATION_ID <- rownames(trawl_comm_matrix15)
rarefied_40$STATION_ID <- rownames(trawl_comm_matrix40)
rarefied_95$STATION_ID <- rownames(trawl_comm_matrix95)


#assemble resulting values into new dataframe

trawl_rarefied <- full_join(rarefied_05, rarefied_10, by = "STATION_ID") %>% 
                  full_join(rarefied_15, by = "STATION_ID") %>% 
                  full_join(rarefied_40, by = "STATION_ID") %>%
                  full_join(rarefied_95, by = "STATION_ID")

names(trawl_rarefied) <- c("n05", "STATION_ID", "n10", "n15", "n40", "n95")
trawl_rarefied <- trawl_rarefied[c(2,1,3,4,5,6)]

#combine these data with SCECAP station info to get year and station type

scecap <- read.csv("./data/landcover/SCECAP_data.csv")

trawl_rarefied_join <- left_join(trawl_rarefied, scecap, by = "STATION_ID")

trawl_rarefied_final <- trawl_rarefied_join[c(1,10,11,2,3,4,5,6)]

#PLOT RESULTS
rare05_lm <- lm(n05 ~ YEAR, data = trawl_rarefied_final)
rare10_lm <- lm(n10 ~ YEAR, data = trawl_rarefied_final)
rare15_lm <- lm(n15 ~ YEAR, data = trawl_rarefied_final)
rare40_lm <- lm(n40 ~ YEAR, data = trawl_rarefied_final)
rare95_lm <- lm(n95 ~ YEAR, data = trawl_rarefied_final)

plot(n05 ~ YEAR, data = trawl_rarefied_final) + abline(rare05_lm)
plot(n10 ~ YEAR, data = trawl_rarefied_final) + abline(rare10_lm)
plot(n15 ~ YEAR, data = trawl_rarefied_final) + abline(rare15_lm)
plot(n40 ~ YEAR, data = trawl_rarefied_final) + abline(rare40_lm)
plot(n95 ~ YEAR, data = trawl_rarefied_final) + abline(rare95_lm)

rarecurve(trawl_comm_matrix95, 95)


###GAMMA-SCALE YEARLY RICHNESS USING RAREFACTION
#will first need to boostrap number of stations in a given year (pre-2006 had 60 not 30 stations/yr)

#combine SCECAP year data with community matrix data

trawl_comm_matrix_df <- as.data.frame(trawl_comm_matrix)
trawl_comm_matrix_df$STATION_ID <- rownames(trawl_comm_matrix_df)

trawl_matrix_join_df <- full_join(trawl_comm_matrix_df, scecap, by = "STATION_ID")
trawl_matrix_df <- trawl_matrix_join_df[c(133,137,138, 1:132)]
trawl_matrix_df

##
station_guide <- unique(trawl_raw_data[c(1,2,3)])

#split into two dataframes based on station type
open_station_guide <- subset(station_guide, STATION_TYPE == "OpenWater")
tidal_station_guide <- subset(station_guide, STATION_TYPE == "TidalCreek")


output_openmatrix <- data.frame(STATION_ID = 1:300, YEAR = c(1999:2018))
output_openmatrix$YEAR <- sort(output_openmatrix$YEAR)
output_openmatrix
sub_stations <- NULL
for(y in 1999:2018) 
  {
  sub_stations[y,1] <- as.character(sample((subset(open_station_guide, YEAR == y)$STATION_ID), size = 15, replace = TRUE))
  }
output_openmatrix$STATION_ID <- sub_stations

output_openmatrix   

sub_stations <- as.character(sample((subset(open_station_guide, YEAR == 1999)$STATION_ID), size = 15, replace = TRUE))
sub_stations
    
    
trawl_comm_matrix_df    
final_out <- NULL
for (y in 1999:2018)
{

  sub_station <- as.character(sample((subset(open_station_guide, YEAR == y)$STATION_ID), size = 15, replace = TRUE))
  station_df <- data.frame(STATION_ID = c(sub_station))
  station_join <- inner_join(trawl_comm_matrix_df, station_df, by = "STATION_ID")
  
  sum_cols <- colSums(station_join[,1:132])    
  year_totals <- data.frame(YEAR = y, ABUNDANCE = sum_cols)    
  year_totals$SP_CODE <- row.names(year_totals) 
  final_out[[y]] <- year_totals

}


final_out[1999:2018]
new_matrix <- create.matrix(my_years, tax.name = "SP_CODE", locality = "YEAR", abund = T, abund.col = "ABUNDANCE")
new_matrix_t <- t(new_matrix)