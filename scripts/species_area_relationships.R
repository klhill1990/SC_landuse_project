#SPECIES AREA RELATIONSHIPS
#analyze pattern between trawl distances and species richness/abundance (ie, do longer trawls produce more individuals/generate higher species richness)

#load required packages
library(tidyverse)

#STEP 1: DATA WRANGLING

#import dataset
trawl_raw_data <- read.csv("./data/trawl_species_data_raw.csv")

#group raw data by station, summarize data to get richness and abundance
trawl_new <- trawl_raw_data %>% group_by(STATION_ID, STATION_TYPE) %>% summarize(n_distinct(SP_CODE, na.rm = T), sum(ABUNDANCE))

#total trawl distances by removing duplicate records created by rows of species, group by station, and total trawl tow distance
trawl_tow_dist <- trawl_raw_data[,c(3,4,6)] %>% distinct(SAMPLE_ID, .keep_all = T) %>% group_by(STATION_ID) %>% summarize(sum(TOW_DIST_m))

#combine resulting dataframes back into one
trawl_dataset <- full_join(trawl_new, trawl_tow_dist, by = "STATION_ID")

#rename columns
names(trawl_dataset) <- c("STATION_ID", "STATION_TYPE", "SP_RICH", "ABUNDANCE", "TOW_DIST_m")

#remove trawls with 0 sp richness and 0 abundances (as the following plots will be log transformed)
trawl_data_final <- trawl_dataset %>% filter(SP_RICH != 0)

#STEP 2: MODELLING 

#create linear models between species richness/abundance and trawl distances (log-log SAR) for all stations and separated by station type

lm_rich_dist_all   <- lm(log(SP_RICH) ~ log(TOW_DIST_m), data = trawl_data_final)
lm_rich_dist_tidal <- lm(log(SP_RICH) ~ log(TOW_DIST_m), data = subset(trawl_data_final, STATION_TYPE == "TidalCreek"))
lm_rich_dist_open  <- lm(log(SP_RICH) ~ log(TOW_DIST_m), data = subset(trawl_data_final, STATION_TYPE == "OpenWater"))


lm_abun_dist_all   <- lm(log(ABUNDANCE) ~ log(TOW_DIST_m), data = trawl_data_final)
lm_abun_dist_tidal <- lm(log(ABUNDANCE) ~ log(TOW_DIST_m), data = subset(trawl_data_final, STATION_TYPE == "TidalCreek"))
lm_abun_dist_open  <- lm(log(ABUNDANCE) ~ log(TOW_DIST_m), data = subset(trawl_data_final, STATION_TYPE == "OpenWater"))

#plot relationships

plot_rich_dist_all   <- plot(log(SP_RICH) ~ log(TOW_DIST_m), data = trawl_data_final, 
                             xlab = "Trawl Length (meters)", ylab = "Species Richness", main = "Species Area Relationship (All Stations)") + 
                        abline(lm_rich_dist_all)

plot_rich_dist_tidal <- plot(log(SP_RICH) ~ log(TOW_DIST_m), data = subset(trawl_data_final, STATION_TYPE == "TidalCreek"),
                             xlab = "Trawl Length (meters)", ylab = "Species Richness", main = "Species Area Relationship (Tidal Creek)") + 
                        abline(lm_rich_dist_tidal)

plot_rich_dist_open  <- plot(log(SP_RICH) ~ log(TOW_DIST_m), data = subset(trawl_data_final, STATION_TYPE == "OpenWater"),
                             xlab = "Trawl Length (meters)", ylab = "Species Richness", main = "Species Area Relationship (Open Water)") + 
                        abline(lm_rich_dist_open)

plot_abun_dist_all   <- plot(log(ABUNDANCE) ~ log(TOW_DIST_m), data = trawl_data_final,
                             xlab = "Trawl Length (meters)", ylab = "Abundance", main = "Species Area Relationship (All Stations)") + 
                        abline(lm_abun_dist_all)

plot_abun_dist_tidal <- plot(log(ABUNDANCE) ~ log(TOW_DIST_m), data = subset(trawl_data_final, STATION_TYPE == "TidalCreek"),
                             xlab = "Trawl Length (meters)", ylab = "Abundance", main = "Species Area Relationship (Tidal Creek)") + 
                        abline(lm_abun_dist_tidal)

plot_abun_dist_open  <- plot(log(ABUNDANCE) ~ log(TOW_DIST_m), data = subset(trawl_data_final, STATION_TYPE == "OpenWater"),
                             xlab = "Trawl Length (meters)", ylab = "Abundance", main = "Species Area Relationship (Open Water)") + 
                        abline(lm_abun_dist_open)
     