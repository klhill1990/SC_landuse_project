#load required packages
library(tidyverse)
library(vegan)

#import dataset
trawl_raw_data <- read.csv("./data/trawl_species_data_raw.csv")

#group raw data by station, summarize data to get richness and abundance
trawl_new <- trawl_raw_data %>% group_by(STATION_ID) %>% summarize(n_distinct(SP_CODE, na.rm = T), sum(ABUNDANCE))

#total trawl distances by removing duplicate records created by rows of species, group by station, and total trawl tow distance
trawl_tow_dist <- trawl_raw_data[,c(3,4,6)] %>% distinct(SAMPLE_ID, .keep_all = T) %>% group_by(STATION_ID) %>% summarize(sum(TOW_DIST_m))

#combine resulting dataframes back into one

trawl_dataset <- full_join(trawl_new, trawl_tow_dist, by = "STATION_ID")

#rename columns
names(trawl_dataset) <- c("STATION_ID", "SP_RICH", "ABUNDANCE", "TRAWL_LENGTH_m")
View(trawl_dataset)

plot(log(SP_RICH + 1) ~ log(TRAWL_LENGTH_m), data = trawl_dataset)
plot(SP_RICH ~ log(ABUNDANCE), data = trawl_dataset)
