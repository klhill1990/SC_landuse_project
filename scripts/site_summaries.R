#SUMMARIZE SPECIES RICHNESS AND ABUNDANCES
#Summarize trawl data with total abundances, richness, trawl distances, and sites visited

#load required packages
library(tidyverse)
library(gridExtra)

#import datasets
trawldata <- read.csv("./data/trawl_species_data_raw.csv")


#SITE SUMMARIES

##group data by station type and summarize number of stations and total distance towed
trawl_sites <- trawldata %>% group_by(STATION_TYPE) %>% summarize(n_distinct(STATION_ID), sum(TOW_DIST_m))

##number of stations visited
no_stations_open  <- as.integer(trawl_sites[1,2])
no_stations_tidal <- as.integer(trawl_sites[2,2])
no_stations_all   <- no_stations_open + no_stations_tidal

##total distance sampled (converted from m to km)
open_dist_km  <- (as.numeric(trawl_sites[1,3]))/1000
tidal_dist_km <- (as.numeric(trawl_sites[2,3]))/1000
all_dist_km   <- open_dist_km + tidal_dist_km


#ABUNDANCE SUMMARIES

##sum total abundance from all sites
total_abundance <- sum(trawldata$ABUNDANCE)

##sum total abundance by station type
abundance_by_type    <- trawldata %>% group_by(STATION_TYPE) %>% summarize(sum(ABUNDANCE))
tidalcreek_abundance <- as.integer(abundance_by_type[2,2])
openwater_abundance  <- as.integer(abundance_by_type[1,2])


#RICHNESS SUMMARIES

##first remove trawls with 'NA' or no catch
trawldata <- filter(trawldata, SP_CODE != 'NA')

##count overall species richness between all sites
total_richness <- length(unique(trawldata$SP_CODE))

##calculate total richness for each site type
trawl_spcount         <- trawldata %>% group_by(STATION_TYPE) %>% summarize(n_distinct(SP_CODE))
trawl_rich_opentotal  <- as.integer(trawl_spcount[1,2]) 
trawl_rich_tidaltotal <- as.integer(trawl_spcount[2,2])

##count number of species unique to each site type (ie, species found in one but not the other)

###separate data by station type
trawldata_open  <- subset(trawldata, STATION_TYPE == "OpenWater")
trawldata_tidal <- subset(trawldata, STATION_TYPE == "TidalCreek")

###use anti join to recombine datasets based on species code NOT shared between the two

####species found in open water sites only
trawl_anti_open  <- anti_join(trawldata_open, trawldata_tidal, by = "SP_CODE")
####species found in tidal creek sites only
trawl_anti_tidal <- anti_join(trawldata_tidal, trawldata_open, by = "SP_CODE")

###count the number of unique species remaining in each station category
trawl_open_spcount  <- trawl_anti_open %>% group_by(STATION_TYPE) %>% summarize(n_distinct(SP_CODE))
trawl_tidal_spcount <- trawl_anti_tidal %>% group_by(STATION_TYPE) %>% summarize(n_distinct(SP_CODE))
unique_open_spp     <- as.integer(trawl_open_spcount[1,2])
unique_tidal_spp    <- as.integer(trawl_tidal_spcount[1,2])


#ORGANIZE INFORMATION INTO TABLE

##create blank dataframe with column labels
trawl_summary_table <- as.data.frame(matrix(nrow = 3, ncol = 5))
names(trawl_summary_table) <- c("Station Type", "Species Richness", "Abundance", "Number of Stations", "Total Distance Trawled (km)")

##populate table with values from above
trawl_summary_table[,1] <- c("TidalCreek", "OpenWater", "All") 
trawl_summary_table[,2] <- c(paste(trawl_rich_tidaltotal, " ", "(",unique_tidal_spp," unique)", sep = ''), 
                             paste(trawl_rich_opentotal, " ", "(",unique_open_spp," unique)", sep = ''),
                             total_richness)
trawl_summary_table[,3] <- c(tidalcreek_abundance, openwater_abundance, total_abundance)
trawl_summary_table[,4] <- c(no_stations_tidal, no_stations_open, no_stations_all)
trawl_summary_table[,5] <- c(tidal_dist_km, open_dist_km, all_dist_km)

##export table as image
png("./output/summary_table.png", height = 50*nrow(trawl_summary_table), width = 200*ncol(trawl_summary_table))
grid.table(trawl_summary_table)
dev.off()

