#SUMMARIZE SPECIES RICHNESS AND ABUNDANCES
#Summarize trawl data with total abundances, richness, trawl distances, and sites visited

#load required packages
library(tidyverse)
library(gridExtra)
library(ggplot2)

#import datasets
trawldata <- read.csv("./data/trawl_species_data_raw.csv")
trawlsp <- read.csv("./data/trawl_richness_abundance.csv")

#SITE SUMMARIES ----

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


#ORGANIZE INFORMATION INTO TABLE ----

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

#CREATE HISTOGRAM TO SHOW TRAWL LENGTH DATA ----

#group raw data by station, summarize data to get richness and abundance
trawl_new <-  trawl_raw_data %>% 
  group_by(STATION_ID, STATION_TYPE) %>% 
  summarize(n_distinct(SP_CODE, na.rm = T), sum(ABUNDANCE))

#total trawl distances by removing duplicate records created by rows of species, group by station, and total trawl tow distance
trawl_tow_dist <- trawl_raw_data[,c(3,4,6)] %>% 
  distinct(SAMPLE_ID, .keep_all = T) %>% 
  group_by(STATION_ID) %>% 
  summarize(sum(TOW_DIST_m))

#combine resulting dataframes back into one
trawl_dataset <- full_join(trawl_new, trawl_tow_dist, by = "STATION_ID")

#rename columns
names(trawl_dataset) <- c("STATION_ID", "STATION_TYPE", "SP_RICH", "ABUNDANCE", "TOW_DIST_m")

#plot histogram of data and export as pdf
tow_dist_histogram <- ggplot(trawl_dataset, aes(x=TOW_DIST_m, fill = STATION_TYPE)) + 
                      geom_histogram(binwidth = 40) + 
                      theme(legend.position = "bottom")

ggsave("./output/histogram_towdistances.pdf", tow_dist_histogram)

#CREATE BOXPLOTS TO SHOW DIFFERENCES IN STATION TYPE ----

##species richness
sp_rich_box <- 
  ggplot(data = trawlsp, aes(x=STATION_TYPE, y=SP_RICH)) + 
  geom_boxplot() + 
  theme_minimal() + 
  labs(y="Species Richness", x="SCECAP Station Type")

##total abundance
abund_box <- 
  ggplot(data = trawlsp, aes(x=STATION_TYPE, y=log(ABUNDANCE))) + 
  geom_boxplot() + 
  theme_minimal() + 
  labs(y="Total Abundance (log transformed)", x="SCECAP Station Type")


##export as PDFs
ggsave("./output/boxplot_richness.pdf", sp_rich_box)
ggsave("./output/boxplot_abundance.pdf", abund_box)

#run T-test to see if difference between two site types are significant
sprich_ttest <- t.test((subset(trawlsp, STATION_TYPE == "TidalCreek"))$SP_RICH, (subset(trawlsp, STATION_TYPE == "OpenWater"))$SP_RICH)
abund_ttest  <- t.test((subset(trawlsp, STATION_TYPE == "TidalCreek"))$ABUNDANCE, (subset(trawlsp, STATION_TYPE == "OpenWater"))$ABUNDANCE)

sprich_ttest$p.value
abund_ttest$p.value
