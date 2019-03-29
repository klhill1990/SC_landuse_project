#SPECIES RICHNESS AND ABUNDANCE

#load required packages
library(tidyverse)
library(stringr)
library(ggplot2)


##STEP 1: DATA WRANGLING
###with trawl species data and sample site data, standardize trawl distances 
###per site type (only include samples which have the target trawl distance 
###of 250 or 500m depending on site type)



#import dataset of stations, sample IDs, and tow lengths

trawl_samples <- read.csv("./data/species_sample_ref.csv")
trawl_species <- read.csv("./data/trawl_species_data.csv")

#add category for station type by extracting first two characters 
#from station ID (RO = open water, RT = tidal creek)

trawl_samples$SITE_TYPE <- substr(trawl_samples$STATION_ID, 1,2)

#use filter function to select only open water samples with tow
#distsances of 500m and tidal creek sites with tow distances of 250m

trawl_samples_tidalcreek <- filter(trawl_samples, SITE_TYPE == "RT", TOW_DIST_m == 250)
trawl_samples_openwater <- filter(trawl_samples, SITE_TYPE == "RO", TOW_DIST_m == 500)

#combine these 2 datasets back into one
trawl_samples_complete <- bind_rows(trawl_samples_openwater, trawl_samples_tidalcreek)

#join these data with trawl species data
trawl_data <- inner_join(trawl_samples_complete, trawl_species, by = "SAMPLE_ID")

#remove rows which contain SP_CODE "X999" as this signifies no species caught
trawl_data <- filter(trawl_data, SP_CODE != "X999")

##STEP 2: CALCUALTING SPECIES RICHNESS AND ABUNDANCE
###calculate species richness (number of unique species per trawl) 
###and total abundance (total number of individuals per trawl)

#use group_by and summarize function to count the number of 
#unique species per trawl (species richness)
trawl_species_rich <- trawl_data %>% 
  group_by(STATION_ID.x, SAMPLE_ID, TOW_DIST_m.x, STATION_TYPE, YEAR) %>% 
  summarize(n_distinct(SP_CODE))

#rename columns
names(trawl_species_rich) <- c("STATION_ID", "SAMPLE_ID", "TOW_DIST_m", "STATION_TYPE", "YEAR", "SP_RICH")

#use group_by and summarize functions to total the numbers of 
#individuals caught across species (total abundance)
trawl_species_abund <- trawl_data %>% 
  group_by(STATION_ID.x, SAMPLE_ID, TOW_DIST_m.x, STATION_TYPE, YEAR) %>% 
  summarize(sum(ABUNDANCE))

#rename columns
names(trawl_species_abund) <- c("STATION_ID", "SAMPLE_ID", "TOW_DIST_m", "STATION_TYPE", "YEAR", "TOTAL_ABUNDANCE")

#join together trawl species and trawl abundances, by sample ID
trawl_species_final <- full_join(trawl_species_rich, trawl_species_abund, by = "SAMPLE_ID")

#remove duplicate columns from join and rename columns
trawl_species_final <- trawl_species_final[,c(1,2,3,4,5,6,11)]
names(trawl_species_final) <- c("STATION_ID", "SAMPLE_ID", "TOW_DIST_m", "STATION_TYPE", "YEAR", "SP_RICH", "TOTAL_ABUNDANCE")

##STEP 3: CREATE GRAPHS AND LOOK FOR TRENDS OVER TIME

#use group by and summarize functions to calculate average richness 
#and average abundance per sample by year and station type
trawl_year_avgs <- trawl_species_final %>% 
  group_by(STATION_TYPE, YEAR) %>% 
  summarize_at(c("SP_RICH", "TOTAL_ABUNDANCE"),mean) 

#plot species richness by year

richness_plot <- 
  ggplot(data = trawl_year_avgs, aes(x = YEAR, y = SP_RICH)) +
  geom_point(aes(col = STATION_TYPE)) +
  stat_smooth(method="lm") +
  labs(subtitle = "Average SCECAP Station Species Richness from 1999-2018", x = "Year", y = "Species Richness") +
  theme_minimal()
  
abundance_plot <- 
  ggplot(data = trawl_year_avgs, aes(x = YEAR, y = TOTAL_ABUNDANCE)) + 
  geom_point(aes(col = STATION_TYPE)) + 
  stat_smooth(method = "lm") +
  labs(subtitle = "Average SCECAP Station Abundance from 1999-2018", x = "Year", y = "Abundance") +
  theme_minimal()

#export plots as PDFs

ggsave("./output/richness_byYear.pdf", richness_plot)
ggsave("./output/abundance_byYear.pdf", abundance_plot)

##STEP 4: LINEAR MODELING

tidal_abundance_lm <- lm(TOTAL_ABUNDANCE ~ YEAR, data = filter(trawl_year_avgs, STATION_TYPE == "TidalCreek")) 
open_abundance_lm  <- lm(TOTAL_ABUNDANCE ~ YEAR, data = filter(trawl_year_avgs, STATION_TYPE == "OpenWater"))
tidal_richness_lm <- lm(SP_RICH ~ YEAR, data = filter(trawl_year_avgs, STATION_TYPE == "TidalCreek")) 
open_richness_lm  <- lm(SP_RICH ~ YEAR, data = filter(trawl_year_avgs, STATION_TYPE == "OpenWater"))

summary(tidal_abundance_lm)
summary(open_abundance_lm)
summary(tidal_richness_lm)
summary(open_richness_lm)


