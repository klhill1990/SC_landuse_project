#SPECIES RICHNESS AND ABUNDANCE

#load required packages

library(tidyverse)
library(stringr)
library(ggplot2)


#Part 1: DATA WRANGLING
##with trawl species data and sample site data, standardize trawl distances per site type (only include samples which have the target trawl distance of 250 or 500m depending on site type)

#import dataset of stations, sample IDs, and tow lengths

trawl_samples <- read.csv("./data/species_sample_ref.csv")
trawl_species <- read.csv("./data/trawl_species_data.csv")

#add category for station type by extracting first two characters from station ID (RO = open water, RT = tidal creek)

trawl_samples$SITE_TYPE <- substr(trawl_samples$STATION_ID, 1,2)

#use filter function to select only open water samples with tow distances of 500m and tidal creek sites with tow distances of 250m

trawl_samples_tidalcreek <- filter(trawl_samples, SITE_TYPE == "RT", TOW_DIST_m == 250)
trawl_samples_openwater  <- filter(trawl_samples, SITE_TYPE == "RO", TOW_DIST_m == 500)

#combine these 2 datasets back into one

trawl_samples_complete <- bind_rows(trawl_samples_openwater, trawl_samples_tidalcreek)

#join these data with trawl species data

trawl_data <- inner_join(trawl_samples_complete, trawl_species, by = "SAMPLE_ID")

#some trawls returned no species (sp code=X999), these are useful data points but will throw off species richness calculations. 

##subset 'no catch' data and reattach later

trawl_data_no_catch <- filter(trawl_data, SP_CODE == "X999")

##remove trawls which returned no species

trawl_data <- filter(trawl_data, SP_CODE != "X999")


#Part 2: CALCUALTING SPECIES RICHNESS AND ABUNDANCE
##calculate species richness (number of unique species per trawl) and total abundance (total number of individuals per trawl)

#use group_by and summarize function to count the number of unique species per trawl (species richness)

trawl_species_rich <- trawl_data %>% 
  group_by(STATION_ID.x, SAMPLE_ID, TOW_DIST_m.x, STATION_TYPE, YEAR) %>% 
  summarize(n_distinct(SP_CODE))

#rename columns

names(trawl_species_rich) <- c("STATION_ID", "SAMPLE_ID", "TOW_DIST_m", "STATION_TYPE", "YEAR", "SP_RICH")

#use group_by and summarize functions to total the numbers of individuals caught across species (total abundance)

trawl_species_abund <- trawl_data %>% 
  group_by(STATION_ID.x, SAMPLE_ID, TOW_DIST_m.x, STATION_TYPE, YEAR) %>% 
  summarize(sum(ABUNDANCE))

#rename columns

names(trawl_species_abund) <- c("STATION_ID", "SAMPLE_ID", "TOW_DIST_m", "STATION_TYPE", "YEAR", "TOTAL_ABUNDANCE")

#join together trawl species and trawl abundances by sample ID

trawl_species_final <- full_join(trawl_species_rich, trawl_species_abund, by = "SAMPLE_ID")

#remove duplicate columns from join and rename columns

trawl_species_final        <- trawl_species_final[,c(1,2,3,4,5,6,11)]
names(trawl_species_final) <- c("STATION_ID", "SAMPLE_ID", "TOW_DIST_m", "STATION_TYPE", "YEAR", "SP_RICH", "TOTAL_ABUNDANCE")

#reattach stations which returned no species (species richnes and abundance = 0)

##add species richness colum populated with 0's

trawl_data_no_catch$SP_RICH <- 0

##select rows which match final dataset, rename and reorder accordingly

trawl_data_no_catch        <- trawl_data_no_catch[,c(1,2,3,6,7,13,14)]
names(trawl_data_no_catch) <- c("STATION_ID", "SAMPLE_ID", "TOW_DIST_m", "YEAR", "STATION_TYPE", "TOTAL_ABUNDANCE", "SP_RICH")
trawl_data_no_catch        <- trawl_data_no_catch[c(1,2,3,5,4,7,6)]

##bind rows from no catch data to final data

trawl_species_final <- bind_rows(trawl_species_final, trawl_data_no_catch)


#Part 3: VISUALIZE DATA

#3.1. Histograms

#make histograms to show distribution of species richness and abundances from trawls at both station types

sp_rich_openwater_hist <- 
  ggplot(data = subset(trawl_species_final, STATION_TYPE == "OpenWater"), aes(SP_RICH)) + 
  geom_histogram(binwidth=1) + 
  labs(title = "Open Water SCECAP Stations", y="Count", x = "Species Richness") +
  theme_minimal()  
  
sp_rich_tidalcreek_hist <- 
  ggplot(data = subset(trawl_species_final, STATION_TYPE == "TidalCreek"), aes(SP_RICH)) + 
  geom_histogram(binwidth=1) + 
  labs(title = "Tidal Creek SCECAP Stations", y="Count", x = "Species Richness") +
  theme_minimal()  

abund_openwater_hist <- 
  ggplot(data = subset(trawl_species_final, STATION_TYPE == "OpenWater"), aes(TOTAL_ABUNDANCE)) + 
  geom_histogram(binwidth=25) + 
  labs(title = "Open Water SCECAP Stations", y="Count", x = "Total Abundance") +
  theme_minimal()  

abund_tidalcreek_hist <- 
  ggplot(data = subset(trawl_species_final, STATION_TYPE == "TidalCreek"), aes(TOTAL_ABUNDANCE)) + 
  geom_histogram(binwidth=25) + 
  labs(title = "Tidal Creek SCECAP Stations", y="Count", x = "Total Abundance") +
  theme_minimal()

#export as PDFs

ggsave("./output/histogram_richness_openwater.pdf", sp_rich_openwater_hist)
ggsave("./output/histogram_richness_tidalcreek.pdf", sp_rich_tidalcreek_hist)
ggsave("./output/histogram_abundance_openwater.pdf", abund_openwater_hist)
ggsave("./output/histogram_abundance_tidalcreek.pdf", abund_tidalcreek_hist)

#3.2. Scatterplots

#use group by and summarize functions to calculate average richness and average abundance per sample by year and station type

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

#plot abundance by year

abundance_plot <- 
  ggplot(data = trawl_year_avgs, aes(x = YEAR, y = TOTAL_ABUNDANCE)) + 
  geom_point(aes(col = STATION_TYPE)) + 
  stat_smooth(method = "lm") +
  labs(subtitle = "Average SCECAP Station Abundance from 1999-2018", x = "Year", y = "Abundance") +
  theme_minimal()

#export plots as PDFs

ggsave("./output/richness_years.pdf", richness_plot)
ggsave("./output/abundance_years.pdf", abundance_plot)

#3.3. Boxplots

#create boxplots to show differences in species richness and abundance between two site types

sp_rich_box <- 
  ggplot(data = trawl_species_final, aes(x=STATION_TYPE, y=SP_RICH)) + 
  geom_boxplot() + 
  theme_minimal() + 
  labs(y="Species Richness", x="SCECAP Station Type")

abund_box <- 
  ggplot(data = trawl_species_final, aes(x=STATION_TYPE, y=TOTAL_ABUNDANCE)) + 
  geom_boxplot() + 
  theme_minimal() + 
  labs(y="Total Abundance", x="SCECAP Station Type")

log_abund_box <- 
  ggplot(data = trawl_species_final, aes(x=STATION_TYPE, y=log(TOTAL_ABUNDANCE))) + 
  geom_boxplot() + 
  theme_minimal() + 
  labs(y="Total Abundance", x="SCECAP Station Type")

#export as PDFs

ggsave("./output/boxplot_richness.pdf", sp_rich_box)
ggsave("./output/boxplot_abundance.pdf", abund_box)
ggsave("./output/boxplot_log_abundance.pdf", log_abund_box)

##STEP 4: LINEAR MODELING

tidal_abundance_lm <- lm(TOTAL_ABUNDANCE ~ YEAR, data = filter(trawl_year_avgs, STATION_TYPE == "TidalCreek")) 
open_abundance_lm  <- lm(TOTAL_ABUNDANCE ~ YEAR, data = filter(trawl_year_avgs, STATION_TYPE == "OpenWater"))
tidal_richness_lm  <- lm(SP_RICH ~ YEAR, data = filter(trawl_year_avgs, STATION_TYPE == "TidalCreek")) 
open_richness_lm   <- lm(SP_RICH ~ YEAR, data = filter(trawl_year_avgs, STATION_TYPE == "OpenWater"))

summary(tidal_abundance_lm)
summary(open_abundance_lm)
summary(tidal_richness_lm)
summary(open_richness_lm)


