#VISUALIZING SPECIES RICHNESS AND ABUNDANCE DATA

#load required packages

library(tidyverse)

library(ggplot2)


#import datasets

trawl_sp <- read.csv("./data/trawl_richness_abundance.csv")


#HISTOGRAMS
##plot histograms to visualize data distributions

rich_openwater_hist <- 
  ggplot(data = subset(trawl_sp, STATION_TYPE == "OpenWater"), aes(SP_RICH)) + 
  geom_histogram(binwidth=2) + 
  labs(title = "Open Water SCECAP Stations", y="Count", x = "Species Richness") +
  theme_minimal()  

rich_tidalcreek_hist <- 
  ggplot(data = subset(trawl_sp, STATION_TYPE == "TidalCreek"), aes(SP_RICH)) + 
  geom_histogram(binwidth=2) + 
  labs(title = "Tidal Creek SCECAP Stations", y="Count", x = "Species Richness") +
  theme_minimal()  

abund_openwater_hist <- 
  ggplot(data = subset(trawl_sp, STATION_TYPE == "OpenWater"), aes(ABUNDANCE)) + 
  geom_histogram(binwidth=30) + 
  labs(title = "Open Water SCECAP Stations", y="Count", x = "Abundance") +
  theme_minimal()  

abund_tidalcreek_hist <- 
  ggplot(data = subset(trawl_sp, STATION_TYPE == "TidalCreek"), aes(ABUNDANCE)) + 
  geom_histogram(binwidth=30) + 
  labs(title = "Tidal Creek SCECAP Stations", y="Count", x = "Abundance") +
  theme_minimal()


#export as PDFs

ggsave("./output/histogram_richness_openwater.pdf", rich_openwater_hist)
ggsave("./output/histogram_richness_tidalcreek.pdf", rich_tidalcreek_hist)
ggsave("./output/histogram_abundance_openwater.pdf", abund_openwater_hist)
ggsave("./output/histogram_abundance_tidalcreek.pdf", abund_tidalcreek_hist)


#BOXPLOTS
##create boxplots to show differences in species richness and abundance between two site types

sp_rich_box <- 
  ggplot(data = trawl_sp, aes(x=STATION_TYPE, y=SP_RICH)) + 
  geom_boxplot() + 
  theme_minimal() + 
  labs(y="Species Richness", x="SCECAP Station Type")

abund_box <- 
  ggplot(data = trawl_sp, aes(x=STATION_TYPE, y=log(ABUNDANCE))) + 
  geom_boxplot() + 
  theme_minimal() + 
  labs(y="Total Abundance (log transformed)", x="SCECAP Station Type")


#export as PDFs

ggsave("./output/boxplot_richness.pdf", sp_rich_box)
ggsave("./output/boxplot_abundance.pdf", abund_box)