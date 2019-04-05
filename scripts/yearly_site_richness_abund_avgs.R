#ANNUAL MEAN SPECIES RICHNESS AND ABUNDANCE
##Calculate average station species richness and abundance by year
##Plot these data over SCECAP's monitoring period (1999 to 2018) and evaluate trends over time


#load required packages
library(tidyverse)
library(ggplot2)

#import datasets
trawl_spp <- read.csv("./data/trawl_richness_abundance.csv")

#use group by and summarize functions to calculate average species richness and abundance per station (separated by station type) per year (with standard deviations) 

trawl_yearly_summary <- trawl_spp %>% 
  group_by(YEAR, STATION_TYPE) %>% 
  summarize(mean(SP_RICH), sd(SP_RICH), mean(ABUNDANCE), sd(ABUNDANCE), n_distinct(STATION_ID))


#rename and reorder columns

names(trawl_yearly_summary) <- c("YEAR","STATION_TYPE","SP_RICH_avg", "SP_RICH_sd", "ABUNDANCE_avg", "ABUNDANCE_sd", "NO_STATIONS")
trawl_yearly_summary <- trawl_yearly_summary[c(1,2,7,3,4,5,6)]


#plot mean annual species richness and abundance for different site types

avg_station_richness_tc  <- qplot(data = subset(trawl_yearly_summary, STATION_TYPE == "TidalCreek"), x = YEAR, y = SP_RICH_avg) +
                            geom_errorbar(aes(x= YEAR, ymin=SP_RICH_avg - SP_RICH_sd, ymax= SP_RICH_avg + SP_RICH_sd), width=0.25) +
                            geom_smooth(method = "lm", se = T, col = 'black') +
                            labs(title = "Annual Mean Species Richness", subtitle = "Tidal Creek SCECAP Stations (1999-2018)", y = "Species Richness per Station", x = "Year")


avg_station_richness_ow  <- qplot(data = subset(trawl_yearly_summary, STATION_TYPE == "OpenWater"), x = YEAR, y = SP_RICH_avg) +
                            geom_errorbar(aes(x= YEAR, ymin=SP_RICH_avg - SP_RICH_sd, ymax= SP_RICH_avg + SP_RICH_sd), width=0.25) +
                            geom_smooth(method = "lm", se = T, col = 'black') +
                            labs(title = "Annual Mean Species Richness", subtitle = "Open Water SCECAP Stations (1999-2018)", y = "Species Richness per Station", x = "Year")


avg_station_abundance_tc <- qplot(data = subset(trawl_yearly_summary, STATION_TYPE == "TidalCreek"), x = YEAR, y = ABUNDANCE_avg) +
                            geom_errorbar(aes(x= YEAR, ymin=ABUNDANCE_avg - ABUNDANCE_sd, ymax= ABUNDANCE_avg + ABUNDANCE_sd), width=0.25) +
                            geom_smooth(method = "lm", se = T, col = 'black') +
                            labs(title = "Annual Mean Abundance", subtitle = "Tidal Creek SCECAP Stations (1999-2018)", y = "Abundance per Station", x = "Year")


avg_station_abundance_ow <- qplot(data = subset(trawl_yearly_summary, STATION_TYPE == "OpenWater"), x = YEAR, y = ABUNDANCE_avg) +
                            geom_errorbar(aes(x= YEAR, ymin=ABUNDANCE_avg - ABUNDANCE_sd, ymax= ABUNDANCE_avg + ABUNDANCE_sd), width=0.25) +
                            geom_smooth(method = "lm", se = T, col = 'black') +
                            labs(title = "Annual Mean Abundance", subtitle = "Open Water SCECAP Stations (1999-2018)", y = "Abundance per Station", x = "Year")


#export plots as PDFs

ggsave("./output/avg_station_abundance_ow.pdf", avg_station_abundance_ow)
ggsave("./output/avg_station_abundance_tc.pdf", avg_station_abundance_tc)
ggsave("./output/avg_station_richness_ow.pdf", avg_station_richness_ow)
ggsave("./output/avg_station_richness_tc.pdf", avg_station_richness_tc)

#run lm's to evaulate trends over time

abund_tc_lm <- lm(ABUNDANCE_avg ~ YEAR, data = subset(trawl_yearly_summary, STATION_TYPE == "TidalCreek"))
abund_ow_lm <- lm(ABUNDANCE_avg ~ YEAR, data = subset(trawl_yearly_summary, STATION_TYPE == "OpenWater"))
srich_tc_lm <- lm(SP_RICH_avg ~ YEAR, data = subset(trawl_yearly_summary, STATION_TYPE == "TidalCreek"))
srich_ow_lm <- lm(SP_RICH_avg ~ YEAR, data = subset(trawl_yearly_summary, STATION_TYPE == "OpenWater"))

summary(abund_tc_lm)
summary(abund_ow_lm)
summary(srich_tc_lm)
summary(srich_ow_lm)
