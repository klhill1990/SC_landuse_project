library(tidyverse)
library(ggplot2)

#import datasets
trawl_spp <- read.csv("./data/trawl_richness_abundance.csv")
View(trawl_spp)

#use group by and summarize functions to calculate average species richness and abundance per station (separated by station type) per year (with standard deviations) 
trawl_yearly_summary <- trawl_spp %>% 
  group_by(YEAR, STATION_TYPE) %>% 
  summarize(mean(SP_RICH), sd(SP_RICH), mean(ABUNDANCE), sd(ABUNDANCE), n_distinct(STATION_ID))



names(trawl_yearly_summary) <- c("YEAR","STATION_TYPE","SP_RICH_avg", "SP_RICH_sd", "ABUNDANCE_avg", "ABUNDANCE_sd", "NO_STATIONS")
trawl_yearly_summary <- trawl_yearly_summary[c(1,2,7,3,4,5,6)]


yearly_richness_tc <-  qplot(data = subset(trawl_yearly_summary, STATION_TYPE == "TidalCreek"), x = YEAR, y = SP_RICH_avg) +
  geom_errorbar(aes(x= YEAR, ymin=SP_RICH_avg - SP_RICH_sd, ymax= SP_RICH_avg + SP_RICH_sd), width=0.25) +
  geom_smooth(method = "lm", se = F, col = 'black') +
  labs(title = "Tidal Creek Species Richness", y = "Species Richness", x = "Year")

yearly_richness_ow <-  qplot(data = subset(trawl_yearly_summary, STATION_TYPE == "OpenWater"), x = YEAR, y = SP_RICH_avg) +
  geom_errorbar(aes(x= YEAR, ymin=SP_RICH_avg - SP_RICH_sd, ymax= SP_RICH_avg + SP_RICH_sd), width=0.25) +
  geom_smooth(method = "lm", se = F, col = 'black') +
  labs(title = "Open Water Species Richness", y = "Species Richness", x = "Year")

yearly_abund_tc <-  qplot(data = subset(trawl_yearly_summary, STATION_TYPE == "TidalCreek"), x = YEAR, y = ABUNDANCE_avg) +
  geom_errorbar(aes(x= YEAR, ymin=ABUNDANCE_avg - ABUNDANCE_sd, ymax= ABUNDANCE_avg + ABUNDANCE_sd), width=0.25) +
  geom_smooth(method = "lm", se = F, col = 'black') +
  labs(title = "Tidal Creek Abundance", y = "Abundance", x = "Year")

yearly_abund_ow <-  qplot(data = subset(trawl_yearly_summary, STATION_TYPE == "OpenWater"), x = YEAR, y = ABUNDANCE_avg) +
  geom_errorbar(aes(x= YEAR, ymin=ABUNDANCE_avg - ABUNDANCE_sd, ymax= ABUNDANCE_avg + ABUNDANCE_sd), width=0.25) +
  geom_smooth(method = "lm", se = F, col = 'black') +
  labs(title = "Open Water Species Richness", y = "Abundance", x = "Year")

yearly_richness_ow
yearly_richness_tc
yearly_abund_ow
yearly_abund_tc


