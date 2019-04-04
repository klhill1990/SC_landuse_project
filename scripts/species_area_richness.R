#Species Area Relationships

#load required packages

library(tidyverse)


#import datasets

trawl <- read.csv("./data/trawl_species_data_raw.csv")


#group by samples and count unique species 

trawl_rich <- trawl %>% group_by(YEAR, STATION_TYPE, STATION_ID, TOW_DIST_m, SAMPLE_ID) %>% summarize(n_distinct(SP_CODE, na.rm = T))

trawl_abun <- trawl %>% group_by(YEAR, STATION_TYPE, STATION_ID, TOW_DIST_m, SAMPLE_ID) %>% summarize(sum(ABUNDANCE))


#join richness and abundance data by sample ID

trawl_join <- inner_join(trawl_rich, trawl_abun, by = "SAMPLE_ID")


#select, rename and reorder columns

trawl_join <- trawl_join[,c(1,2,3,4,5,6,11)]

names(trawl_join) <- c("YEAR", "STATION_TYPE", "STATION_ID", "TRAWL_DIST_m", "SAMPLE_ID", "SP_RICH", "ABUNDANCE")

trawl_final <- trawl_join[c(5,3,2,1,4,6,7)]


#create linear models of log tranformed distance vs. log transformed richness/abundance and plot

##Distance vs. Richness 

rich_lm  <- lm(log(SP_RICH + 1) ~ log(TRAWL_DIST_m), data = trawl_final)

plot(log(SP_RICH + 1) ~ log(TRAWL_DIST_m), data = trawl_final) + abline(rich_lm)


##Distance vs. Abundance

abun_lm <- lm(log(ABUNDANCE + 1) ~ log(TRAWL_DIST_m), data = trawl_final)

plot(log(ABUNDANCE + 1) ~ log(TRAWL_DIST_m), data = trawl_final) + abline(abun_lm)


#separate by site types (open water)

##Distance vs. Richness

rich_ow_lm  <- lm(log(SP_RICH + 1) ~ log(TRAWL_DIST_m), data = subset(trawl_final, STATION_TYPE == "OpenWater"))

plot(log(SP_RICH + 1) ~ log(TRAWL_DIST_m), data = subset(trawl_final, STATION_TYPE == "OpenWater")) + abline(rich_ow_lm)


##Distance vs. Abundance 

abun_ow_lm  <- lm(log(ABUNDANCE + 1) ~ log(TRAWL_DIST_m), data = subset(trawl_final, STATION_TYPE == "OpenWater"))

plot(log(ABUNDANCE + 1) ~ log(TRAWL_DIST_m), data = subset(trawl_final, STATION_TYPE == "OpenWater")) + abline(abun_ow_lm)


#separate by site types (tidal creek)

##Distance vs. Richness

rich_tc_lm  <- lm(log(SP_RICH + 1) ~ log(TRAWL_DIST_m), data = subset(trawl_final, STATION_TYPE == "TidalCreek"))

plot(log(SP_RICH + 1) ~ log(TRAWL_DIST_m), data = subset(trawl_final, STATION_TYPE == "TidalCreek")) + abline(rich_tc_lm)


##Distance vs. Abundance 

abun_tc_lm  <- lm(log(ABUNDANCE + 1) ~ log(TRAWL_DIST_m), data = subset(trawl_final, STATION_TYPE == "TidalCreek"))

plot(log(ABUNDANCE + 1) ~ log(TRAWL_DIST_m), data = subset(trawl_final, STATION_TYPE == "TidalCreek")) + abline(abun_tc_lm)

