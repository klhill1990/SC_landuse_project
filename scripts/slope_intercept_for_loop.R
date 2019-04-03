#load required packages
library(tidyverse)

#import dataset
trawl_sp <- read.csv("./data/trawl_richness_abundance.csv")

#create blank matrices for the following for loops to populate with slope/intercepts (one for tidal creek sites, openwater sites, and both sites lumped together)
output_matrix_tidal <- as.data.frame(NULL, ncol = 2, nrow = 2)
output_matrix_open <- as.data.frame(NULL, ncol = 2, nrow = 2)
output_matrix_all <- as.data.frame(NULL, ncol = 2, nrow = 1)

#loop 1: tidal creek stations
for (i in 1:999) 
  
  {
    #group trawl data by year and station, sample 15 stations per year
    subsampled_data <- trawl_sp %>% group_by(YEAR, STATION_TYPE) %>% sample_n(15, replace = T)
    #build lm with richness as the y variable and year as the X, data subset by station type
    rich_tidal_lm   <- lm(SP_RICH ~ YEAR, data = subset(subsampled_data, STATION_TYPE == "TidalCreek"))
    #extract coefficients (slope/intercept) from lm model
    b0_tidal <- as.numeric(rich_tidal_lm$coefficients[1])
    b1_tidal <- as.numeric(rich_tidal_lm$coefficients[2])
    #populate empty matrix with these model coefficients
    output_matrix_tidal[i,1] <- b0_tidal
    output_matrix_tidal[i,2] <- b1_tidal
  }

#loop 2: open water stations (same as loop 1, but with 'openwater' sites subset)
for (i in 1:999) 
  
{
  subsampled_data <- trawl_sp %>% group_by(YEAR, STATION_TYPE) %>% sample_n(15, replace = T)
  rich_open_lm    <- lm(SP_RICH ~ YEAR, data = subset(subsampled_data, STATION_TYPE == "OpenWater"))
  b0_open <- as.numeric(rich_open_lm$coefficients[1])
  b1_open <- as.numeric(rich_open_lm$coefficients[2])
  output_matrix_open[i,1] <- b0_open
  output_matrix_open[i,2] <- b1_open
}

#loop 3: all sites (same as loops 1 and 2, but group by year only and sample 30 not 15 stations)
for (i in 1:999) 

  {
  subsampled_data_all <- trawl_sp %>% group_by(YEAR) %>% sample_n(30, replace = T)
  rich_all_lm         <- lm(SP_RICH ~ YEAR, data = subsampled_data_all)
  b0_all <- as.numeric(rich_all_lm$coefficients[1])
  b1_all <- as.numeric(rich_all_lm$coefficients[2])
  output_matrix_all[i,1] <- b0_all
  output_matrix_all[i,2] <- b1_all
}

#add columns in matrices with station type
output_matrix_tidal$STATION_TYPE <- "TidalCreek"
output_matrix_open$STATION_TYPE  <- "OpenWater"
output_matrix_all$STATION_TYPE   <- "AllTypes"

#rename columns to be more descriptive
names(output_matrix_tidal) <- c("INTERCEPT","SLOPE", "STATION_TYPE")
names(output_matrix_open)  <- c("INTERCEPT","SLOPE", "STATION_TYPE")
names(output_matrix_all)   <- c("INTERCEPT","SLOPE", "STATION_TYPE")

#use rbind to merge all output matrices into one
output_matrix_final <- rbind(output_matrix_tidal, output_matrix_open, output_matrix_all)

#summarize results
summary_stats <-  output_matrix_final %>% group_by(STATION_TYPE) %>% summarize_at(c("INTERCEPT", "SLOPE"), mean)
summary_stats
