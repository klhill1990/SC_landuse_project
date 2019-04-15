#CALCULATING BETA DIVERSITY

###LOAD REQUIRED PACKAGES ----
library(tidyverse)
library(vegan)
library(ggplot2)
library(nlme)

###IMPORT DATASETS ----

trawl_spp <- read.csv("./data/trawl_richness_abundance.csv")
trawl_matrix <- read.csv("./data/species_matrix.csv")
trawl_matrix_ref <- read.csv("./data/species_matrix_reference.csv")

###CALCULATE GAMMA DIVERSITY ----
#total species richness across all stations per year using sample-based rarefaction with species accumulation curves

#combine species matrix with reference matrix
trawl_data <- cbind(trawl_matrix_ref, trawl_matrix)

##TIDAL CREEK SPECIES RICHNESS 

#create empty matrix to store results of loops
output_tc <- as.data.frame(NA, ncol = 2, nrow = 1)

#run for loop where index is years 1999 to 2018
for (i in 1999:2018)
{
  #filter trawl data by station type and year (index value)
  sp_matrix <- trawl_data %>% filter(STATION_TYPE == "TidalCreek", YEAR == i)
  
  #clear columns from data that do not contain species count data
  sp_matrix <- sp_matrix[,5:133]
  
  #run species accumulation function on matrix
  sp_accum <- specaccum(sp_matrix)
  
  #extract richness and standard deviation values from species accumulation results (note that '12' represents minimum number of sites visited per year)
  rich <- sp_accum$richness[12]
  sd <- sp_accum$sd[12]
  
  #store resulting values in empty matrix
  output_tc [i, 1] <- rich
  output_tc [i, 2] <- sd
}

#trim the for loop's output down to rows with correct years
output_tc <- output_tc[1999:2018,]

#create new column with years
output_tc$YEAR <- 1999:2018

#rename and reorder columns to tidy up dataframe
names(output_tc) <- c("RICHNESS", "SD" , "YEAR")

#save final dataset
tidalcreek_gamma <- output_tc[c(3,1,2)]

##OPEN WATER SPECIES RICHNESS

#repeat steps above but for open water sites (note that the minimum sites/yr are 11 for open water sites)
output_ow <- as.data.frame(NA, ncol = 2, nrow = 1)

for (i in 1999:2018)
{
  sp_matrix <- trawl_data %>% filter(STATION_TYPE == "OpenWater", YEAR == i)
  sp_matrix <- sp_matrix[,5:133]
  sp_accum <- specaccum(sp_matrix)
  rich <- sp_accum$richness[11]
  sd <- sp_accum$sd[11]
  output_ow [i, 1] <- rich
  output_ow [i, 2] <- sd
}

output_ow <- output_ow[1999:2018,]
output_ow$YEAR <- 1999:2018
names(output_ow) <- c("RICHNESS", "SD" , "YEAR")
openwater_gamma <- output_ow[c(3,1,2)]

###CALCULATE ALPHA DIVERSITY ----
#average station-level species richness per year

#use group by and summarize functions to calculate average species richness per station per year (separated by station type)
trawl_yearly_summary <- trawl_spp %>% 
                        group_by(YEAR, STATION_TYPE) %>% 
                        summarize(mean(SP_RICH), sd(SP_RICH), mean(ABUNDANCE), sd(ABUNDANCE), n_distinct(STATION_ID))

#rename and reorder columns
names(trawl_yearly_summary) <- c("YEAR","STATION_TYPE","SP_RICH_avg", "SP_RICH_sd", "ABUNDANCE_avg", "ABUNDANCE_sd", "NO_STATIONS")
trawl_yearly_summary        <- trawl_yearly_summary[c(1,2,7,3,4,5,6)]

#create two new dataframes of alpha species richness for open water and tidal creek sites
tidalcreek_alpha <- subset(trawl_yearly_summary, STATION_TYPE == "TidalCreek")
openwater_alpha  <- subset(trawl_yearly_summary, STATION_TYPE == "OpenWater")

### CALCULATE BETA DIVERSITY ---
#species richness between sites by year (alpha richness/gamma richness) 

#pull in alpha and gamma richness values from previous datasets
tidalcreek_beta <- tidalcreek_alpha[c(1,4)]
openwater_beta <- openwater_alpha[c(1,4)]

tidalcreek_beta$gamma_rich <- tidalcreek_gamma$RICHNESS
openwater_beta$gamma_rich  <- openwater_gamma$RICHNESS

#calculate beta richness in new column 
tidalcreek_beta$beta_rich <- (tidalcreek_beta$gamma_rich/tidalcreek_beta$SP_RICH_avg)
openwater_beta$beta_rich <- (openwater_beta$gamma_rich/openwater_beta$SP_RICH_avg)

#combine different scales of richness into one dataframe
tidal_rich_all <- tidalcreek_beta
names(tidal_rich_all) <- c("YEAR", "ALPHA_RICH", "GAMMA_RICH", "BETA_RICH")
tidal_rich_all[c(1,2,4,3)]

open_rich_all <- openwater_beta
names(open_rich_all) <- c("YEAR", "ALPHA_RICH", "GAMMA_RICH", "BETA_RICH")
open_rich_all[c(1,2,4,3)]

open_rich_all$STATION_TYPE <- "OpenWater"
tidal_rich_all$STATION_TYPE <- "TidalCreek"

rich_all <- rbind(open_rich_all, tidal_rich_all)

##PLOT RESULTS -----

beta_richness_open  <-  qplot(data = open_rich_all, x = YEAR, y = BETA_RICH) +
                        geom_smooth(method = "lm", se = T, col = 'black') +
                        labs(title = "Beta Species Richness", subtitle = "Open Water SCECAP Stations (1999-2018)", y = "Species Richness", x = "Year")

beta_richness_tidal <-  qplot(data = tidal_rich_all, x = YEAR, y = BETA_RICH) +
                        geom_smooth(method = "lm", se = T, col = 'black') +
                        labs(title = "Species Richness", subtitle = "Tidal Creek SCECAP Stations (1999-2018)", y = "Species Richness", x = "Year")

ggsave("./output/beta_richness_open.pdf", beta_richness_open)
ggsave("./output/beta_richness_tidal.pdf", beta_richness_tidal)

##MODEL RESULTS ----

#use GLS function with temporal autocorrelation (years) 
beta_rich_open_gls  <- gls(BETA_RICH ~ YEAR, data = open_rich_all, correlation = corAR1(form = ~ YEAR))
beta_rich_tidal_gls <- gls(BETA_RICH ~ YEAR, data = tidal_rich_all, correlation = corAR1(form = ~ YEAR))

summary(beta_rich_open_gls)
summary(beta_rich_tidal_gls)
