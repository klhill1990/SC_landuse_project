#SAMPLE BASED RAREFACTION AND SPECIES ACCUMULATION CURVES
#use sample-based rarefaction to model the change in species richness over time. 


#load required packages

library(tidyverse)
library(vegan)
library(ggplot2)


#import datasets

trawl_matrix <- read.csv("./data/species_matrix.csv")
trawl_matrix_ref <- read.csv("./data/species_matrix_reference.csv")


#combine species matrix with reference matrix

trawl_data <- cbind(trawl_matrix_ref, trawl_matrix)


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
tidalcreek_final <- output_tc[c(3,1,2)]


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
openwater_final <- output_ow[c(3,1,2)]


sp_matrix03 <- trawl_data %>% filter(STATION_TYPE == "OpenWater", YEAR == 2003)
sp_matrix03 <- sp_matrix03[,5:133]
sp_accum03 <- specaccum(sp_matrix03)
plot(sp_accum03, xlab = "Sites Sampled", ylab = "Species Richness", main = "2003 Open Water Sites") + abline(v=11)


#PLOT RESULTS

#also plot example of species accumulation curve, for this example I chose year 2003

sp_matrix03_ow <- trawl_data %>% filter(STATION_TYPE == "OpenWater", YEAR == 2003)
sp_matrix03_ow <- sp_matrix03_ow[,5:133]
sp_accum03_ow <- specaccum(sp_matrix03_ow)
sp_accum_openwater_2003 <- plot(sp_accum03_ow, xlab = "Sites Sampled", ylab = "Species Richness", main = "2003 Open Water Sites") + abline(v=11)

sp_matrix03_tc <- trawl_data %>% filter(STATION_TYPE == "TidalCreek", YEAR == 2003)
sp_matrix03_tc <- sp_matrix03_tc[,5:133]
sp_accum03_tc <- specaccum(sp_matrix03_tc)
sp_accum_tidalcreek_2003 <- plot(sp_accum03_tc, xlab = "Sites Sampled", ylab = "Species Richness", main = "2003 Tidal Creek Sites") + abline(v=12)


tidalcreek_plot <-  qplot(data = tidalcreek_final, x = YEAR, y = RICHNESS) +
                    geom_errorbar(aes(x= YEAR, ymin=RICHNESS - SD, ymax= RICHNESS + SD), width=0.25) +
                    geom_smooth(method = "lm", se = F, col = 'black') +
                    labs(title = "Tidal Creek Species Richness", y = "Species Richness", x = "Year")

openwater_plot <-   qplot(data = openwater_final, x = YEAR, y = RICHNESS) +
                    geom_errorbar(aes(x= YEAR, ymin=RICHNESS - SD, ymax= RICHNESS + SD), width=0.25) +
                    geom_smooth(method = "lm", se = F, col = 'black') +
                    labs(title = "Open Water Species Richness", y = "Species Richness", x = "Year")


#export plots

ggsave("./output/tidalcreek_species_accum.pdf", tidalcreek_plot)
ggsave("./output/openwater_species_accum.pdf", openwater_plot)


#view linear model results

tidalcreek_lm <- lm(RICHNESS ~ YEAR, data = tidalcreek_final)
openwater_lm  <- lm(RICHNESS ~ YEAR, data = openwater_final)

summary(tidalcreek_lm)
summary(openwater_lm)
        