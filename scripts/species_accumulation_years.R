#load required packages
library(tidyverse)
library(vegan)

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

#plot results
plot(RICHNESS ~ YEAR, data = tidalcreek_final, main = "Tidal Creek Sp. Richness")
plot(RICHNESS ~ YEAR, data = openwater_final, main = "Open Water Sp. Richness")


