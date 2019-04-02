library(tidyverse)

trawl_sp <- read.csv("./data/trawl_richness_abundance.csv")

trawl_subsample <- function(n) 
  {
  subsampled_data <- trawl_sp %>% group_by(YEAR, STATION_TYPE) %>% sample_n(n, replace = T)
  }

trawl_replicated <- replicate(10, trawl_subsample(15), simplify = F)

for (i in 1:10) 

    {
  output_df <- data.frame(NULL, nrows = 40, ncol = 5)
  new_df <- as.data.frame(trawl_replicated[i])
  subsample_avgs <- new_df %>% group_by(YEAR, STATION_TYPE) %>% summarize_at(c("SP_RICH", "ABUNDANCE"),mean)
  subsample_avgs <- as.data.frame(subsample_avgs)
  output_df[[i]] <- subsample_avgs
  }







plot(SP_RICH ~ YEAR, data = subset(subsample_avgs, STATION_TYPE == "TidalCreek"))
plot(SP_RICH ~ YEAR, data = subsample_avgs)
subsampled_data %>% group_by(YEAR, STATION_TYPE) %>% summarize_at(c("SP_RICH", "ABUNDANCE"),mean)
