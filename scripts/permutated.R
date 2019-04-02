library(tidyverse)


trawl_sp <- read.csv("./data/trawl_richness_abundance.csv")


View(trawl_sp)

station_year <- trawl_sp %>% group_by(YEAR, STATION_TYPE) %>% summarize(n_distinct(STATION_ID))
View(station_year)

station_random <- trawl_sp %>% group_by(YEAR, STATION_TYPE) %>% sample_n(15, replace = T)

View(station_random)
