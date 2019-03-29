#CREATING WATERSHED SUMMARIES w/ STATION TYPES (tidal creek vs. open water)
library(tidyr)
library(dplyr)


#import scecap data

scecap <- read.csv("./data/SCECAP_data.csv")

#use 'dplyr' functions to summarize environmental data by watershed

HUC08_summary2 <- scecap %>% 
  group_by(HUC_08, STATION_TYPE) %>% 
  summarize_at(vars("BIBI_CP","ERMQ","ERMQ_Metals_08", "ERMQ_PAH_13", "ERMQ_PCB_Total", "ERMQ_Pest_02"), mean, na.rm = TRUE)

HUC10_summary2 <- scecap %>% 
  group_by(HUC_10, STATION_TYPE) %>% 
  summarize_at(vars("BIBI_CP","ERMQ","ERMQ_Metals_08", "ERMQ_PAH_13", "ERMQ_PCB_Total", "ERMQ_Pest_02"), mean, na.rm = TRUE) 

HUC12_summary2 <- scecap %>% 
  group_by(HUC_12, STATION_TYPE) %>% 
  summarize_at(vars("BIBI_CP","ERMQ","ERMQ_Metals_08", "ERMQ_PAH_13", "ERMQ_PCB_Total", "ERMQ_Pest_02"), mean, na.rm = TRUE)  

HUC14_summary2 <- scecap %>% 
  group_by(HUC_14, STATION_TYPE) %>% 
  summarize_at(vars("BIBI_CP","ERMQ","ERMQ_Metals_08", "ERMQ_PAH_13", "ERMQ_PCB_Total", "ERMQ_Pest_02"), mean, na.rm = TRUE)  

#count number of stations per watershed

HUC08_stationcounts <- scecap %>% group_by(HUC_08) %>% summarize(n_distinct(STATION_ID))
HUC10_stationcounts <- scecap %>% group_by(HUC_10) %>% summarize(n_distinct(STATION_ID))
HUC12_stationcounts <- scecap %>% group_by(HUC_12) %>% summarize(n_distinct(STATION_ID))
HUC14_stationcounts <- scecap %>% group_by(HUC_14) %>% summarize(n_distinct(STATION_ID))

HUC08_stationcounts
HUC08_summary2
#join station count to summary data

HUC08_join <- full_join(HUC08_summary2, HUC08_stationcounts, by = "HUC_08")
HUC10_join <- full_join(HUC10_summary2, HUC10_stationcounts, by = "HUC_10")
HUC12_join <- full_join(HUC12_summary2, HUC12_stationcounts, by = "HUC_12")
HUC14_join <- full_join(HUC14_summary2, HUC14_stationcounts, by = "HUC_14")

#rename HUC_xx to HUC_ID 

HUC08_rename <- rename(HUC08_join, HUC_ID = HUC_08)
HUC10_rename <- rename(HUC10_join, HUC_ID = HUC_10)
HUC12_rename <- rename(HUC12_join, HUC_ID = HUC_12)
HUC14_rename <- rename(HUC14_join, HUC_ID = HUC_14)


#create new column of HUC digits (8,10,12,14)

HUC08_rename$HUC_DIGITS <- 8
HUC10_rename$HUC_DIGITS <- 10
HUC12_rename$HUC_DIGITS <- 12
HUC14_rename$HUC_DIGITS <- 14

#combine into one file

watershed_env_data2 <- bind_rows(HUC08_rename, HUC10_rename, HUC12_rename, HUC14_rename)


#rename columns

names(watershed_env_data2) <- c("HUC_ID", "STATION_TYPE", "BIBI", "ERMQ", "ERMQ_METAL", "ERMQ_PAH", "ERMQ_PCB", "ERMQ_PEST", "NUMBER_STATIONS", "HUC_DIGITS")

#reorder columns

watershed_env_data2 <- watershed_env_data2[c(1,10,9,2,3,4,5,6,7,8)]

#print results

watershed_env_data2

write.csv(watershed_env_data2, "./output/watershed_env_data2.csv")
View(watershed_env_data2)
```

