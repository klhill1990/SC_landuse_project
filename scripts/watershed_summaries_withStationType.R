#Summarize watershed enviormental data grouping by HUC size and site type (open water vs. tidal creek)


library(tidyr)
library(dplyr)

#import scecap data

scecap <- read.csv("./data/SCECAP_data.csv")

#create separate dataframes with 2 station types

scecap_ow <- filter(scecap, STATION_TYPE == 'OpenWater')
scecap_tc <- filter(scecap, STATION_TYPE == 'TidalCreek')

#use 'dplyr' functions to summarize environmental data by watershed

HUC08_summary_ow <- scecap_ow %>% 
  group_by(HUC_08) %>% 
  summarize_at(vars("BIBI_CP","ERMQ","ERMQ_Metals_08", "ERMQ_PAH_13", "ERMQ_PCB_Total", "ERMQ_Pest_02"), mean, na.rm = TRUE)

HUC10_summary_ow <- scecap_ow %>% 
  group_by(HUC_10) %>% 
  summarize_at(vars("BIBI_CP","ERMQ","ERMQ_Metals_08", "ERMQ_PAH_13", "ERMQ_PCB_Total", "ERMQ_Pest_02"), mean, na.rm = TRUE) 

HUC12_summary_ow <- scecap_ow %>% 
  group_by(HUC_12) %>% 
  summarize_at(vars("BIBI_CP","ERMQ","ERMQ_Metals_08", "ERMQ_PAH_13", "ERMQ_PCB_Total", "ERMQ_Pest_02"), mean, na.rm = TRUE)  

HUC14_summary_ow <- scecap_ow %>% 
  group_by(HUC_14) %>% 
  summarize_at(vars("BIBI_CP","ERMQ","ERMQ_Metals_08", "ERMQ_PAH_13", "ERMQ_PCB_Total", "ERMQ_Pest_02"), mean, na.rm = TRUE)  

HUC08_summary_tc <- scecap_tc %>% 
  group_by(HUC_08) %>% 
  summarize_at(vars("BIBI_CP","ERMQ","ERMQ_Metals_08", "ERMQ_PAH_13", "ERMQ_PCB_Total", "ERMQ_Pest_02"), mean, na.rm = TRUE)

HUC10_summary_tc <- scecap_tc %>% 
  group_by(HUC_10) %>% 
  summarize_at(vars("BIBI_CP","ERMQ","ERMQ_Metals_08", "ERMQ_PAH_13", "ERMQ_PCB_Total", "ERMQ_Pest_02"), mean, na.rm = TRUE) 

HUC12_summary_tc <- scecap_tc %>% 
  group_by(HUC_12) %>% 
  summarize_at(vars("BIBI_CP","ERMQ","ERMQ_Metals_08", "ERMQ_PAH_13", "ERMQ_PCB_Total", "ERMQ_Pest_02"), mean, na.rm = TRUE)  

HUC14_summary_tc <- scecap_tc %>% 
  group_by(HUC_14) %>% 
  summarize_at(vars("BIBI_CP","ERMQ","ERMQ_Metals_08", "ERMQ_PAH_13", "ERMQ_PCB_Total", "ERMQ_Pest_02"), mean, na.rm = TRUE)  

#count number of stations per watershed

HUC08_stationcount_ow <- scecap_ow %>% group_by(HUC_08) %>% summarize(n_distinct(STATION_ID))
HUC10_stationcount_ow <- scecap_ow %>% group_by(HUC_10) %>% summarize(n_distinct(STATION_ID))
HUC12_stationcount_ow <- scecap_ow %>% group_by(HUC_12) %>% summarize(n_distinct(STATION_ID))
HUC14_stationcount_ow <- scecap_ow %>% group_by(HUC_14) %>% summarize(n_distinct(STATION_ID))

HUC08_stationcount_tc <- scecap_tc %>% group_by(HUC_08) %>% summarize(n_distinct(STATION_ID))
HUC10_stationcount_tc <- scecap_tc %>% group_by(HUC_10) %>% summarize(n_distinct(STATION_ID))
HUC12_stationcount_tc <- scecap_tc %>% group_by(HUC_12) %>% summarize(n_distinct(STATION_ID))
HUC14_stationcount_tc <- scecap_tc %>% group_by(HUC_14) %>% summarize(n_distinct(STATION_ID))


#join station count to summary data
HUC08_joined_ow <- full_join(HUC08_summary_ow, HUC08_stationcount_ow, by = "HUC_08")
HUC10_joined_ow <- full_join(HUC10_summary_ow, HUC10_stationcount_ow, by = "HUC_10")
HUC12_joined_ow <- full_join(HUC12_summary_ow, HUC12_stationcount_ow, by = "HUC_12")
HUC14_joined_ow <- full_join(HUC14_summary_ow, HUC14_stationcount_ow, by = "HUC_14")

HUC08_joined_tc <- full_join(HUC08_summary_tc, HUC08_stationcount_tc, by = "HUC_08")
HUC10_joined_tc <- full_join(HUC10_summary_tc, HUC10_stationcount_tc, by = "HUC_10")
HUC12_joined_tc <- full_join(HUC12_summary_tc, HUC12_stationcount_tc, by = "HUC_12")
HUC14_joined_tc <- full_join(HUC14_summary_tc, HUC14_stationcount_tc, by = "HUC_14")

#rename HUC_xx to HUC_ID 

HUC08_renamed_ow <- rename(HUC08_joined_ow, HUC_ID = HUC_08)
HUC10_renamed_ow <- rename(HUC10_joined_ow, HUC_ID = HUC_10)
HUC12_renamed_ow <- rename(HUC12_joined_ow, HUC_ID = HUC_12)
HUC14_renamed_ow <- rename(HUC14_joined_ow, HUC_ID = HUC_14)

HUC08_renamed_tc <- rename(HUC08_joined_tc, HUC_ID = HUC_08)
HUC10_renamed_tc <- rename(HUC10_joined_tc, HUC_ID = HUC_10)
HUC12_renamed_tc <- rename(HUC12_joined_tc, HUC_ID = HUC_12)
HUC14_renamed_tc <- rename(HUC14_joined_tc, HUC_ID = HUC_14)


#create new column of HUC digits (8,10,12,14)

HUC08_renamed_ow$HUC_DIGITS <- 8
HUC10_renamed_ow$HUC_DIGITS <- 10
HUC12_renamed_ow$HUC_DIGITS <- 12
HUC14_renamed_ow$HUC_DIGITS <- 14

HUC08_renamed_tc$HUC_DIGITS <- 8
HUC10_renamed_tc$HUC_DIGITS <- 10
HUC12_renamed_tc$HUC_DIGITS <- 12
HUC14_renamed_tc$HUC_DIGITS <- 14

#combine into one file

watershed_env_data_ow <- bind_rows(HUC08_renamed_ow, HUC10_renamed_ow, HUC12_renamed_ow, HUC14_renamed_ow)
watershed_env_data_tc <- bind_rows(HUC08_renamed_tc, HUC10_renamed_tc, HUC12_renamed_tc, HUC14_renamed_tc)


#rename columns

names(watershed_env_data_ow) <- c("HUC_ID", "BIBI", "ERMQ", "ERMQ_METAL", "ERMQ_PAH", "ERMQ_PCB", "ERMQ_PEST", "NUMBER_STATIONS", "HUC_DIGITS")
names(watershed_env_data_tc) <- c("HUC_ID", "BIBI", "ERMQ", "ERMQ_METAL", "ERMQ_PAH", "ERMQ_PCB", "ERMQ_PEST", "NUMBER_STATIONS", "HUC_DIGITS")
#reorder columns

watershed_env_data_ow <- watershed_env_data_ow[c(1,9,8,2,3,4,5,6,7)]
watershed_env_data_tc <- watershed_env_data_tc[c(1,9,8,2,3,4,5,6,7)]
#print results

watershed_env_data_ow
watershed_env_data_tc

write.csv(watershed_env_data_ow, "./output/watershed_env_data_openwater.csv")
write.csv(watershed_env_data_tc, "./output/watershed_env_data_tidalcreek.csv")
```

