#MAP SCECAP STATIONS IN SOUTH CAROLINA

#load required packages
library(ggplot2)
library(ggmap)

#import datasets
scecap <- read.csv("./data/landcover/SCECAP_data.csv")

#trim dataset to only station ID, lat, lon, station type, and year

scecap_xy <- scecap[c(1,5,6,8,7)]
names(scecap_xy) <- c("STATION_ID", "YEAR", "STATION_TYPE", "lon", "lat")

#set bounding box for SC coast (used boxfinder.com)
SCbbox <- c(left = -81.172485, bottom = 31.896214, right = -78.392944, top = 33.979809)

#use get_stamentmap() with bounding box to download basemap from stamen maps
SCmap <- get_stamenmap(SCbbox, maptype = 'toner-lite', source = "stamen", zoom = 9)

#plot xy coordinates from scecap dataset using geom_point
scecap_map <- ggmap(SCmap) + 
              geom_point(aes(x = lon, y = lat, col = STATION_TYPE), data = scecap_xy, size = 0.5) + 
              theme(legend.position = "bottom") + 
              labs(title = "SCECAP Stations 1999 to 2018", x = NULL, y = NULL)


#export map as pdf
ggsave("./output/scecap_map.pdf", scecap_map)
