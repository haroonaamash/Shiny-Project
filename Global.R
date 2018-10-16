
library(readxl)
library(dplyr)
library(ggplot2)
library(rgdal)
library(tigris) 
library(plotly)
library(DT)
library(scales)
library(htmltools)
library(tidyr)
library(httr)

#import and read df
NYCfinal <- read.csv('NYCFINAL.csv', stringsAsFactors = FALSE)
geo.coordinates <- read.csv('LATLON copy.csv', stringsAsFactors = FALSE)

#format lattitiude and longitudes
geo.coordinates$the_geom= gsub(pattern = "POINT", replacement = "", geo.coordinates$the_geom)
geo.coordinates$the_geom= gsub("\\(|\\)", "", geo.coordinates$the_geom)
geo.coordinates$the_geom= gsub(pattern = " ", replacement = ":", geo.coordinates$the_geom)
geo.coordinates$the_geom=substr(geo.coordinates$the_geom, 2,45345464)

geo.coordinates= separate(geo.coordinates, the_geom, sep= ":",into = c('l1','l2'))
geo.coordinates= geo.coordinates %>% mutate(., Coor.dinates= paste(geo.coordinates$l2,":",geo.coordinates$l1))
geo.coordinates=geo.coordinates[,-c(2,3)]



# get neighborhood  data
r <- GET('http://data.beta.nyc//dataset/0ff93d2d-90ba-457c-9f7e-39e47bf2ac5f/resource/35dd04fb-81b3-479b-a074-a27a37888ce7/download/d085e2f8d0b54d4590b1e7d1f35594c1pediacitiesnycneighborhoods.geojson')
nyc_neighborhoods <- readOGR(content(r,'text'), 'OGRGeoJSON', verbose = F)

# initialize spatial df for map FOR HISTORIC DATA

HISTORICAL_ <- read_csv("HISTORICAL.csv")
historic.grouping <- HISTORICAL_ %>% group_by(BOROUGH,YEAR,TYPE.OF.HOME,NEIGHBORHOOD) %>% 
                          summarise(n=n(), total.transaction.vol= mean(MEDIAN.SALE.PRICE))  

nyc_neighborhoods@data$neighborhood= toupper(nyc_neighborhoods@data$neighborhood)
map_data2 <- geo_join(nyc_neighborhoods, historic.grouping, "neighborhood", "NEIGHBORHOOD")

manhattan.map = map_data2[map_data2@data$borough == "Manhattan", ]
brooklyn.map = map_data2[map_data2@data$borough == "Brooklyn", ]
queens.map = map_data2[map_data2@data$borough == "Queens", ]
bronx.map = map_data2[map_data2@data$borough == "Bronx", ]
statenisland.map = map_data2[map_data2@data$borough == "Staten Island", ]

# input variables

Select.Neighborhood = c('All',unique(NYCfinal[NYCfinal$BOROUGH == "Manhattan",]$NEIGHBORHOOD),
                        unique(NYCfinal[NYCfinal$BOROUGH == "Queens",]$NEIGHBORHOOD),
                        unique(NYCfinal[NYCfinal$BOROUGH == "Brooklyn",]$NEIGHBORHOOD),
                        unique(NYCfinal[NYCfinal$BOROUGH == "Bronx",]$NEIGHBORHOOD),
                        unique(NYCfinal[NYCfinal$BOROUGH == "Staten Island",]$NEIGHBORHOOD))

Select.Borough = c('All',unique(NYCfinal$BOROUGH))


Parameters = c('Total Volume', 'Average Median Price') 


#####--------------------------------------------------------------------------------------------

#2 variables for year and borough
chart = 
  historic.grouping %>% filter(BOROUGH == 'All') %>%
  group_by(TYPE.OF.HOME) %>%
  summarise(total.volume=median(total.transaction.vol))

chart2 = 
  historic.grouping %>% filter(BOROUGH == 'All') %>%
  group_by(YEAR) %>% 
  summarise(Mean=Mean(total.transaction.vol))


