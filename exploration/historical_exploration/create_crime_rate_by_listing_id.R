library(dplyr)
library(readr)
library(ggplot2)
library(reshape)
library(scales)
library(tidyr)
library(lubridate)

library(sp)
library(ggmap)
library(maptools)
library(broom)
library(httr)
library(rgdal)



#load and prep crime data
crime_data <- read_csv("../../NYPD_7_Major_Felony_Incident_Map.csv", na='\\N')
crime_data <- crime_data %>% extract(`Location 1`, c("Latitude", "Longitude"), "\\(([^,]+), ([^)]+)\\)")
crime_data <- mutate(crime_data, Latitude = as.numeric(Latitude), Longitude = as.numeric(Longitude))
recent_crime_data <- filter(crime_data, `Occurrence Year` == 2015 | `Occurrence Year` == 2014 )

#load neighborhood population data
population_data <- read_csv("../../New_York_City_Population_By_Neighborhood_Tabulation_Areas.csv", na='\\N')
population_data <- filter(population_data, Year == 2010)
names(population_data)[names(population_data) == 'NTA Name'] <- 'NTAName'
names(population_data)[names(population_data) == 'NTA Code'] <- 'NTACode'

#load listings_history
load("listings_history.RData")

#load listings from listings history###

#get shapefile for nyc neighborhood tabulation areas
r <- GET('http://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services/nynta/FeatureServer/0/query?where=1=1&outFields=*&outSR=4326&f=geojson')
nyc_neighborhoods <- readOGR(content(r,'text'), 'OGRGeoJSON', verbose = F)
nyc_neighborhoods_df <- tidy(nyc_neighborhoods)
nyc_map <- get_map(location = c(lon = -74.00, lat = 40.71), maptype = "terrain", zoom = 10)

####get Crime rate by NTA ###
#convert crime long and lat to coordinates and convert them to projection of ny shapefile
crime_locations <- data.frame(latitude=recent_crime_data$Latitude, longitude=recent_crime_data$Longitude)
crime_locations<- crime_locations[complete.cases(crime_locations),]
crime_locations_spdf <- crime_locations 
coordinates(crime_locations_spdf) <- ~longitude + latitude 
proj4string(crime_locations_spdf) <- proj4string(nyc_neighborhoods) 

#map long and lat of crimes to neighborhoods
crime_matches <- over(crime_locations_spdf, nyc_neighborhoods)
crime_locations <- cbind(crime_locations, crime_matches)


#get sum of crimes per neighborhood and join dataframes
crimes_by_neighborhood <- crime_locations %>%
  group_by(NTACode) %>%
  summarize(num_crimes=n())

#join population to crimes
crimes_by_neighborhood <- left_join(crimes_by_neighborhood, population_data, by="NTACode")

#calculate crime rate per 1000 residents
crimes_by_neighborhood <- mutate(crimes_by_neighborhood, crime_rate= (1000*(num_crimes/Population)))

####find NTA associated with each listing ###
#convert listings long and lat to coordinates and convert them to projection of ny shapefile
listings_locations <- data.frame(listing_id= all_listings_2015$id, latitude=all_listings_2015$latitude, longitude=all_listings_2015$longitude)
listings_locations<- listings_locations[complete.cases(listings_locations),]
listings_locations_spdf <- listings_locations
coordinates(listings_locations_spdf) <- ~longitude + latitude 
proj4string(listings_locations_spdf) <- proj4string(nyc_neighborhoods) 


#map long and lat of crimes to neighborhoods
listing_matches <- over(listings_locations_spdf, nyc_neighborhoods)
listings_locations <- cbind(listings_locations, listing_matches)


#select relevant columns from crimes and listings
crime_NTA <- select(crimes_by_neighborhood, NTACode, num_crimes, NTAName, Population, crime_rate)
listings_NTA <-  select(listings_locations, listing_id, NTACode)

#join crimes onto listings
crime_rate_by_listings <- left_join(listings_NTA, crime_NTA, by="NTACode")

#select relevant columns
listings_NTA_crime_rate <- select(crime_rate_by_listings, listing_id, crime_rate)

#write previous date_frame to a csv file
write_csv(listings_NTA_crime_rate, "../../raw_data/crime_and_listings.csv")




