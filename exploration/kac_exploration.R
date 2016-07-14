library(dplyr)
library(readr)
library(ggplot2)
library(reshape)
library(scales)
library(tidyr)
library(lubridate)

library(tigris)
library(leaflet)
library(sp)
library(ggmap)
library(maptools)
library(broom)
library(httr)
library(rgdal)

# load airbnb data into dataframes
load("airbnb.RData")

#load and prep crime data
crime_data <- read_csv("NYPD_7_Major_Felony_Incident_Map.csv", na='\\N')
crime_data <- crime_data %>% extract(`Location 1`, c("Latitude", "Longitude"), "\\(([^,]+), ([^)]+)\\)")
crime_data <- mutate(crime_data, Latitude = as.numeric(Latitude), Longitude = as.numeric(Longitude))


#price by neighborhood
listings %>% group_by(zipcode) %>% ggplot(aes(x = neighbourhood, y = price)) + geom_point() + scale_y_log10()

#num of reviews by price
ggplot(listings, aes (x = number_of_reviews, y = price)) + geom_point()

#minimum length of stay by borough for entire  home/apt
listings %>% filter(room_type == "Entire home/apt") %>% ggplot(aes(x = neighbourhood_group_cleansed , y = minimum_nights)) + geom_point() + scale_y_log10()

#Entire home/apt listings per borough
listings %>% filter(room_type == "Entire home/apt") %>% group_by(neighbourhood_cleansed) %>% ggplot(aes(x = neighbourhood_group_cleansed)) + geom_bar()

#general listings per borough
listings %>% group_by(neighbourhood_cleansed) %>% ggplot(aes(x = neighbourhood_group_cleansed)) + geom_bar()


#average location review
reviewByNeigh <- listings %>% group_by(neighbourhood_group_cleansed) %>% summarise(avgLocReview = mean(review_scores_location, na.rm = TRUE), numlistings = n())
reviewByNeigh %>% ggplot(aes(x = neighbourhood_group_cleansed, y = avgLocReview)) + geom_point()

#average price per neigh
priceByNeigh <- listings %>% group_by(neighbourhood_group_cleansed) %>% summarise(avgPrice= mean(price, na.rm = TRUE))
priceByNeigh %>% ggplot(aes(x = neighbourhood_group_cleansed, y = avgPrice)) + geom_point()  

#listings near mass transit
transitListings <- filter(listings, grepl('subway|train|station|trains|subways|stations', transit, ignore.case = TRUE))

#comparing prices, num reviews ratings btwn host and superhost
ggplot(listings, aes (x = number_of_reviews, y = price, color = is_host_superhost)) + geom_point() + scale_y_log10()
filter(listings, is_host_superhost == "t") %>% summarise(avgPrice= mean(price, na.rm = TRUE), avgRating= mean(review_scores_rating, na.rm = TRUE), avgNumRatings = mean(number_of_reviews, na.rm = TRUE))
filter(listings, is_host_superhost == "f") %>% summarise(avgPrice= mean(price, na.rm = TRUE), avgRating= mean(review_scores_rating, na.rm = TRUE), avgNumRatings = mean(number_of_reviews, na.rm = TRUE))

#listings near mass transit
listings <- mutate(listings, hasTransit = grepl('subway|train|station|trains|subways|stations', transit, ignore.case = TRUE))

#affect of mass transit on price and number of reviews
ggplot(listings, aes (x = number_of_reviews, y = price, color = hasTransit)) + geom_point() + scale_y_log10()
filter(listings, grepl('subway|train|station|trains|subways|stations', transit, ignore.case = TRUE)) %>% summarise(avgPrice= mean(price, na.rm = TRUE), avgRating= mean(review_scores_rating, na.rm = TRUE), avgNumRatings = mean(number_of_reviews, na.rm = TRUE))
  #hasTransit = TRUE
  #avgPrice   avgRating   avgNumRatings
  #146.0276   92.64457      15.51466
filter(listings, !grepl('subway|train|station|trains|subways|stations', transit, ignore.case = TRUE)) %>% summarise(avgPrice= mean(price, na.rm = TRUE), avgRating= mean(review_scores_rating, na.rm = TRUE), avgNumRatings = mean(number_of_reviews, na.rm = TRUE))
  #hasTransit = FALSE
  #avgPrice avgRating avgNumRatings
  #159.4239  91.98864      9.920217

#affect of hotels proximity on price

#affect of attractions of price

#distinct host ID and neigh/zipcode/latitude longitude
distinct(listings, host_id)

#safe locations
safe_listings <- filter(listings, grepl('safe', name, ignore.case = TRUE)|grepl('safe', summary, ignore.case = TRUE)|grepl('safe', space, ignore.case = TRUE)|grepl('safe', description, ignore.case = TRUE)|grepl('safe', neighborhood_overview, ignore.case = TRUE)) 

#crime_data from 2015 and 2014
recent_crime_data <- filter(crime_data, `Occurrence Year` == 2015 | `Occurrence Year` == 2014 )

#get shapefile for nyc neighborhoods
r <- GET('http://data.beta.nyc//dataset/0ff93d2d-90ba-457c-9f7e-39e47bf2ac5f/resource/35dd04fb-81b3-479b-a074-a27a37888ce7/download/d085e2f8d0b54d4590b1e7d1f35594c1pediacitiesnycneighborhoods.geojson')
nyc_neighborhoods <- readOGR(content(r,'text'), 'OGRGeoJSON', verbose = F)
nyc_neighborhoods_df <- tidy(nyc_neighborhoods)
nyc_map <- get_map(location = c(lon = -74.00, lat = 40.71), maptype = "terrain", zoom = 10)


#convert crime long and lat to coordinates and convert them to projection of ny shapefile
crime_locations <- data.frame(latitude=recent_crime_data$Latitude, longitude=recent_crime_data$Longitude)
crime_locations<- crime_locations[complete.cases(crime_locations),]
crime_locations_spdf <- crime_locations 
coordinates(crime_locations_spdf) <- ~longitude + latitude 
proj4string(crime_locations_spdf) <- proj4string(nyc_neighborhoods) 

#map long and lat of crimes to neighborhoods
matches <- over(crime_locations_spdf, nyc_neighborhoods)
crime_locations <- cbind(crime_locations, matches)

#get sum of crimes per neighborhood and join dataframes
crimes_by_neighborhood <- crime_locations %>%
  group_by(neighborhood) %>%
  summarize(num_crimes=n())
plot_data <- tidy(nyc_neighborhoods, region="neighborhood") %>%
  left_join(., crimes_by_neighborhood, by=c("id"="neighborhood")) %>%
  filter(!is.na(num_crimes))

#map of num crime by neighborhood
ggmap(nyc_map) + 
  geom_polygon(data=plot_data, aes(x=long, y=lat, group=group, fill=num_crimes)) +
  geom_point(data=safe_listings, aes(x = longitude, y = latitude, color = "red", alpha= 0.1))

#
