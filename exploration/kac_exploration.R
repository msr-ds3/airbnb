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

# load data into dataframes
load("../airbnb.RData")

crime_data <- read_csv("NYPD_7_Major_Felony_Incident_Map.csv", na='\\N')
crime_data <- crime_data %>% extract(`Location 1`, c("Latitude", "Longitude"), "\\(([^,]+), ([^)]+)\\)")
#mutate as.numeric Lat Long column

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

distinct(listings, host_id)

r <- GET('http://data.beta.nyc//dataset/0ff93d2d-90ba-457c-9f7e-39e47bf2ac5f/resource/35dd04fb-81b3-479b-a074-a27a37888ce7/download/d085e2f8d0b54d4590b1e7d1f35594c1pediacitiesnycneighborhoods.geojson')
nyc_neighborhoods <- readOGR(content(r,'text'), 'OGRGeoJSON', verbose = F)

points <- subset(transitListings, select = c("latitude", "longitude"))
points_spdf <- points
coordinates(points_spdf) <- ~longitude + latitude
proj4string(points_spdf) <- proj4string(nyc_neighborhoods)
matches <- over(points_spdf, nyc_neighborhoods)
points <- cbind(points, matches)


leaflet(nyc_neighborhoods) %>%
  addTiles() %>% 
  addPolygons(popup = ~neighborhood) %>% 
  addMarkers(~longitude, ~latitude, popup = ~neighborhood, data = points) %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(-73.98, 40.75, zoom = 13)


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

