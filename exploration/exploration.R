# Erica Ram

library(ggplot2)
library(tigris)
library(dplyr)
library(leaflet)
library(sp)
library(ggmap)
library(maptools)
library(broom)
library(httr)
library(rgdal)
load("../airbnb.RData")

# number of listings per neighbourhood colored by neighbourhood group
ggplot(listings, aes(x = neighbourhood_cleansed, fill = neighbourhood_group_cleansed)) +
  geom_bar()

# number of listings per zipcode
ggplot(listings, aes(x = zipcode)) +
  geom_bar()

# number of listings per neighbourhood group
# based on this graph, it can be determined that the most listings as of 6/2/2016 are in Manhattan
ggplot(listings, aes(x = neighbourhood_group_cleansed)) +
  geom_bar()

# histogram of dates and different bars showing number of listings available
# because this is a look at the future availability from 6/2/2016, the soonest times tend to be 
# booked more than not, and also upticks can be noticed around holiday times (Thanksgiving and Christmas)
ggplot(calendar, aes(x = date, fill = available)) + 
  geom_bar()

# dot plot of review ratings compared to the number of reviews
# listings with more reviews appear to have a higher ratings overall, my guess being that ones with good
# initial ratings are more likely to be booked and reviewed again compared to ones with a small # of bad reviews
ggplot(listings, aes(x = review_scores_rating, y = number_of_reviews)) +
  geom_point()

# count by room type, colored by neighbourhood group
# mostly entire home/apt in all 5 groups, shared room only apparent in Manhattan, Queens, and Brooklyn
ggplot(listings, aes(x = room_type, fill = neighbourhood_group_cleansed)) +
  geom_bar()

# count of listings by rating and colored by instant bookable
# based on this graph, instant bookable listings make up a minority of the total listings regardless of rating
ggplot(listings, aes(x = review_scores_rating, fill = instant_bookable)) + 
  geom_bar()

# count by bed type
# most listings have a "Real Bed"
ggplot(listings, aes(x = bed_type)) + 
  geom_bar()

# experimenting with the map libraries
r <- GET('http://data.beta.nyc//dataset/0ff93d2d-90ba-457c-9f7e-39e47bf2ac5f/resource/35dd04fb-81b3-479b-a074-a27a37888ce7/download/d085e2f8d0b54d4590b1e7d1f35594c1pediacitiesnycneighborhoods.geojson')
nyc_neighborhoods <- readOGR(content(r,'text'), 'OGRGeoJSON', verbose = F)
nyc_neighborhoods_df <- tidy(nyc_neighborhoods)

a <- GET('https://data.cityofnewyork.us/api/geospatial/mzbd-kucq?method=export&format=GeoJSON')
areas_of_interest <- readOGR(content(a,'text'), 'OGRGeoJSON', verbose = F)
areas_of_interest_df <- tidy(areas_of_interest)
areas_of_interest_df <- mutate(areas_of_interest_df, long = coords.x1, lat = coords.x2, coords.x1 = NULL, coords.x2 = NULL)

nyc_map <- get_map(location = c(lon = -74.00, lat = 40.71), maptype = "terrain", zoom = 10)
save(nyc_neighborhoods, nyc_neighborhoods_df, areas_of_interest_df, nyc_map, file = "maps.RData")

load("maps.RData")
# leaflet map experiment
points <- data.frame(lat=listings$latitude, lng=listings$longitude)
points <- points[complete.cases(points),]
points_spdf <- points
coordinates(points_spdf) <- ~lng + lat
proj4string(points_spdf) <- proj4string(nyc_neighborhoods)
matches <- over(points_spdf, nyc_neighborhoods)
points <- cbind(points, matches)
leaflet(nyc_neighborhoods) %>%
  addTiles() %>% 
  addPolygons(popup = ~neighborhood) %>% 
  addMarkers(~lng, ~lat, popup = ~neighborhood, data = points) %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(-73.98, 40.75, zoom = 10)

# ggmap experiment
# plotting locations by lat and long
avg_price_by_neighbourhood <- group_by(listings, neighbourhood_cleansed) %>% summarize(total_listings = n(),total_price = sum(price)) %>% mutate(neighborhood = neighbourhood_cleansed, neighbourhood_cleansed = NULL, avg_price = total_price/total_listings)
df <- mutate(avg_price_by_neighbourhood, total_listings = NULL, total_price = NULL)
listings_with_avg_price <- inner_join(df, listings, by = "neighbourhood_cleansed")
nyc_neighborhoods_with_avg_price <- inner_join(nyc_neighborhoods_df, avg_price_by_neighbourhood, by = "neighborhood", copy = TRUE)

ggmap(nyc_map) + 
  geom_polygon(data=nyc_neighborhoods_df, aes(x=long, y=lat, group=group), color="blue", fill=NA) +
  geom_point(data=listings_with_avg_price, aes(x = longitude, y = latitude, color = log(avg_price))) + 
  geom_point(data=areas_of_interest_df, aes(x = long, y = lat), color="red")


# experimenting to color neighborhoods by average price with ggmap
points_by_neighborhood <- listings_with_avg_price %>%
  mutate(neighborhood = neighbourhood, neighbourhood = NULL) %>%
  group_by(neighborhood) %>%
  dplyr::summarize(num_points=n())

df1 <- left_join(points_by_neighborhood, avg_price_by_neighbourhood, by = "neighborhood")

plot_data <- tidy(nyc_neighborhoods, region="neighborhood") %>%
  left_join(., df1, by=c("id"="neighborhood")) %>%
  filter(!is.na(num_points))

manhattan_map <- get_map(location = c(lon = -73.93, lat = 40.75), maptype = "terrain", zoom = 11)

# this plot shows the log number of points (listings) by neighborhood
ggmap(manhattan_map) + 
  geom_polygon(data=plot_data, aes(x=long, y=lat, group=group, fill=log(avg_price)), color="gray", alpha=0.75) +
  scale_fill_gradient(low="blue", high="red")
  #scale_fill_gradientn(colours = rainbow(10))
