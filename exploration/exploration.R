# Erica Ram
# exploratory analysis of the Inside Airbnb dataset for June 2016

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
library(readr)
library(tidyr)
load("../airbnb.RData")
load("maps.RData")

# number of listings per neighbourhood colored by neighbourhood group
ggplot(listings, aes(x = neighbourhood_cleansed,
                     fill = neighbourhood_group_cleansed)) +
  geom_bar()

# number of listings per zipcode
ggplot(listings, aes(x = zipcode)) +
  geom_bar()

# number of listings per neighbourhood group
# based on this graph, it can be determined that the most listings as of 
# 6/2/2016 are in Manhattan
ggplot(listings, aes(x = neighbourhood_group_cleansed)) +
  geom_bar()

# histogram of dates and different bars showing number of listings available
# because this is a look at the future availability from 6/2/2016, the soonest 
# times tend to be booked more than not, and also upticks can be noticed around 
# holiday times (Thanksgiving and Christmas)
ggplot(calendar, aes(x = date, fill = available)) + 
  geom_bar()

# dot plot of review ratings compared to the number of reviews
# listings with more reviews appear to have a higher ratings overall, my guess
# being that ones with good initial ratings are more likely to be booked and 
# reviewed again compared to ones with a small # of bad reviews
ggplot(listings, aes(x = review_scores_rating, y = number_of_reviews)) +
  geom_point()

# count by room type, colored by neighbourhood group
# mostly entire home/apt in all 5 groups, shared room only apparent in 
# Manhattan, Queens, and Brooklyn
ggplot(listings, aes(x = room_type, fill = neighbourhood_group_cleansed)) +
  geom_bar()

# count of listings by rating and colored by instant bookable
# based on this graph, instant bookable listings make up a minority of the total
# listings regardless of rating
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
areas_of_interest_df <- mutate(areas_of_interest_df, long = coords.x1,
                               lat = coords.x2, coords.x1 = NULL, coords.x2 = NULL)

nyc_map <- get_map(location = c(lon = -74.00, lat = 40.71), maptype = "terrain",
                   zoom = 10)

manhattan_map <- get_map(location = c(lon = -73.93, lat = 40.75),
                         maptype = "terrain", zoom = 11)

save(nyc_neighborhoods, nyc_neighborhoods_df, areas_of_interest_df, nyc_map,
     manhattan_map, file = "maps.RData")

load("maps.RData")
# leaflet map experiment
points <- data.frame(lat=listings$latitude, lng=listings$longitude)
points <- points[complete.cases(points),]
points <- points[1:10000,]
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
avg_price_by_neighbourhood <- group_by(listings, neighbourhood_cleansed) %>% 
  summarize(total_listings = n(),total_price = sum(price)) %>% 
  mutate(neighborhood = neighbourhood_cleansed, neighbourhood_cleansed = NULL, 
         avg_price = total_price/total_listings)
df <- mutate(avg_price_by_neighbourhood, total_listings = NULL, 
             total_price = NULL)
listings <- mutate(listings, neighborhood = neighbourhood_cleansed, 
                   neighbourhood_cleansed = NULL)
listings_with_avg_price <- inner_join(df, listings, by = "neighborhood")
nyc_neighborhoods_with_avg_price <- inner_join(as.data.frame(nyc_neighborhoods), 
                                               listings_with_avg_price, 
                                               by = "neighborhood", copy = TRUE)


ggmap(nyc_map) + 
  geom_polygon(data=nyc_neighborhoods_df, aes(x=long, y=lat, group=group), 
               color="blue", fill=NA) +
  geom_point(data=listings_with_avg_price, aes(x = longitude, y = latitude, 
                                               color = log(avg_price))) + 
  geom_point(data=areas_of_interest_df, aes(x = long, y = lat), color="red")


# experimenting to color neighborhoods by average price with ggmap
points_by_neighborhood <- listings_with_avg_price %>%
  mutate(neighborhood = neighbourhood, neighbourhood = NULL) %>%
  group_by(neighborhood) %>%
  dplyr::summarize(num_points=n())

df1 <- left_join(points_by_neighborhood, avg_price_by_neighbourhood, 
                 by = "neighborhood")

plot_data <- tidy(nyc_neighborhoods, region="neighborhood") %>%
  left_join(., df1, by=c("id"="neighborhood")) %>%
  filter(!is.na(num_points))

# this plot shows the log number of points (listings) by neighborhood
ggmap(manhattan_map) + 
  geom_polygon(data=plot_data, aes(x=long, y=lat, group=group,
                                   fill=log(avg_price)),
               color="gray", alpha=0.75) +
  scale_fill_gradient(low="blue", high="red")
  #scale_fill_gradientn(colours = rainbow(10))

# map by number of listings (num)
ggmap(manhattan_map) + 
  geom_polygon(data=plot_data, aes(x=long, y=lat, group=group, fill=num_points),
               color="gray", alpha=0.75) +
  scale_fill_gradient(low="blue", high="red")

# looking at crime stats
c <- GET('https://data.cityofnewyork.us/api/geospatial/78dh-3ptz?method=export&format=GeoJSON')
precincts <- readOGR(content(c,'text'), 'OGRGeoJSON', verbose = F)
precincts_df <- tidy(precincts)

# map with precincts overlaid in black
ggmap(manhattan_map) + 
  geom_polygon(data=plot_data, aes(x=long, y=lat, group=group, fill=num_points),
               color="gray", alpha=0.75) +
  scale_fill_gradient(low="blue", high="red") +
  geom_polygon(data=precincts_df, aes(x=long, y=lat, group=group), color="black",
               fill=NA)

# crime data by neighborhood
crime_data <- read_csv("../NYPD_7_Major_Felony_Incident_Map.csv", na='\\N')
crime_data <- crime_data %>% extract(`Location 1`, c("Latitude", "Longitude"),
                                     "\\(([^,]+), ([^)]+)\\)")
crime_points <- data.frame(lat=Vias.numeric(crime_data$Latitude),
                           lng=as.numeric(crime_data$Longitude))
crime_points <- crime_points[complete.cases(crime_points),]
crime_points_spdf <- crime_points 
coordinates(crime_points_spdf) <- ~lng + lat 
proj4string(crime_points_spdf) <- proj4string(nyc_neighborhoods) 
crime_matches <- over(crime_points_spdf, nyc_neighborhoods) 
crime_points <- cbind(crime_points, crime_matches)

df2 <- group_by(crime_points, neighborhood) %>% summarize(num_points = n()) %>% 
  left_join(crime_points, ., by = "neighborhood")

# map dot plotted with crime numbers by color -> Kaciny plotted with heatmap
ggmap(manhattan_map) + 
  geom_point(data=df2, aes(x = lng, y = lat, color = num_points)) +
  scale_color_gradient(low="blue", high="red")
  
# average reviews by neighborhood
avg_reviews <- listings[complete.cases(listings),] %>% 
  group_by(neighbourhood_cleansed) %>%
  summarize(num_listings=n(), sum_rating = sum(review_scores_rating)) %>%
  mutate(avg_reviews=sum_rating/num_listings,
         neighborhood = neighbourhood_cleansed, neighbourhood_cleansed = NULL)

plot_reviews_data <- tidy(nyc_neighborhoods, region="neighborhood") %>%
  left_join(., avg_reviews, by=c("id"="neighborhood")) %>%
  filter(!is.na(avg_reviews))

ggmap(manhattan_map) + 
  geom_polygon(data=plot_reviews_data, aes(x=long, y=lat, group=group,
                                           fill=avg_reviews), color="gray", 
               alpha=0.75)

# median asking rent by neighborhood
df <- read_csv("../rental_inventory_time_series_prices_All_Types_Any_Bedrooms.csv")
names(df)[names(df)=="2016-04-01"] <- "median_asking"
names(df)[names(df)=="Area"] <- "neighborhood"
median_neighborhood_rent <- select(df, neighborhood, Boro, AreaType,
                                   median_asking) %>% 
  filter(AreaType == "Neighborhood")
median_neighborhood_rent <- 
  median_neighborhood_rent[complete.cases(median_neighborhood_rent),]
median_neighborhood_rent[92,1] = "St. George"

plot_rent_data <- tidy(nyc_neighborhoods, region="neighborhood") %>%
  left_join(., median_neighborhood_rent, by=c("id"="neighborhood"))

ggmap(nyc_map) + 
  geom_polygon(data=plot_rent_data, aes(x=long, y=lat, group=group,
                                        fill=log(median_asking)), color="gray") +
  scale_fill_gradient(low="blue", high="red")


# median asking rent by neighborhood for Sept 2015 (less NAs)
names(df)[names(df)=="2015-09-01"] <- "median_asking"
names(df)[names(df)=="Area"] <- "neighborhood"
median_neighborhood_rent <- select(df, neighborhood, Boro, AreaType, median_asking) %>% 
  filter(AreaType == "Neighborhood")
median_neighborhood_rent <- 
  median_neighborhood_rent[complete.cases(median_neighborhood_rent),]
median_neighborhood_rent[92,1] = "St. George"

plot_rent_data <- tidy(nyc_neighborhoods, region="neighborhood") %>%
  left_join(., median_neighborhood_rent, by=c("id"="neighborhood")) %>%
  filter(!is.na(median_asking))

ggmap(nyc_map) + 
  geom_polygon(data=plot_rent_data, aes(x=long, y=lat, group=group, 
                                        fill=log(median_asking)), color="gray") +
  scale_fill_gradient(low="blue", high="red") +
  ggtitle("Median Asking Rent by Neighborhood")

# average entire apt/house airbnb by neighborhood
entire_with_price <- filter(listings, room_type == "Entire home/apt") %>%
  select(id, price, latitude, longitude, neighbourhood_cleansed) %>%
  mutate(neighborhood = neighbourhood_cleansed, neighbourhood_cleansed = NULL) %>%
  group_by(neighborhood) %>%
  summarize(num_listings = n(), total_price = sum(price), 
            median_price = median(price)) %>%
  mutate(avg_price = total_price/num_listings)

plot_entire_cost_data <- tidy(nyc_neighborhoods, region="neighborhood") %>%
  left_join(., entire_with_price, by=c("id"="neighborhood")) %>%
  filter(!is.na(avg_price))

ggmap(nyc_map) + 
  geom_polygon(data=plot_entire_cost_data, aes(x=long, y=lat, group=group, 
                                               fill=log(avg_price)), color="gray") +
  scale_fill_gradient(low="blue", high="red") +
  ggtitle("Median Asking Airbnb for Entire House/Apt by Neighborhood")

# median asking rent vs average airbnb
plot_rent_data_adjusted <- mutate(plot_rent_data, daily_adjusted = median_asking/28)
rent_vs_airbnb <- full_join(plot_rent_data_adjusted, plot_entire_cost_data,
                            by = "id") %>%
  filter(!is.na(median_price), !is.na(daily_adjusted), !is.na(avg_price))

ggplot(rent_vs_airbnb, aes(x = avg_price, y = daily_adjusted)) + 
  geom_point()

# plotting median airbnb price vs daily adjusted rent
ggplot(rent_vs_airbnb, aes(x = median_price, y = daily_adjusted)) + 
  geom_point() +
  scale_x_continuous(limits=c(0, 300)) +
  scale_y_continuous(limits=c(0, 300))

# plot rent month to month
complete_months <- df[complete.cases(df),]
df2 <- filter(complete_months, AreaType == "Neighborhood")
df2 <- mutate(df2, AreaType = NULL)
df2 <- melt(df2, id.vars = c("Area", "Boro"))

# graphed over one year
ggplot(df2, aes(x = as.Date(variable), y = value, group = Area, color = Area)) + 
  geom_line() +
  scale_x_date(limits = c(as.Date("2015-01-01"), as.Date("2015-12-01"))) + 
  facet_wrap(~ Area)

# whole set
ggplot(df2, aes(x = as.Date(variable), y = value, group = Area, color = Area)) + 
  geom_line() +
  scale_x_date(limits = c(as.Date("2009-03-01"), as.Date("2016-04-01"))) + 
  facet_wrap(Boro ~ Area)
