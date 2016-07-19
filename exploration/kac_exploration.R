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

# load airbnb RData into dataframes if available
load("airbnb.RData")
load("supplementary_data.RData")

#load and prep crime data
crime_data <- read_csv("NYPD_7_Major_Felony_Incident_Map.csv", na='\\N')
crime_data <- crime_data %>% extract(`Location 1`, c("Latitude", "Longitude"), "\\(([^,]+), ([^)]+)\\)")
crime_data <- mutate(crime_data, Latitude = as.numeric(Latitude), Longitude = as.numeric(Longitude))
recent_crime_data <- filter(crime_data, `Occurrence Year` == 2015 | `Occurrence Year` == 2014 )

#load neighborhood population data
population_data <- read_csv("New_York_City_Population_By_Neighborhood_Tabulation_Areas.csv", na='\\N')
population_data <- filter(population_data, Year == 2010)
names(population_data)[names(population_data) == 'NTA Name'] <- 'NTAName'

#load neighborhood noise complaint data
noise_data <- read_csv("311_Noise_complaints_09_2015_SE.csv", na='\\N')
noise_data <- select(noise_data, `Unique Key`, `Created Date`, `Agency Name`, 
                     `Complaint Type`, `Descriptor`, `Incident Zip`, `City`, `Borough`, 
                     `X Coordinate (State Plane)`, `Y Coordinate (State Plane)`, `Latitude`, 
                     `Longitude`, `Location`)

# save to RData
save(recent_crime_data, population_data, noise_data, file = "supplementary_data.RData")

#price by neighborhood
listings %>% group_by(zipcode) %>% ggplot(aes(x = neighbourhood, y = price)) + geom_point() + scale_y_log10()

#num of reviews by price
ggplot(listings, aes (x = number_of_reviews, y = price)) + geom_point()

#minimum length of stay by borough for entire  home/apt
listings %>% filter(room_type == "Entire home/apt") %>% 
  ggplot(aes(x = neighbourhood_group_cleansed , y = minimum_nights)) + geom_point() + scale_y_log10()

#Entire home/apt listings per borough
listings %>% filter(room_type == "Entire home/apt") %>% group_by(neighbourhood_cleansed) %>% 
  ggplot(aes(x = neighbourhood_group_cleansed)) + geom_bar()

#general listings per borough
listings %>% group_by(neighbourhood_cleansed) %>% ggplot(aes(x = neighbourhood_group_cleansed)) + geom_bar()


#average location review
reviewByNeigh <- listings %>% group_by(neighbourhood_group_cleansed) %>% 
  summarise(avgLocReview = mean(review_scores_location, na.rm = TRUE), numlistings = n())
reviewByNeigh %>% ggplot(aes(x = neighbourhood_group_cleansed, y = avgLocReview)) + geom_point()

#average price per neigh
priceByNeigh <- listings %>% group_by(neighbourhood_group_cleansed) %>% summarise(avgPrice= mean(price, na.rm = TRUE))
priceByNeigh %>% ggplot(aes(x = neighbourhood_group_cleansed, y = avgPrice)) + geom_point()  

#listings near mass transit
transitListings <- filter(listings, grepl('subway|train|station|trains|subways|stations', transit, ignore.case = TRUE))

#comparing prices, num reviews ratings btwn host and superhost
ggplot(listings, aes (x = number_of_reviews, y = price, color = is_host_superhost)) + geom_point() + scale_y_log10()
filter(listings, is_host_superhost == "t") %>% summarise(avgPrice= mean(price, na.rm = TRUE), 
      avgRating= mean(review_scores_rating, na.rm = TRUE), avgNumRatings = mean(number_of_reviews, na.rm = TRUE))
filter(listings, is_host_superhost == "f") %>% summarise(avgPrice= mean(price, na.rm = TRUE), 
       avgRating= mean(review_scores_rating, na.rm = TRUE), avgNumRatings = mean(number_of_reviews, na.rm = TRUE))

#listings near mass transit
listings <- mutate(listings, hasTransit = grepl('subway|train|station|trains|subways|stations', transit, ignore.case = TRUE))

#affect of mass transit on price and number of reviews
ggplot(listings, aes (x = number_of_reviews, y = price, color = hasTransit)) + geom_boxplot() + scale_y_log10()
filter(listings, grepl('subway|train|station|trains|subways|stations', transit, ignore.case = TRUE)) %>% 
  summarise(avgPrice= mean(price, na.rm = TRUE), avgRating= mean(review_scores_rating, na.rm = TRUE), 
  avgNumRatings = mean(number_of_reviews, na.rm = TRUE))
  #hasTransit = TRUE
  #avgPrice   avgRating   avgNumRatings
  #146.0276   92.64457      15.51466
filter(listings, !grepl('subway|train|station|trains|subways|stations', transit, ignore.case = TRUE)) %>% 
  summarise(avgPrice= mean(price, na.rm = TRUE), avgRating= mean(review_scores_rating, na.rm = TRUE), 
  avgNumRatings = mean(number_of_reviews, na.rm = TRUE))
  #hasTransit = FALSE
  #avgPrice avgRating avgNumRatings
  #159.4239  91.98864      9.920217

#affect of hotels proximity on price

#affect of attractions on price

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

###############NEIGHBORHOOD TABULATION AREAS#################################

#get shapefile for nyc neighborhood tabulation areas
r <- GET('http://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services/nynta/FeatureServer/0/query?where=1=1&outFields=*&outSR=4326&f=geojson')
nyc_neighborhoods <- readOGR(content(r,'text'), 'OGRGeoJSON', verbose = F)
nyc_neighborhoods_df <- tidy(nyc_neighborhoods)
nyc_map <- get_map(location = c(lon = -74.00, lat = 40.71), maptype = "terrain", zoom = 10)

#CRIME
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
  group_by(NTAName) %>%
  summarize(num_crimes=n())

#join population to crimes
crimes_by_neighborhood <- inner_join(crimes_by_neighborhood, population_data, by="NTAName")

#calculate crime rate per 1000 residents
crimes_by_neighborhood <- mutate(crimes_by_neighborhood, crime_rate= (1000*(num_crimes/Population)))

#create crime plot data
plot_crime_data <- tidy(nyc_neighborhoods, region="NTAName") %>%
  left_join(., crimes_by_neighborhood, by=c("id"="NTAName")) %>%
  filter(!is.na(crime_rate))


#map of crime rate by neighborhood
ggmap(nyc_map) + 
  geom_polygon(data=plot_crime_data, aes(x=long, y=lat, group=group, fill=log(crime_rate))) + 
  scale_fill_gradient(low = "blue", high = "red")
  


#SAFE
#create True/False column for Safe keyword usage
listings <- mutate(listings, safe_keyword = grepl('safe', name, ignore.case = TRUE)
                   |grepl('safe', summary, ignore.case = TRUE)
                   |grepl('safe', space, ignore.case = TRUE)
                   |grepl('safe', description, ignore.case = TRUE)
                   |grepl('safe', neighborhood_overview, ignore.case = TRUE))

#grab latitudes and longitudes
safe_locations <- data.frame(latitude=listings$latitude, longitude=listings$longitude, safe_keyword = listings$safe_keyword)

#Separate listings by safe keyword usage
safe_listings <- filter(safe_locations, safe_keyword == TRUE) 
no_safe_listings <- filter(safe_locations, safe_keyword == FALSE) 

#convert safe and no safe listings long and lat to coordinates and convert them to projection of ny shapefile
safe_listings <- data.frame(latitude=safe_listings$latitude, longitude=safe_listings$longitude)
safe_listings<- safe_listings[complete.cases(safe_listings),]
safe_listings_spdf <- safe_listings 
coordinates(safe_listings_spdf) <- ~longitude + latitude 
proj4string(safe_listings_spdf) <- proj4string(nyc_neighborhoods) 

no_safe_listings <- data.frame(latitude=no_safe_listings$latitude, longitude=no_safe_listings$longitude)
no_safe_listings<- no_safe_listings[complete.cases(no_safe_listings),]
no_safe_listings_spdf <- no_safe_listings 
coordinates(no_safe_listings_spdf) <- ~longitude + latitude 
proj4string(no_safe_listings_spdf) <- proj4string(nyc_neighborhoods) 

#bind long and lat of safe and no_safe to neighborhoods
safe_matches <- over(safe_listings_spdf, nyc_neighborhoods)
safe_listings <- cbind(safe_listings, safe_matches)

no_safe_matches <- over(no_safe_listings_spdf, nyc_neighborhoods)
no_safe_listings <- cbind(no_safe_listings, no_safe_matches)

#get sum of safe/no safe per neighborhood and join dataframes
safe_by_neighborhood <- safe_listings %>%
  group_by(NTAName) %>%
  summarize(num_safe=n())

no_safe_by_neighborhood <- no_safe_listings %>%
  group_by(NTAName) %>%
  summarize(num_no_safe=n())

#join safe and no safe dataframes by Neighborhood
safe_by_neighborhood1 <- inner_join(safe_by_neighborhood, no_safe_by_neighborhood, by="NTAName")

#get total num of listings per neighborhood
safe_by_neighborhood1 <- mutate(safe_by_neighborhood1, total = (num_no_safe+num_safe))

#eliminate neighborhoods with less than 10 overall listings
safe_by_neighborhood1 <- filter(safe_by_neighborhood1, total > 10)


#create safe plot data
plot_safe_data <- tidy(nyc_neighborhoods, region="NTAName") %>%
  left_join(., safe_by_neighborhood1, by=c("id"="NTAName")) %>%
  filter(!is.na(total))


#map of safe listings ratio by neighborhood
ggmap(nyc_map) + 
  geom_polygon(data=plot_safe_data, aes(x=long, y=lat, group=group, fill=(num_safe/total))) + 
  scale_fill_gradient(low = "red", high = "blue")




#####QUIET#####

#create True/False column for Quiet keyword usage
listings <- mutate(listings, quiet_keyword = grepl('quiet', name, ignore.case = TRUE)
                   |grepl('quiet', summary, ignore.case = TRUE)
                   |grepl('quiet', space, ignore.case = TRUE)
                   |grepl('quiet', description, ignore.case = TRUE)
                   |grepl('quiet', neighborhood_overview, ignore.case = TRUE))

#grab latitudes and longitudes
quiet_locations <- data.frame(latitude=listings$latitude, longitude=listings$longitude, quiet_keyword = listings$quiet_keyword)

#Separate quiet listings from other listings
quiet_listings <- filter(quiet_locations, quiet_keyword == TRUE) 
no_quiet_listings <- filter(quiet_locations, quiet_keyword == FALSE) 

#convert listings long and lat to coordinates and convert them to projection of ny shapefile
quiet_listings <- data.frame(latitude=quiet_listings$latitude, longitude=quiet_listings$longitude)
quiet_listings<- quiet_listings[complete.cases(quiet_listings),]
quiet_listings_spdf <- quiet_listings 
coordinates(quiet_listings_spdf) <- ~longitude + latitude 
proj4string(quiet_listings_spdf) <- proj4string(nyc_neighborhoods) 

no_quiet_listings <- data.frame(latitude=no_quiet_listings$latitude, longitude=no_quiet_listings$longitude)
no_quiet_listings<- no_quiet_listings[complete.cases(no_quiet_listings),]
no_quiet_listings_spdf <- no_quiet_listings 
coordinates(no_quiet_listings_spdf) <- ~longitude + latitude 
proj4string(no_quiet_listings_spdf) <- proj4string(nyc_neighborhoods) 

#map long and lat of quiet and no_quiet to neighborhoods
quiet_matches <- over(quiet_listings_spdf, nyc_neighborhoods)
quiet_listings <- cbind(quiet_listings, quiet_matches)

no_quiet_matches <- over(no_quiet_listings_spdf, nyc_neighborhoods)
no_quiet_listings <- cbind(no_quiet_listings, no_quiet_matches)

#get sum of quiet vs no quiet listings per neighborhood and join dataframes
quiet_by_neighborhood <- quiet_listings %>%
  group_by(NTAName) %>%
  summarize(num_quiet=n())

no_quiet_by_neighborhood <- no_quiet_listings %>%
  group_by(NTAName) %>%
  summarize(num_no_quiet=n())

#join quiet and no quiet dataframes and get total listings by neighborhood
quiet_by_neighborhood1 <- inner_join(quiet_by_neighborhood, no_quiet_by_neighborhood, by="NTAName")
quiet_by_neighborhood1 <- mutate(quiet_by_neighborhood1, total = (num_no_quiet+num_quiet))

#eliminate neighborhoods with less than 10 overall listings
quiet_by_neighborhood1 <- filter(quiet_by_neighborhood1, total > 10)


#create quiet plot data
plot_quiet_data <- tidy(nyc_neighborhoods, region="NTAName") %>%
  left_join(., quiet_by_neighborhood1, by=c("id"="NTAName")) %>%
  filter(!is.na(total))


#map of quiet listings ratio by neighborhood
ggmap(nyc_map) + 
  geom_polygon(data=plot_quiet_data, aes(x=long, y=lat, group=group, fill=(num_quiet/total))) + 
  scale_fill_gradient(low = "red", high = "blue")




#NOISE
#convert noise long and lat to coordinates and convert them to projection of ny shapefile
noise_locations <- data.frame(latitude=noise_data$Latitude, longitude=noise_data$Longitude)
noise_locations<- noise_locations[complete.cases(noise_locations),]
noise_locations_spdf <- noise_locations 
coordinates(noise_locations_spdf) <- ~longitude + latitude 
proj4string(noise_locations_spdf) <- proj4string(nyc_neighborhoods) 

#map long and lat of noise complaints to neighborhoods
noise_matches <- over(noise_locations_spdf, nyc_neighborhoods)
noise_locations <- cbind(noise_locations, noise_matches)


#get sum of noise complaints per neighborhood and join dataframes
noise_by_neighborhood <- noise_locations %>%
  group_by(NTAName) %>%
  summarize(num_noise=n())

#join population to noise_complaints
noise_by_neighborhood <- inner_join(noise_by_neighborhood, population_data, by="NTAName")

#calculate noise complaint rate per 1000 residents
noise_by_neighborhood <- mutate(noise_by_neighborhood, noise_rate= (1000*(num_noise/Population)))

#create noise plot data
plot_noise_data <- tidy(nyc_neighborhoods, region="NTAName") %>%
  left_join(., noise_by_neighborhood, by=c("id"="NTAName")) %>%
  filter(!is.na(noise_rate))

#map of noise complaints by neighborhood
ggmap(nyc_map) + 
  geom_polygon(data=plot_noise_data, aes(x=long, y=lat, group=group, fill=log(noise_rate))) + 
  scale_fill_gradient(low = "blue", high = "red")



#QUIET VS NOISE
#join quiet and noise dataframes
n_vs_q <- inner_join(noise_by_neighborhood, quiet_by_neighborhood1, by="NTAName")
n_vs_q <- mutate(n_vs_q, quiet_ratio= (num_quiet/total))

#correlation between noise complaint rate and quiet ratio
cor(n_vs_q$noise_rate, n_vs_q$quiet_ratio)

#order dataframe by quiet ratio value
n_vs_q$NTAName <- factor(n_vs_q$NTAName, levels = n_vs_q$NTAName[order(n_vs_q$quiet_ratio)])

#plot quiet ratio and noise rate by neighborhood
ggplot(n_vs_q, aes(NTAName)) +
  geom_point(aes(y=(quiet_ratio*10), color="quiet_ratio")) +
  geom_point(aes(y=log(noise_rate), color = "noise_rate"))

#CRIME VS SAFE
#join crime and safe dataframes
c_vs_s <- inner_join(crimes_by_neighborhood, safe_by_neighborhood1, by="NTAName")

#calculate safe to no safe ratio
c_vs_s <- mutate(c_vs_s, safe_ratio= (num_safe/total))

#correlation between crime rate and safe ratio
cor(c_vs_s$crime_rate, c_vs_s$safe_ratio)

#order dataframe by safe ratio value
c_vs_s$NTAName <- factor(c_vs_s$NTAName, levels = c_vs_s$NTAName[order(c_vs_s$safe_ratio)])

#plot safe ratio and crime rate by neighborhood
ggplot(c_vs_s, aes(NTAName)) +
  geom_point(aes(y=(safe_ratio*10), color="safe_ratio")) +
  geom_point(aes(y=log(crime_rate), color = "crime_rate"))

