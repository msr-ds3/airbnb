library(readr)
library(dplyr)
library(ggplot2)

load('airbnb.RData')
listings <- mutate(listings, price = as.numeric(gsub("[$,]", "", price)))




#Ungrouped plots
#looking at review scores rating
ggplot(data = listings, aes(x = review_scores_rating)) + geom_histogram() + facet_wrap(~neighbourhood_group_cleansed)
summary(listings$review_scores_rating)
  #Mean = 92.41
#review scores accuracy
summary(listings$review_scores_accuracy)
  #mean is 9.462 (out of 10)
#review scores cleanliness
summary(listings$review_scores_cleanliness)
  #mean is 9.118
#review scores location
summary(listings$review_scores_location)
  #mean 9.344
#review scores value
summary(listings$review_scores_value)
  #9.243
#review scores checkin
summary(listings$review_scores_checkin)
  #9.636
#review scores communication
summary(listings$review_scores_communication)
  #9.7

#to look at price
summary(listings$price)
  #Mean $151.60 


#to group by neighborhood
listings_by_neighborhood <- listings %>% group_by(neighbourhood_cleansed)

number_per_neighborhood <- listings_by_neighborhood %>% summarize(total = n())
summary(number_per_neighborhood$total)
#Min 1
#Mean 172.7
#Max 3289.0 (Williamsburg)

#to group by borough
listings_by_borough <- listings %>% group_by(neighbourhood_group_cleansed)

number_per_borough <- listings_by_borough %>% summarize(total = n())
#Manhattan has most, followed by Brooklyn. Least in Staten Island
summary(number_per_borough$total)
#Min 190, Max 18160

ggplot(data = listings_by_borough, aes(x = price, y = minimum_nights)) + geom_point()
#doesnt really seem to be a connection
ggplot(data = listings_by_borough, aes(x = price, y = minimum_nights)) + geom_point()+ facet_wrap(~neighbourhood_group_cleansed)
  #Manhattan had most variability
ggplot(data = listings_by_borough, aes(x = price, y = accommodates)) + geom_point()+ facet_wrap(~neighbourhood_group_cleansed)
ggplot(data = listings_by_borough, aes(x = price, y = bedrooms)) + geom_point()+ facet_wrap(~neighbourhood_group_cleansed)
ggplot(data = listings_by_borough, aes(x = price, y = bathrooms)) + geom_point()+ facet_wrap(~neighbourhood_group_cleansed)

ggplot(data = listings, aes(x = price)) + geom_histogram()+ facet_wrap(~neighbourhood_group_cleansed)
ggplot(data = listings, aes(x = accommodates, y = price)) + geom_point() + facet_wrap(~neighbourhood_group_cleansed)
#Doesn't seem to be a correlation between # accomodates and price
ggplot(data = listings, aes(x = review_scores_rating, y = price)) + geom_point() + facet_wrap(~neighbourhood_group_cleansed)



#separate by room type

listings_by_room_type <- listings %>% group_by(room_type)
ggplot(data = listings_by_room_type, aes(x = price, y = review_scores_rating)) + geom_point()+ facet_wrap(~neighbourhood_group_cleansed)
ggplot(data = listings_by_room_type, aes(x = price, y = review_scores_rating)) + geom_point()+ facet_wrap(~room_type)
ggplot(data = listings_by_room_type, aes(x = review_scores_rating)) + geom_histogram()+ facet_wrap(~host_identity_verified)


number_room_type <- listings_by_room_type %>% summarize(total = n())
View(number_room_type)  
#Max: entire apartment = 19072
#Min: shared room = 1094

#separate by property type

listings_by_property_type <- listings %>% group_by(property_type)
number_property_type <- listings_by_property_type %>% summarize(total = n())
View(number_property_type)
#Max apartment 32127
#Min Castle, Chalet, Earth house

#Find out types of property in each borough
property_by_borough <- listings %>% group_by(neighbourhood_group_cleansed, room_type) %>% summarize(total = n())
View(property_by_borough)
ggplot(property_by_borough, aes(x = room_type, y = total)) + geom_point() + facet_wrap(~neighbourhood_group_cleansed)



###July 12
#try and look at number of unique urls
length(unique(listings$picture_url))
#36595
#Difference between total listings and unique urls
36608-36595
#There are 13 nonunique entries
listings_duplicate <- listings
listings_duplicate$duplicate_url <- as.numeric(duplicated(listings$picture_url))
listings_url_duplicate <- listings_duplicate %>% filter(duplicate_url ==1)
View(listings_url_duplicate)


#try and look and number of unique host picture url
length(unique(listings$host_picture_url))
  #30162
picture_duplicate <- listings
picture_duplicate$duplicate_pic <- as.numeric(duplicated(listings$host_picture_url))
listings_picture_duplicate <- picture_duplicate %>% filter(duplicate_pic ==1)
View(listings_picture_duplicate)
summary(listings_picture_duplicate)


#unique summaries
length(unique(listings$summary))
#33437
36608-33437
#3137 match
 

summary_duplicate$duplicate_summary <- as.numeric(duplicated(listings$summary))
listings_summary_duplicate <- summary_duplicate %>% filter(duplicate_summary ==1)
View(listings_summary_duplicate)
#need to figure out how to remove the NAs
#maybe duplicated:
#https://www.airbnb.com/rooms/761562
#https://www.airbnb.com/rooms/792748



#available for full year ahead
listings_yearly <- listings %>% filter(availability_365 == 365)
listings_booked <- listings %>% filter(availability_365 != 365)
summary(listings_yearly$price)
  #range 10 to 9999, mean 190.80
summary(listings_booked$price)
  #range 10 to 10,000, mean 149.1
summary(listings_yearly$number_of_reviews)
  #range 0 to 136, mean 4.36 
summary(listings_booked$number_of_reviews)
  #range 0 to 324, mean 13.76
summary(listings_yearly$review_scores_rating)
  #range 20 to 100 (1039 NAs) mean 90.71
summary(listings_booked$review_scores_rating)
  #range 20 to 100 (8767 NAs) mean 92.49

#Day 3
##Morning Meeting

##40.76400 -73.98908
##40.76178 -73.99049
#find a review prior to  2016-04-02


##after chris helped me, he suggest a better way to sort my listings
duplicate_summary_host <- listings %>% filter(summary != "" & summary != "." & room_type == "Entire home/apt") %>% group_by(summary) %>% mutate(host_count=n_distinct(host_id)) %>% ungroup() %>% filter(host_count > 1) %>% arrange(summary, host_since)
#only entire apartments with duplicate summaries
duplicate_summary <- listings %>% filter(summary != "" & summary != "." & room_type =="Entire home/apt") %>% group_by(summary) %>% mutate(summary_count=n()) %>% ungroup() %>% filter(summary_count>1) %>% arrange(host_name)
View(duplicate_summary)


#look at ej's calendar for one listing and compare
calendar_ej <- calendar %>% filter(listing_id == 9795028 | listing_id == 10641378) %>% filter(available == "t") 
View(calendar_ej)
