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

#to find multilistings 
multilistings <- listings %>% filter(room_type == "Entire home/apt") %>% group_by(host_id) %>% mutate(host_count = n()) %>% filter(host_count > 1) %>% arrange(host_id)
nrow(multilistings)
#1730 multilistings
summary(multilistings$host_count)
#Min 2, Max 28, Mean 3.5  

#plot of number of multilistings per borough (separated by number per host)

ggplot(data = multilistings, aes(x = host_count)) + geom_histogram() +facet_wrap(~neighbourhood_group_cleansed)

#create a table with just entire apt/hm
listings_entire_apt <- listings %>% filter(room_type == "Entire home/apt")


#percentage of multilistings
nrow(multilistings)/ nrow(listings_entire_apt) * 100
#9.079%

###To Calculate % of multilistings
#to create table with just entire apartment and home
listings_entire_apt <- listings %>% filter(room_type == "Entire home/apt")
#to create table of multilistings
multilistings <- listings %>% filter(room_type == "Entire home/apt") %>% group_by(host_id) %>% mutate(host_count = n()) %>% filter(host_count > 1) %>% arrange(host_id)
#to find percentage of multilistings
nrow(multilistings)/ nrow(listings_entire_apt) * 100


#number of people that have entire apt listed and min days <= 30
multilistings_less30 <- multilistings %>% filter(minimum_nights <= 30)
nrow(multilistings_less30) / nrow(multilistings)
#99.88% of postings are illegal

#number of multilistings by borough
boro_total_multilistings <- multilistings %>% ungroup() %>% group_by(neighbourhood_group_cleansed) %>% summarize(multilistings_per_borough = n()) 
boro_total_multilistings_less30 <- multilistings_less30 %>% ungroup() %>% group_by(neighbourhood_group_cleansed) %>% summarize(multilistings_per_borough_less30 = n())
multilistings_by_borough <- inner_join(boro_total_multilistings, boro_total_multilistings_less30, by = "neighbourhood_group_cleansed")
multilistings_by_borough$percentage_multilistings <- (multilistings_by_borough$multilistings_per_borough_less30/multilistings_by_borough$multilistings_per_borough)

#number of multilistings by neighborhood
neigh_total_multilistings <- multilistings %>% ungroup() %>% group_by(neighbourhood_cleansed) %>% summarize(multilistings_per_neigh = n()) 
neigh_total_multilistings_less30 <- multilistings_less30 %>% ungroup() %>% group_by(neighbourhood_cleansed) %>% summarize(multilistings_per_neigh_less30 = n())
multilistings_by_neigh <- inner_join(neigh_total_multilistings, neigh_total_multilistings_less30, by = "neighbourhood_cleansed")
multilistings_by_neigh$percentage_multilistings <- (multilistings_by_neigh$multilistings_per_neigh_less30/multilistings_by_neigh$multilistings_per_neigh)



#number of people that have entire apt listed and min days <= 30
multilistings_less30 <- multilistings %>% filter(minimum_nights <= 30)
nrow(multilistings_less30) / nrow(multilistings)
#99.88% of postings are illegal


#matching summaries and matching 
duplicate_summary_host <- listings %>% filter(summary != "" & summary != "." & room_type == "Entire home/apt") %>% group_by(summary) %>% mutate(host_count=n_distinct(host_id)) %>% ungroup() %>% filter(host_count > 1) %>% arrange(summary, host_since)
View(duplicate_summary_host)
nrow(duplicate_summary_host)
#120 
n_distinct(duplicate_summary_host$host_since)
#110 distinct start dates

#find matching summaries and host since
duplicate_summary_host <- listings %>% filter(summary != "" & summary != "." & room_type == "Entire home/apt") %>% group_by(summary) %>% mutate(host_count=n_distinct(host_id)) %>% ungroup() %>% filter(host_count > 1) %>% arrange(summary, host_since)
duplicate_host_date <- listings %>% filter(summary != "" & summary != "." & room_type == "Entire home/apt") %>% group_by(summary) %>% mutate(host_count=n_distinct(host_id)) %>% ungroup() %>% filter(host_count > 1) %>% group_by(host_since)%>% mutate(host_count_date=n()) %>% ungroup() %>% filter(host_count_date > 1) %>% group_by(host_id)
duplicate_host_date <- listings %>% group_by(host_id)
duplicate_summary_host_date <-  listings %>% filter(summary != "" & summary != "." & room_type == "Entire home/apt") %>% group_by(summary) %>% mutate(host_count=n_distinct(host_id)) %>% ungroup() %>% filter(host_count > 1) %>% arrange(summary, host_since)%>% group_by(host_since) %>% mutate(host_since_count = n_distinct(host_since)) %>% filter(host_since_count > 1)
View(duplicate_summary_host_date)


#Wordclouds
install.packages("wordcloud")
install.packages("tm")
library(wordcloud)
library(tm)
#wordcloud of listings name
wordcloud(listings$name)
wordcloud(listings$summary)
wordcloud(listings$space)
wordcloud(listings$description)
