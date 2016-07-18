library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(scales)

listings1509 <- read_csv("../../raw_data/2015-09-01-listings.csv", na='\\N')
listings1510 <- read_csv("../../raw_data/2015-10-01-listings.csv", na='\\N')
listings1511 <- read_csv("../../raw_data/2015-11-01-listings.csv", na='\\N')
listings151120 <- read_csv("../../raw_data/2015-11-20-listings.csv", na='\\N') #watch out for this one! This is still November
listings1512 <- read_csv("../../raw_data/2015-12-02-listings.csv", na='\\N')
listings1601 <- read_csv("../../raw_data/2016-01-01-listings.csv", na='\\N')
listings1602 <- read_csv("../../raw_data/2016-02-02-listings.csv", na='\\N')
# No data for March 2016 :(
listings1604 <- read_csv("../../raw_data/2016-04-03-listings.csv", na='\\N') #APRIL! This is not March. March has been skipped.
listings1605 <- read_csv("../../raw_data/2016-05-02-listings.csv", na='\\N')
listings1606 <- read_csv("../../raw_data/2016-06-02-listings.csv", na='\\N')

#fix prices for listings
listings1511 <- mutate(listings1511, price = as.numeric(gsub("[$,]", "", price)), 
                       weekly_price = as.numeric(gsub("[$,]", "", weekly_price)),
                       monthly_price = as.numeric(gsub("[$,]", "", monthly_price)),
                       security_deposit = as.numeric(gsub("[$,]", "", security_deposit)),
                       cleaning_fee = as.numeric(gsub("[$,]", "", cleaning_fee)),
                       extra_people = as.numeric(gsub("[$,]", "", extra_people)))
listings151120 <- mutate(listings151120, price = as.numeric(gsub("[$,]", "", price)), 
                         weekly_price = as.numeric(gsub("[$,]", "", weekly_price)),
                         monthly_price = as.numeric(gsub("[$,]", "", monthly_price)),
                         security_deposit = as.numeric(gsub("[$,]", "", security_deposit)),
                         cleaning_fee = as.numeric(gsub("[$,]", "", cleaning_fee)),
                         extra_people = as.numeric(gsub("[$,]", "", extra_people)))

#if statement: to find if the id was purged or not
# looking at pre and post purge

prepurge <- listings1511 %>% filter(room_type == "Entire home/apt") %>% group_by(host_id) %>% mutate(host_count = n()) %>% filter(host_count > 1) %>% arrange(host_id)
View(prepurge)
nrow(prepurge) #3331

postpurge <- listings151120 %>% filter(room_type == "Entire home/apt") %>% group_by(host_id) %>% mutate(host_count = n()) %>% filter(host_count > 1) %>% arrange(host_id)
View(postpurge)
nrow(postpurge) #1829

purged_listings <- anti_join(prepurge, postpurge, by = 'id')
View(purged_listings)

purged_tf <- c()
for(i in 1:nrow(prepurge)){
  if(prepurge[i,]$id %in% purged_listings$id){
    purged_tf <- c(purged_tf, TRUE)
  } else {
    purged_tf <- c(purged_tf, FALSE)
  }
}

prepurge$purged <- purged_tf
View(prepurge)

nrow(prepurge)
prepurge <- data.frame(prepurge)

summary(purged_listings)


#create a bare prepurge file for the logistic regression
prepurge_regression <- prepurge %>% select(id, host_id, host_since, host_response_time, host_response_rate, 
host_acceptance_rate, host_is_superhost, host_neighbourhood, host_has_profile_pic, host_identity_verified, 
neighbourhood_cleansed, neighbourhood_group_cleansed, is_location_exact, property_type, accommodates, bathrooms, 
bedrooms, beds, bed_type, price, guests_included, extra_people, minimum_nights, maximum_nights, has_availability, 
availability_30, availability_60, availability_90, availability_365, number_of_reviews, first_review, last_review, 
review_scores_rating, review_scores_accuracy, review_scores_cleanliness, review_scores_checkin, review_scores_communication, 
review_scores_location, review_scores_value, instant_bookable, reviews_per_month, cancellation_policy, 
require_guest_profile_picture, require_guest_phone_verification, host_count, purged) 


prepurge_regression <- prepurge %>% select(host_since, host_response_time, host_response_rate, 
host_acceptance_rate, host_is_superhost, host_neighbourhood, host_has_profile_pic, host_identity_verified, 
neighbourhood_cleansed, neighbourhood_group_cleansed, is_location_exact, property_type, accommodates, bathrooms, 
bedrooms, beds, bed_type, price, guests_included, extra_people, minimum_nights, maximum_nights, has_availability, 
availability_30, availability_60, availability_90, availability_365, number_of_reviews, first_review, last_review, 
review_scores_rating, review_scores_accuracy, review_scores_cleanliness, review_scores_checkin, review_scores_communication, 
review_scores_location, review_scores_value, instant_bookable, reviews_per_month, cancellation_policy, 
require_guest_profile_picture, require_guest_phone_verification, host_count, purged) 

prepurge_regression$host_has_profile_pic <- as.logical(prepurge_regression$host_has_profile_pic)

#
prepurge_regression2 <- na.omit(prepurge_regression)
summary(prepurge_regression2)



prepurge_regression2 <- prepurge %>% select(host_since, host_response_time, host_response_rate, 
host_acceptance_rate, host_is_superhost, is_location_exact, property_type, accommodates, bathrooms, 
bedrooms, beds, bed_type, price, guests_included, extra_people, minimum_nights, maximum_nights, has_availability, 
availability_30, availability_60, availability_90, availability_365, number_of_reviews, first_review, last_review, 
review_scores_rating, review_scores_accuracy, review_scores_cleanliness, review_scores_checkin, review_scores_communication, 
review_scores_location, review_scores_value, instant_bookable, reviews_per_month, cancellation_policy, 
require_guest_profile_picture, require_guest_phone_verification, host_count, purged )


prepurge_regression3 <- prepurge %>% select(host_since, host_response_time, host_response_rate, host_acceptance_rate, host_is_superhost, is_location_exact, property_type, accommodates, bathrooms, bedrooms, beds, bed_type, price, guests_included, extra_people, minimum_nights, availability_30, availability_60, availability_90, availability_365, number_of_reviews, first_review, last_review, review_scores_rating, review_scores_accuracy, review_scores_cleanliness, review_scores_checkin, review_scores_communication, review_scores_location, review_scores_value, instant_bookable, reviews_per_month, cancellation_policy, require_guest_profile_picture, require_guest_phone_verification, host_count, purged)

model1 <- glm(purged ~ ., data = prepurge_regression3, family = "binomial")
summary(model1)
round(exp(coef(model1)))
stargazer(model1, type="text")

##maximum nights is causing a problem
##has availability too

model2 <- glm(purged ~ host_response_rate + host_acceptance_rate + host_is_superhost + is_location_exact + property_type + minimum_nights + first_review + review_scores_location + review_scores_value + instant_bookable + cancellation_policy + require_guest_profile_picture + require_guest_phone_verification + host_count, data = prepurge_regression3, family = "binomial")
summary(model2)

model3 <- glm(purged ~ host_acceptance_rate + host_is_superhost + is_location_exact + property_type + minimum_nights + first_review + review_scores_location + review_scores_value + instant_bookable + cancellation_policy + require_guest_profile_picture + host_count, data = prepurge_regression3, family = "binomial")
summary(model3)

prepurge_regression4 <- prepurge %>% select(host_since, host_response_time, host_response_rate, host_acceptance_rate, host_is_superhost, neighbourhood_cleansed, neighbourhood_group_cleansed, is_location_exact, property_type, accommodates, bathrooms, bedrooms, beds, bed_type, price, guests_included, extra_people, minimum_nights, maximum_nights, availability_30, availability_60, availability_90, availability_365, number_of_reviews, first_review, last_review, review_scores_rating, review_scores_accuracy, review_scores_cleanliness, review_scores_checkin, review_scores_communication, review_scores_location, review_scores_value, instant_bookable, reviews_per_month, cancellation_policy, require_guest_profile_picture, require_guest_phone_verification, host_count, purged)

model4 <- glm(purged ~., data = prepurge_regression4, family = "binomial")
summary(model4)

model5 <- glm(purged ~ host_acceptance_rate + host_is_superhost + neighbourhood_group_cleansed +is_location_exact + property_type + minimum_nights + first_review + review_scores_location + review_scores_value + instant_bookable + cancellation_policy + require_guest_profile_picture + host_count, data = prepurge_regression4, family = "binomial")
summary(model5)

model6 <- glm(purged ~ host_acceptance_rate + host_is_superhost + neighbourhood_cleansed +is_location_exact + property_type + minimum_nights + first_review + review_scores_location + review_scores_value + instant_bookable + cancellation_policy + require_guest_profile_picture + host_count, data = prepurge_regression4, family = "binomial")
summary(model6)

