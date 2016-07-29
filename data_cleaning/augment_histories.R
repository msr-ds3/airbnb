# DS3, AirBnB Analysis Team
# 2016-07-27

# This file adds additional features to the listings_history file. The goal is 
# to use some of this features in our prediction of future renter / host 
# behavior.

library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(scales)


###############################################################################
## Load in data
###############################################################################

# Listings history
# Note: this assumes listing_history.csv exists and is in the raw_data 
#  directory. You may need to move this file there.
listings_history <- read.csv("../raw_data/listing_history.csv")

# Reviewer data
# We only have NYC listings, so we don't need other reviews (below).
# review_files <- c('../raw_data/barcelona-reviews.csv',
#                   '../raw_data/copenhagen-reviews.csv',
#                   '../raw_data/los-angeles-reviews.csv',
#                   '../raw_data/berlin-reviews.csv',
#                   '../raw_data/listing_history.csv',
#                   '../raw_data/new-york-city-reviews.csv',
#                   '../raw_data/san-francisco-reviews.csv',
#                   '../raw_data/chicago-reviews.csv',
#                   '../raw_data/london-reviews.csv',
#                   '../raw_data/paris-reviews.csv',
#                   '../raw_data/sydney-reviews.csv')

review_files <- c('../raw_data/new-york-city-reviews.csv')
reviews <- data.frame()
for (f in review_files) {
  reviews <- rbind(reviews, read.csv(review_files))
}


###############################################################################
## Find out where reviewers stayed for first review
###############################################################################

# How many reviewers are there?
reviews %>% summarize(n_distinct(reviewer_id))

# Add a review number for each review, for each reviewer
reviews <- reviews %>% 
  group_by(reviewer_id) %>%
  arrange(date) %>%
  mutate(review_number = row_number()) %>%
  ungroup()

first_reviews <- reviews %>% filter(review_number == 1)
  
# Check that length of first_reviews is same as number of reviewers..
# Should be TRUE
(first_reviews %>% nrow) == reviews %>% summarize(n_distinct(reviewer_id))


###############################################################################
## Combine first review data with listings data 
###############################################################################

df <- left_join(reviews, listings_history, by="listing_id")

df <- df %>% 
  rename(review_id = id,
         review_date = date) %>%
  select(-text_2015)


###############################################################################
## Save!
###############################################################################

write_csv(df, "../raw_data/reviews_and_listings.csv")


###############################################################################
## Analysis (should probably delete or move)
###############################################################################

num_reviews <- reviews %>%
  group_by(reviewer_id) %>%
  summarize(num_stays = n(),
            repeat_customer = num_stays > 1)

# Should change to left join and filter on NAs... TODO
foo <- right_join(features_reviews, num_reviews)

foo %>% 
  group_by(first_stay_w_super_2015) %>% 
  summarize(mean(repeat_customer))

foo <- filter(foo, !is.na(first_stay_w_super_2015))

baz <- glm(repeat_customer ~ first_stay_w_super_2015, data=foo, family="binomial")
