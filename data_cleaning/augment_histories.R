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

# Reviews, joined to listings
reviews_and_listings <- read.csv("../raw_data/reviews_and_listings.csv.gz")

# More "raw" reviews
reviews <- read.csv('../raw_data/new-york-city-reviews.csv')


###############################################################################
## And features from where reviewers stayed for FIRST and LAST reviews
###############################################################################

review_listing_features <- reviews_and_listings %>% 
  filter(as.character(review_date) < "2016-01-01") %>%
  group_by(reviewer_id) %>%
  arrange(review_date) %>%
  summarize(
    fs_listing_id = first(listing_id),
    fs_review_date = first(review_date),
    fs_room_type = first(room_type),
    fs_mean_price = first(mean_price),
    fs_min_price = first(min_price),
    fs_max_price = first(max_price),
    fs_is_multilisting = first(is_multilisting),
    fs_host_since = first(host_since),
    fs_host_listings_count = first(host_listings_count),
    fs_first_review = first(first_review),
    fs_first_review_month_2015 = first(first_review_month_2015),
    fs_is_superhost_2015 = first(is_superhost_2015),
    
    ls_listing_id = last(listing_id),
    ls_review_date = first(review_date),
    ls_room_type = last(room_type),
    ls_mean_price = last(mean_price),
    ls_min_price = last(min_price),
    ls_max_price = last(max_price),
    ls_is_multilisting = last(is_multilisting),
    ls_host_since = last(host_since),
    ls_host_listings_count = last(host_listings_count),
    ls_first_review = last(first_review),
    ls_first_review_month_2015 = last(first_review_month_2015),
    ls_is_superhost_2015 = last(is_superhost_2015)    
  )

###############################################################################
## Save review features
###############################################################################

write_csv(review_listing_features, "../raw_data/review_listing_features.csv")


###############################################################################
## And features for listers from FIRST and MOST RECENT and AVERAGE
###############################################################################

review_features <- reviews %>%
  select(-comments) %>%
  filter(as.character(date) < "2016-01-01") %>%
  group_by(reviewer_id) %>%
  arrange(date) %>%
  mutate(
    num_reviews = n(),
    first_review_date = first(date),
    last_review_date = last(date),
    num_different_places = n_distinct(listing_id)
  ) %>%
  ungroup()
  
listing_review_features <- review_features %>%
  group_by(listing_id) %>%
  arrange(date) %>%
  summarize(
    fr_num_reviews = first(num_reviews),
    fr_first_review_date = first(first_review_date),
    fr_last_review_date = first(last_review_date),
    fr_num_different_places = first(num_different_places),
    
    lr_num_reviews = last(num_reviews),
    lr_first_review_date = last(first_review_date),
    lr_last_review_date = last(last_review_date),
    lr_num_different_places = last(num_different_places),
    
    avg_reviewer_num_reviews = mean(num_reviews),
    avg_reviewer_num_places = mean(num_different_places)
  )

# TODO: entropy of places reviewers stay
# TODO: join with Francesco's text data

###############################################################################
## Save listing review features
###############################################################################

write_csv(listing_review_features, "../raw_data/listing_review_features.csv")

