#to plot the activity of listings and reviews

library(ggplot2)


#load the derived data sets
load("listings_history.RData")
load("reviewer_history.RData")

#create histogram of number of occurances of each listing in 2015

listings_history 

ggplot(data = listings_history, aes(x = total_occ_2015)) + 
  geom_histogram() +
  xlab("Total Occurences in 2015") +
  ylab("Frequency") +
  ggtitle("Frequency of Occurrences by Listing for 2015") +
  ggsave(file = "../airbnb/figures/hist_occurances_by_listing_2015.pdf")


#create the same histogram only with first seen month 2015-01
listings_history_january <- listings_history %>% 
  filter(first_seen_month == "2015-01")
ggplot(data = listings_history_january, aes(x = total_occ_2015)) + 
  geom_histogram() +
  xlab("Total Occurences in 2015") +
  ylab("Frequency") +
  ggtitle(
    "Frequency of Occurences by Listings for 2015 for January 2015 Cohort") +
  ggsave(file = 
           "../airbnb/figures/hist_occurances_by_listing_2015_jan2015cohort.pdf")


#create histogram of number of reviews of each reviewer in 2015
reviewer_data_filter <- reviewer_data %>% filter(num_within_time_period <10)
ggplot(data = reviewer_data_filter, aes(x = num_within_time_period)) + 
  geom_histogram() + 
  xlab("Number of Reviews in 2015") +
  ggtitle("Frequency of Reviews in 2015")

##have to ggsave but ask about what want to do
