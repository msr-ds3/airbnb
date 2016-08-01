#to plot the activity of listings and reviews

library(ggplot2)


#load the derived data sets
listings_history <- read_csv("../raw_data/listing_history.csv")

reviews <- read_csv("../raw_data/us_rev_data.csv", na='\\N')

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
  filter(host_since >= "2015-01-01" & host_since <= "2015-01-31") %>% 
  filter(purged == FALSE) %>% group_by(host_id.x) %>% 
  filter(row_number() == sample(1:row_number(), 1)) 
ggplot(data = listings_history_january, aes(x = total_occ_2015)) + 
  geom_histogram() +
  xlab("Total Occurrences in 2015") +
  ylab("Frequency") +
  ggtitle(
    "Frequency of Occurrences by Listings for 2015 for January 2015 Cohort") +
  scale_x_continuous(breaks=0:9) + 
  ggsave(file = 
           "../figures/occurrences_by_listing_2015_jancohort.png", width = 7)


#create histogram of number of reviews of each reviewer in 2015
ggplot(data = reviews, aes(x = num_in_2015)) + 
  geom_histogram() + 
  xlab("Number of Reviews in 2015") +
  ggtitle("Frequency of Reviews in 2015") +
  ggsave(file = 
           "../figures/us_reviews_2015.pdf")


#to see the number that have less than 6
reviews_under10 <- reviews %>% filter(num_in_2015 <=6)
ggplot(data = reviews_under10, aes(x = num_in_2015)) + 
  geom_histogram() + 
  xlab("Number of Reviews in 2015") +
  ggtitle("Frequency of Reviews in 2015") + 
  ggsave(file = 
           "../figures/us_reviews_under_6_2015.pdf")
##have to ggsave but ask about what want to do
