#to create the train and test sets for the decision trees for the 
#June cohort for the skimmed listings data

library(readr)
library(dplyr)

#load the listings history database

load("skimmed_listings_history.RData") 
##created by file = "create_listings_history_skimmed.R"

#filter the listings for only those in the June cohort
#these listings were first seen in June 2015

listings_history_june <- skimmed_listings_history %>% 
  filter(first_seen_month == 6)

#create an index to use for test and train
#this will create a test set that is 20% of the listings history data

indexes <- sample(1:nrow(listings_history_june), 
                  size=0.2*nrow(listings_history_june))

#apply the indexes to listings history to generate test and train

skimmed_june_test=listings_history_june[indexes, ]
skimmed_june_train=listings_history_june[-indexes, ]


#save the test and train so can be loaded into future files

save(skimmed_june_test, skimmed_june_train, file = "skimmed_june_test_train.RData")
