#to create the train and test sets for the decision trees for the 
#November cohort for the skimmed listings data

library(readr)

#load the listings history database

load("../skimmed_listings_history.RData") 
##created by file = "create_listings_history_skimmed.R"

#filter the listings for only those in the November cohort
#these listings were first seen in November 2015

listings_history_november <- skimmed_listings_history %>% 
  filter(first_seen_month == 11)

#create an index to use for test and train
#this will create a test set that is 20% of the listings history data

indexes <- sample(1:nrow(listings_history_november), 
                  size=0.2*nrow(listings_history_november))

#apply the indexes to listings history to generate test and train

skimmed_nov_test=listings_history_november[indexes, ]
skimmed_nov_train=listings_history_november[-indexes, ]


#save the test and train so can be loaded into future files

save(skimmed_nov_test, skimmed_nov_train, file = "skimmed_nov_test_train.RData")
