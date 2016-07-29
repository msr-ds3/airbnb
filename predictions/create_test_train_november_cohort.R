#to create the train and test sets for the decision trees for the 
#November cohort

library(readr)

#load the listings history database

listings_history <- read_csv("../raw_data/listing_history.csv")

#filter the listings for only those in the january cohort
#these listings were first seen in January 2015

listings_history_november <- listings_history %>% filter(first_seen_month == 11)

#create an index to use for test and train
#this will create a test set that is 20% of the listings history data

indexes <- sample(1:nrow(listings_history_november), 
                  size=0.2*nrow(listings_history_november))

#apply the indexes to listings history to generate test and train

nov_test=listings_history_november[indexes, ]
nov_train=listings_history_november[-indexes, ]


#save the test and train so can be loaded into future files

save(nov_test, nov_train, file = "nov_test_train.RData")
