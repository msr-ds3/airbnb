#to create the train and test sets for the decision trees for the 
#January cohort

library(readr)

#load the listings history database

listings_history <- read_csv("../raw_data/listing_history.csv")

#filter the listings for only those in the january cohort
#these listings were first seen in January 2015

listings_history_january <- listings_history %>% filter(first_seen_month == 1)

#create an index to use for test and train
#this will create a test set that is 20% of the listings history data

indexes <- sample(1:nrow(listings_history_january), 
                  size=0.2*nrow(listings_history_january))

#apply the indexes to listings history to generate test and train

jan_test=listings_history_january[indexes, ]
jan_train=listings_history_january[-indexes, ]


#save the test and train so can be loaded into future files

save(jan_test, jan_train, file = "jan_test_train.RData")
