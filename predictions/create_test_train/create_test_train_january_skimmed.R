#to create the train and test sets for the decision trees for the 
#January cohort for the skimmed listings history

library(readr)

#load the listings history database skimmed

load("../skimmed_listings_history.RData") 
##created by file = "create_listings_history_skimmed.R"

#filter the listings for only those in the january cohort
#these listings were first seen in January 2015

listings_history_january <- skimmed_listings_history %>% filter(first_seen_month == 1)

#create an index to use for test and train
#this will create a test set that is 20% of the listings history data

indexes <- sample(1:nrow(listings_history_january), 
                  size=0.2*nrow(listings_history_january))

#apply the indexes to listings history to generate test and train

skimmed_jan_test=listings_history_january[indexes, ]
skimmed_jan_train=listings_history_january[-indexes, ]


#save the test and train so can be loaded into future files

save(skimmed_jan_test, skimmed_jan_train, file = "skimmed_jan_test_train.RData")
