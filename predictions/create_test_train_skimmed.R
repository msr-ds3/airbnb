#to create the train and test sets for the decision trees for all listings
#skimmed : only one random listing per host

library(readr)

#load the listings history database

load("skimmed_listings_history.RData") 
##created by file = "create_listings_history_skimmed.R"

#create an index to use for test and train
#this will create a test set that is 20% of the listings history data

indexes <- sample(1:nrow(skimmed_listings_history), 
                  size=0.2*nrow(skimmed_listings_history))

#apply the indexes to listings history to generate test and train

skimmed_test=skimmed_listings_history[indexes, ]
skimmed_train=skimmed_listings_history[-indexes, ]


#save the test and train so can be loaded into future files

save(skimmed_test, skimmed_train, file = "skimmed_test_train.RData")
 