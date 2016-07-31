#to create the train and test sets for the decision trees for all listings

library(readr)

#load the listings history database

listings_history <- read_csv("../raw_data/listing_history.csv")

#create an index to use for test and train
#this will create a test set that is 20% of the listings history data

indexes <- sample(1:nrow(listings_history), 
                  size=0.2*nrow(listings_history))

#apply the indexes to listings history to generate test and train

test=listings_history[indexes, ]
train=listings_history[-indexes, ]


#save the test and train so can be loaded into future files

save(test, train, file = "test_train.RData")
