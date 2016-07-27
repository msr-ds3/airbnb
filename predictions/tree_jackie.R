#install.packages("rpart")
#install.packages("rpart.plot")
#install.packages("ROCR")

library(rpart)
library(rpart.plot)
library(readr)
library(dplyr)
library(ROCR)

#load the listings history dataframe

listings_history <- read_csv("../raw_data/listing_history.csv")
View(listings_history)

#to remove the duplicate listing_id column
listings_history[,78] <- NULL

#to replace NA's with False
listings_history$has_reviews_2016[is.na(listings_history$has_reviews_2016)] <- FALSE

View(listings_history)


#create a bare data frame with just what you need for the tree
listings_history_tree <- listings_history[,3:93]
listings_history_tree$has_reviews_2016 <- listings_history$has_reviews_2016

#create test and train set
indexes <- sample(1:nrow(listings_history_tree), 
                  size=0.2*nrow(listings_history_tree))
listings_history_test=listings_history_tree[indexes, ]
listings_history_train=listings_history_tree[-indexes, ]

listings_history_sample <- listings_history_tree[1:1000,]

#begin with a small cp
set.seed(123)
tree <- rpart(has_reviews_2016 ~ property_type + room_type + mean_price + 
                min_price + max_price + is_multilisting + 
                host_listings_count + host_duration + first_seen_month + 
                last_seen_month + listing_recency_2015_weeks + scrap_duration +
                total_occ_2015 + first_review_year + last_review_year + 
                reviews_total_2015 + num_reviews_2015 + has_reviews_2015 + 
                first_review_month_2015 + last_review_month_2015 + 
                review_recency_2015_weeks + last_rating + is_superhost_2015 +
                is_superhost_count_2015 + TV + Internet + Wireless.Internet +
                Air.Conditioning + Kitchen + Heating + Family.Kid.Friendly + 
                Smoke.Detector + Carbon.Monoxide.Detector + Essentials +
                Shampoo + Cable.TV + Free.Parking.on.Premises + Breakfast +
                Pets.live.on.this.property + Dog.s. + First.Aid.Kit + 
                Buzzer.Wireless.Intercom + Washer + Dryer + Pets.Allowed + 
                Gym + Safety.Card + Fire.Extinguisher + Wheelchair.Accessible +
                Cat.s. + Indoor.Fireplace + Suitable.for.Events + Doorman + 
                Hot.Tub + Elevator.in.Building + Pool + Smoking.Allowed + 
                Other.pet.s. + Washer...Dryer + Lock.on.Bedroom.Door + 
                X24.Hour.Check.in + Hangers + Hair.Dryer + Iron + 
                Laptop.Friendly.Workspace + 
                translation.missing..en.hosting_amenity_49 + 
                translation.missing..en.hosting_amenity_50 + email + phone +
                facebook + reviews + kba + google + jumio + sent_id + linkedin +
                manual_offline + manual_online + weibo + photographer + None +
                amex + verifications_count ,
              data = listings_history_sample, control = rpart.control(maxdepth = 5))

#pick tree size that minimizes misclassification error (prediction error)
#prediction error rate in training data = root node error * rel error * 100%
#prediction error rate in cv = root node error * xerror * 100%

printcp(tree)
bestcp <- tree$cptable[which.min(tree$cptable[,"xerror"]), "CP"]
#prune tree using best cp
tree_pruned <- prune(tree, cp = bestcp)


#plots
plot(tree_pruned, uniform = TRUE)
text(tree_pruned, cex = 0.8, use.n = TRUE, xpd = TRUE)

prp(tree_pruned, faclen = 0, cex = 0.8, extra = 1)
#faclen = 0 : use full names of the factor labels
#extra = 1: adds number of observations at each node; equivalent to using
#use.n = TRUE in plot.rpart

#to plot on the tree how many people died v. survived (not a proportions)
tot_count <- function(x, labs, digits, varlen)
{
  paste(labs, "\n\nn =", x$frame$n)
}

prp(tree_pruned, faclen = 0, cex = 0.8, node.fun=tot_count)

#use predict to see the prediction
sample_predict <- predict(tree_pruned, listings_history_sample)


## start with recency frequency models
set.seed(123)
tree_rf <- rpart(has_reviews_2016 ~ host_listings_count + host_duration + 
                   first_seen_month + last_seen_month + 
                   listing_recency_2015_weeks + scrap_duration + 
                   total_occ_2015 + review_recency_2015_weeks + 
                   is_superhost_2015 + is_superhost_count_2015, 
                 data = listings_history_train, 
                 control = rpart.control(maxdepth = 5))
printcp(tree_rf)
bestcp_rf <- tree_rf$cptable[which.min(tree_rf$cptable[,"xerror"]), "CP"]
#prune tree using best cp
tree_pruned_rf <- prune(tree_rf, cp = bestcp_rf)

#plots
plot(tree_pruned_rf, uniform = TRUE)
text(tree_pruned_rf, cex = 0.8, use.n = TRUE, xpd = TRUE)
prp(tree_pruned_rf, faclen = 0, cex = 0.8, extra = 1)

#use the tree_pruned_rf to predict on the test set
tree_predict <- predict(tree_pruned_rf, listings_history_test)
ROCR_rf <- prediction(tree_predict, listings_history_test$has_reviews_2016)
