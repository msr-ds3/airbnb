library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(scales)
library(rpart)
library(rpart.plot)


############################################################# [ data prep ]
listings_history <- read_csv("../raw_data/listing_history.csv")
View(listings_history)

#listings_history[78] <- NULL #deleting mulltiple listing_id column in column 78. Don't need this if Kaciny fixes.

# create test and train set
set.seed(123)
indexes <- sample(1:nrow(listings_history), 
                  size=0.2*nrow(listings_history))
listings_history_test=listings_history[indexes, ]
listings_history_train=listings_history[-indexes, ]

############################################################# [ functions ]
# plot pruned tree, with some hardcoded assumptions
plot_decision_tree <- function(tree_model){
  bestcp <- tree_model$cptable[which.min(tree_model$cptable[,"xerror"]), "CP"]
  tree_model_pruned <- prune(tree_model, cp = bestcp) # prune tree using best cp
  #plot
  plot(tree_model_pruned, uniform = TRUE) 
  text(tree_model_pruned, cex = 0.8, use.n = TRUE, xpd = TRUE)
  prp(tree_model_pruned, faclen = 0, cex = 0.8, extra = 1)
}
############################################################# [ exists_in_2016 ~ recency frequency (RF) ]
# model
tree_rf <- rpart(exist_in_2016 ~ host_listings_count + host_duration + 
                   first_seen_month + last_seen_month + 
                   listing_recency_2015_weeks + scrap_duration + 
                   total_occ_2015 + review_recency_2015_weeks + 
                   is_superhost_2015 + is_superhost_count_2015, 
                 data = listings_history_train, 
                 control = rpart.control(maxdepth = 5))

printcp(tree_rf) #summary

# draw
plot_decision_tree(tree_rf)

############################################################# [ exists_in_2016 ~ reviews (RV) ]
# model
tree_rv <- rpart(exist_in_2016 ~ first_review_year + last_review_year +
                   num_as_of_2015 + num_reviews_in_2015 + has_reviews_2015 +
                   first_review_month_2015 + last_review_month_2015 + 
                   review_recency_2015_weeks + last_rating, 
                 data = listings_history_train, 
                 control = rpart.control(maxdepth = 5))

printcp(tree_rv)

# draw
plot_decision_tree(tree_rv)
