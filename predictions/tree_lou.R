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
plot_decision_tree <- function(tree_model, pruned=FALSE){
  if(pruned == TRUE){
    tree_model_pruned = prune_tree(tree_model)  # prune tree using best cp
    #plot tree
    plot(tree_model_pruned, uniform = TRUE) 
    text(tree_model_pruned, cex = 0.8, use.n = TRUE, xpd = TRUE)
    prp(tree_model_pruned, faclen = 0, cex = 0.8, extra = 1)
  } else {
    #plot tree
    plot(tree_model, uniform = TRUE) 
    text(tree_model, cex = 0.8, use.n = TRUE, xpd = TRUE)
    prp(tree_model, faclen = 0, cex = 0.8, extra = 1)
  }
}

plot_roc_curve <- function(tree_model){
  #use the tree_pruned_model to predict on the test set
  model_predict <- predict(prune_tree(tree_model), listings_history_test)
  ROCR_model <- prediction(model_predict, listings_history_test$has_reviews_2016)
  roc.perf <- performance(ROCR_model, measure = "tpr", x.measure = "fpr")
  #plot the ROC curve
  plot(roc.perf)
  abline(a=0, b= 1)
}

roc_area <- function(tree_model){
  #use the tree_pruned_model to predict on the test set
  model_predict <- predict(prune_tree(tree_model), listings_history_test)
  ROCR_model <- prediction(model_predict, listings_history_test$has_reviews_2016)
  #find the area under the roc curve
  auc_model <- performance(ROCR_model, measure = "auc")
  auc_model@y.values
}

prune_tree <- function(tree_model){
  bestcp <- tree_model$cptable[which.min(tree_model$cptable[,"xerror"]), "CP"]
  tree_model_pruned <- prune(tree_model, cp = bestcp) # prune tree using best cp
  tree_model_pruned
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

# tree 
plot_decision_tree(tree_rf)

# ROC curve
plot_roc_curve(tree_rf)

# ROC area
roc_area(tree_rf) # 0.7898

############################################################# [ exists_in_2016 ~ reviews (RV) ]
# model
tree_rv <- rpart(exist_in_2016 ~ first_review_year + last_review_year +
                   num_as_of_2015 + num_reviews_in_2015 + has_reviews_2015 +
                   first_review_month_2015 + last_review_month_2015 + 
                   review_recency_2015_weeks + last_rating, 
                 data = listings_history_train, 
                 control = rpart.control(maxdepth = 5))

# tree
plot_decision_tree(tree_rv)

# ROC curve
plot_roc_curve(tree_rv)

# ROC area
roc_area(tree_rv) # 0.8548


############################################################# [ exists_in_2016 ~ price (P) ]
# model
tree_p <- rpart(exist_in_2016 ~ min_price + max_price + mean_price, 
                data = listings_history_train, 
                control = rpart.control(maxdepth = 5))

# tree
plot_decision_tree(tree_p) #Fail: fit is not a tree, just a root

# ROC curve
plot_roc_curve(tree_p)

# ROC area
roc_area(tree_p) # 0.8548


############################################################# [ exists_in_2016 ~ RF + RV ] 
tree_rf_rv <- rpart(exist_in_2016 ~ host_listings_count + host_duration + 
                      first_seen_month + last_seen_month + 
                      listing_recency_2015_weeks + scrap_duration + 
                      total_occ_2015 + review_recency_2015_weeks + 
                      is_superhost_2015 + is_superhost_count_2015 + 
                      first_review_year + last_review_year +
                      num_as_of_2015 + num_reviews_in_2015 + has_reviews_2015 +
                      first_review_month_2015 + last_review_month_2015 + 
                      last_rating, 
                    data = listings_history_train, 
                    control = rpart.control(maxdepth = 5))

# tree
plot_decision_tree(tree_rf_rv) #nothing changes!

# ROC curve
plot_roc_curve(tree_rf_rv)

# ROC area
roc_area(tree_rf_rv) # 0.7898419

############################################################# [ exists_in_2016 ~ RF + P ] 
tree_rf_p <- rpart(exist_in_2016 ~ host_listings_count + host_duration + 
                     first_seen_month + last_seen_month + 
                     listing_recency_2015_weeks + scrap_duration + 
                     total_occ_2015 + review_recency_2015_weeks + 
                     is_superhost_2015 + is_superhost_count_2015 + 
                     min_price + max_price + mean_price, 
                   data = listings_history_train, 
                   control = rpart.control(maxdepth = 5))
# tree
plot_decision_tree(tree_rf_p) #nothing changes again!

# ROC curve
plot_roc_curve(tree_rf_p)

# ROC area
roc_area(tree_rf_p) # 0.7898419

############################################################# [ exists_in_2016 ~ RV + P ] 
tree_rv_p <- rpart(has_reviews_2016 ~ first_review_year + last_review_year +
                     num_as_of_2015 + num_reviews_in_2015 + has_reviews_2015 +
                     first_review_month_2015 + last_review_month_2015 + 
                     review_recency_2015_weeks + last_rating + min_price + 
                     max_price + mean_price, 
                   data = listings_history_train, 
                   control = rpart.control(maxdepth = 5))

# plot
plot_decision_tree(tree_rv_p) 

# ROC curve
plot_roc_curve(tree_rv_p)

# ROC area
roc_area(tree_rv_p) # 0.8547258

############################################################# [ exists_in_2016 ~ RF + RV + P ] 
tree_rf_rv_p <- rpart(exist_in_2016 ~ host_listings_count + host_duration + 
                        first_seen_month + last_seen_month + 
                        listing_recency_2015_weeks + scrap_duration + 
                        total_occ_2015 + review_recency_2015_weeks + 
                        is_superhost_2015 + is_superhost_count_2015 + 
                        first_review_year + last_review_year +
                        num_as_of_2015 + num_reviews_in_2015 + has_reviews_2015 +
                        first_review_month_2015 + last_review_month_2015 + 
                        last_rating + mean_price + max_price + min_price + verifications_count, 
                      data = listings_history_train, 
                      control = rpart.control(maxdepth = 5))

# tree
plot_decision_tree(tree_rf_rv_p) 

# ROC curve
plot_roc_curve(tree_rf_rv_p)

# ROC area
roc_area(tree_rf_rv_p) # 0.7898419

