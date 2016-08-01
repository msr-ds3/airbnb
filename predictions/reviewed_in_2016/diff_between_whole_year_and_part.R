#install.packages("rpart")
#install.packages("rpart.plot")
#install.packages("ROCR")

library(rpart)
library(rpart.plot)
library(readr)
library(dplyr)
library(ROCR)

#load the test and train data for january
#this file was created by  "create_test_train_january_cohort.R"

load("../skimmed_listings_history.RData")

View(skimmed_listings_history)

all_2015 <- skimmed_listings_history %>% filter(total_occ_2015 == 10)
part_2015 <- skimmed_listings_history %>% filter(total_occ_2015 != 10)

#deal with all listings first
indexes <- sample(1:nrow(all_2015), 
                  size=0.2*nrow(all_2015))

#apply the indexes to listings history to generate test and train
test_all=all_2015[indexes, ]
train_all=all_2015[-indexes, ]

#models

#recency frequency
tree <- rpart(has_reviews_2016 ~ host_listings_count + host_duration + 
                first_seen_month + last_seen_month + 
                listing_recency_2015_weeks + scrap_duration + 
                total_occ_2015 + review_recency_2015_weeks + 
                is_superhost_2015 + is_superhost_count_2015, 
              data = train_all, 
              control = rpart.control(cp = 0.001))

# evaluate on the test data
bestcp <- tree$cptable[which.min(tree$cptable[,"xerror"]), "CP"]
tree_pruned <- prune(tree, cp = bestcp)
predicted <- predict(tree_pruned, test_all)
ROCR_predict <- prediction(predicted, test_all$has_reviews_2016)
roc.perf <- performance(ROCR_predict, measure = "tpr", x.measure = "fpr")
auc_pred <- performance(ROCR_predict, measure = "auc")
auc_pred@y.values

tree <- rpart(has_reviews_2016 ~ first_review_year + last_review_year +
                num_as_of_2015 + num_reviews_in_2015 + has_reviews_2015 +
                first_review_month_2015 + last_review_month_2015 + 
                last_rating, 
              data = listings_train, 
              control = rpart.control(cp = 0.001))

# evaluate on the test data
# evaluate on the test data
bestcp <- tree$cptable[which.min(tree$cptable[,"xerror"]), "CP"]
tree_pruned <- prune(tree, cp = bestcp)
predicted <- predict(tree_pruned, listings_test)
ROCR_predict <- prediction(predicted, listings_test$has_reviews_2016)
roc.perf <- performance(ROCR_predict, measure = "tpr", x.measure = "fpr")
auc_pred <- performance(ROCR_predict, measure = "auc")
auc[i]  <- auc_pred@y.values



for (i in trees) {
  tree <- rpart(feature, data = listings_train, 
                control = rpart.control(cp = 0.001))
  
  # evaluate on the test data
  # evaluate on the test data
  bestcp <- tree$cptable[which.min(tree$cptable[,"xerror"]), "CP"]
  tree_pruned <- prune(tree, cp = bestcp)
  predicted <- predict(tree_pruned, listings_test)
  ROCR_predict <- prediction(predicted, listings_test$has_reviews_2016)
  roc.perf <- performance(ROCR_predict, measure = "tpr", x.measure = "fpr")
  auc_pred <- performance(ROCR_predict, measure = "auc")
  auc[i]  <- auc_pred@y.values
}
