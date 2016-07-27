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

#create test and train set
indexes <- sample(1:nrow(listings_history), 
                  size=0.2*nrow(listings_history))
listings_history_test=listings_history[indexes, ]
listings_history_train=listings_history[-indexes, ]

#recency frequency models
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
rf_predict <- predict(tree_pruned_rf, listings_history_test)
ROCR_rf <- prediction(rf_predict, listings_history_test$has_reviews_2016)
roc.perf <- performance(ROCR_rf, measure = "tpr", x.measure = "fpr")
#plot the ROC curve
plot(roc.perf)
abline(a=0, b= 1)
#find the area under the roc curve
auc_rf <- performance(ROCR_rf, measure = "auc")
auc_rf@y.values
#0.8789

##review models
set.seed(123)
tree_rv <- rpart(has_reviews_2016 ~ first_review_year + last_review_year +
                   num_as_of_2015 + num_reviews_in_2015 + has_reviews_2015 +
                   first_review_month_2015 + last_review_month_2015 + 
                   review_recency_2015_weeks + last_rating, 
                 data = listings_history_train, 
                 control = rpart.control(maxdepth = 5))
printcp(tree_rv)
bestcp_rv <- tree_rv$cptable[which.min(tree_rv$cptable[,"xerror"]), "CP"]
#prune tree using best cp
tree_pruned_rv <- prune(tree_rv, cp = bestcp_rv)

#plots
plot(tree_pruned_rv, uniform = TRUE)
text(tree_pruned_rv, cex = 0.8, use.n = TRUE, xpd = TRUE)
prp(tree_pruned_rv, faclen = 0, cex = 0.8, extra = 1)

#use the tree_pruned_rf to predict on the test set
rv_predict <- predict(tree_pruned_rv, listings_history_test)
ROCR_rv <- prediction(rv_predict, listings_history_test$has_reviews_2016)
roc_perf_rv <- performance(ROCR_rv, measure = "tpr", x.measure = "fpr")
plot(roc_perf_rv)
abline(a=0, b= 1)
auc_rv <- performance(ROCR_rv, measure = "auc")
auc_rv@y.values
#0.8452245

##price trees
set.seed(123)
tree_p <- rpart(has_reviews_2016 ~ min_price + max_price + mean_price, 
                 data = listings_history_train, 
                 control = rpart.control(maxdepth = 5))
printcp(tree_p)
bestcp_p <- tree_p$cptable[which.min(tree_p$cptable[,"xerror"]), "CP"]
#prune tree using best cp
tree_pruned_p <- prune(tree_p, cp = bestcp_p)

#plots
prp(tree_pruned_p, faclen = 0, cex = 0.8, extra = 1)

#use the tree_pruned_rf to predict on the test set
p_predict <- predict(tree_pruned_p, listings_history_test)
ROCR_p <- prediction(p_predict, listings_history_test$has_reviews_2016)
roc_perf_p <- performance(ROCR_p, measure = "tpr", x.measure = "fpr")
#plot the roc curve
plot(roc_perf_p)
abline(a=0, b= 1)
#find the area under the roc curve
auc_p <- performance(ROCR_p, measure = "auc")
auc_p@y.values
#0.5

# RF and RV trees
set.seed(123)
tree_rf_rv <- rpart(has_reviews_2016 ~ host_listings_count + host_duration + 
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
printcp(tree_rf_rv)
bestcp_rf_rv <- tree_rf_rv$cptable[which.min(tree_rf_rv$cptable[,"xerror"]), "CP"]
#prune tree using best cp
tree_pruned_rf_rv <- prune(tree_rf_rv, cp = bestcp_rf_rv)

#plots
plot(tree_pruned_rf_rv, uniform = TRUE)
text(tree_pruned_rf_rv, cex = 0.8, use.n = TRUE, xpd = TRUE)
prp(tree_pruned_rf_rv, faclen = 0, cex = 0.8, extra = 1)

#use the tree_pruned_rf to predict on the test set
rf_rv_predict <- predict(tree_pruned_rf_rv, listings_history_test)
ROCR_rf_rv <- prediction(rf_rv_predict, listings_history_test$has_reviews_2016)
roc_perf_rf_rv <- performance(ROCR_rf_rv, measure = "tpr", x.measure = "fpr")
#plot the ROC curve
plot(roc_perf_rf_rv)
abline(a=0, b= 1)
#find the area under the roc curve
auc_rf_rv <- performance(ROCR_rf_rv, measure = "auc")
auc_rf_rv@y.values
#0.8837904

#recency frequency & price trees
set.seed(123)
tree_rf_p <- rpart(has_reviews_2016 ~ host_listings_count + host_duration + 
                   first_seen_month + last_seen_month + 
                   listing_recency_2015_weeks + scrap_duration + 
                   total_occ_2015 + review_recency_2015_weeks + 
                   is_superhost_2015 + is_superhost_count_2015 + 
                   min_price + max_price + mean_price, 
                 data = listings_history_train, 
                 control = rpart.control(maxdepth = 5))
printcp(tree_rf_p)
bestcp_rf_p <- tree_rf_p$cptable[which.min(tree_rf_p$cptable[,"xerror"]), "CP"]
#prune tree using best cp
tree_pruned_rf_p <- prune(tree_rf_p, cp = bestcp_rf_p)

#plots
plot(tree_pruned_rf_p, uniform = TRUE)
text(tree_pruned_rf_p, cex = 0.8, use.n = TRUE, xpd = TRUE)
prp(tree_pruned_rf_p, faclen = 0, cex = 0.8, extra = 1)

#use the tree_pruned_rf to predict on the test set
rf_p_predict <- predict(tree_pruned_rf_p, listings_history_test)
ROCR_rf_p <- prediction(rf_p_predict, listings_history_test$has_reviews_2016)
roc_perf_rf_p <- performance(ROCR_rf_p, measure = "tpr", x.measure = "fpr")
#plot the ROC curve
plot(roc_perf_rf_p)
abline(a=0, b= 1)
#find the area under the roc curve
auc_rf_p <- performance(ROCR_rf_p, measure = "auc")
auc_rf_p@y.values
#0.8789705

##review & price trees
set.seed(123)
tree_rv_p <- rpart(has_reviews_2016 ~ first_review_year + last_review_year +
                   num_as_of_2015 + num_reviews_in_2015 + has_reviews_2015 +
                   first_review_month_2015 + last_review_month_2015 + 
                   review_recency_2015_weeks + last_rating + min_price + 
                   max_price + mean_price, 
                 data = listings_history_train, 
                 control = rpart.control(maxdepth = 5))
printcp(tree_rv_p)
bestcp_rv_p<- tree_rv_p$cptable[which.min(tree_rv_p$cptable[,"xerror"]), "CP"]
#prune tree using best cp
tree_pruned_rv_p <- prune(tree_rv_p, cp = bestcp_rv_p)

#plots
plot(tree_pruned_rv_p, uniform = TRUE)
text(tree_pruned_rv_p, cex = 0.8, use.n = TRUE, xpd = TRUE)
prp(tree_pruned_rv_p, faclen = 0, cex = 0.8, extra = 1)

#use the tree_pruned_rf to predict on the test set
rv_p_predict <- predict(tree_pruned_rv_p, listings_history_test)
ROCR_rv_p <- prediction(rv_p_predict, listings_history_test$has_reviews_2016)
roc_perf_rv_p <- performance(ROCR_rv_p, measure = "tpr", x.measure = "fpr")
plot(roc_perf_rv_p)
abline(a=0, b= 1)
auc_rv_p <- performance(ROCR_rv_p, measure = "auc")
auc_rv_p@y.values
#0.8452245

# RF, RV, and Price trees
set.seed(123)
tree_rf_rv_p <- rpart(has_reviews_2016 ~ host_listings_count + host_duration + 
                      first_seen_month + last_seen_month + 
                      listing_recency_2015_weeks + scrap_duration + 
                      total_occ_2015 + review_recency_2015_weeks + 
                      is_superhost_2015 + is_superhost_count_2015 + 
                      first_review_year + last_review_year +
                      num_as_of_2015 + num_reviews_in_2015 + has_reviews_2015 +
                      first_review_month_2015 + last_review_month_2015 + 
                      last_rating + mean_price + max_price + min_price, 
                    data = listings_history_train, 
                    control = rpart.control(maxdepth = 5))
printcp(tree_rf_rv_p)
bestcp_rf_rv_p <- tree_rf_rv_p$cptable[which.min(tree_rf_rv_p$cptable[,"xerror"]), "CP"]
#prune tree using best cp
tree_pruned_rf_rv_p <- prune(tree_rf_rv_p, cp = bestcp_rf_rv_p)

#plots
plot(tree_pruned_rf_rv_p, uniform = TRUE)
text(tree_pruned_rf_rv_p, cex = 0.8, use.n = TRUE, xpd = TRUE)
prp(tree_pruned_rf_rv_p, faclen = 0, cex = 0.8, extra = 1)

#use the tree_pruned_rf to predict on the test set
rf_rv_p_predict <- predict(tree_pruned_rf_rv_p, listings_history_test)
ROCR_rf_rv_p <- prediction(rf_rv_p_predict, listings_history_test$has_reviews_2016)
roc_perf_rf_rv_p <- performance(ROCR_rf_rv_p, measure = "tpr", x.measure = "fpr")
#plot the ROC curve
plot(roc_perf_rf_rv_p)
abline(a=0, b= 1)
#find the area under the roc curve
auc_rf_rv_p <- performance(ROCR_rf_rv_p, measure = "auc")
auc_rf_rv_p@y.values
#0.8837904

# RF RV P and add in room_type and is_multilistings
set.seed(123)
tree <- rpart(has_reviews_2016 ~ host_listings_count + host_duration + 
                        first_seen_month + last_seen_month + 
                        listing_recency_2015_weeks + scrap_duration + 
                        total_occ_2015 + review_recency_2015_weeks + 
                        is_superhost_2015 + is_superhost_count_2015 + 
                        first_review_year + last_review_year +
                        num_as_of_2015 + num_reviews_in_2015 + has_reviews_2015 +
                        first_review_month_2015 + last_review_month_2015 + 
                        last_rating + mean_price + max_price + min_price + 
                        room_type + is_multilisting, 
                      data = listings_history_train, 
                      control = rpart.control(maxdepth = 5))
printcp(tree)
bestcp <- tree$cptable[which.min(tree$cptable[,"xerror"]), "CP"]
#prune tree using best cp
tree_pruned <- prune(tree, cp = bestcp)

#plots
plot(tree_pruned, uniform = TRUE)
text(tree_pruned, cex = 0.8, use.n = TRUE, xpd = TRUE)
prp(tree_pruned, faclen = 0, cex = 0.8, extra = 1)

#use the tree_pruned_rf to predict on the test set
predict <- predict(tree_pruned, listings_history_test)
ROCR <- prediction(predict, listings_history_test$has_reviews_2016)
roc_perf<- performance(ROCR, measure = "tpr", x.measure = "fpr")
#plot the ROC curve
plot(roc_perf)
abline(a=0, b= 1)
#find the area under the roc curve
auc <- performance(ROCR, measure = "auc")
auc@y.values
#0.8837904

#using only the january cohort
listings_history_januarycohort <- listings_history %>% filter(first_seen_month == 1)
View(listings_history_januarycohort)

#create a test and train set for the january cohort
jan_indexes2 <- sample(1:nrow(listings_history_januarycohort), 
                  size=0.2*nrow(listings_history_januarycohort))
listings_history_test_jan2=listings_history_januarycohort[jan_indexes2, ]
listings_history_train_jan2=listings_history_januarycohort[-jan_indexes2, ]

#recency frequency models
set.seed(123)
jan_tree_rf <- rpart(has_reviews_2016 ~ host_listings_count + host_duration + 
                   first_seen_month + last_seen_month + 
                   listing_recency_2015_weeks + scrap_duration + 
                   total_occ_2015 + review_recency_2015_weeks + 
                   is_superhost_2015 + is_superhost_count_2015, 
                 data = listings_history_train_jan, 
                 control = rpart.control(maxdepth = 5))
printcp(jan_tree_rf)
jan_bestcp_rf <- jan_tree_rf$cptable[which.min(jan_tree_rf$cptable[,"xerror"]), "CP"]
#prune tree using best cp
jan_tree_pruned_rf <- prune(jan_tree_rf, cp = jan_bestcp_rf)

#plots
plot(jan_tree_pruned_rf, uniform = TRUE)
text(jan_tree_pruned_rf, cex = 0.8, use.n = TRUE, xpd = TRUE)
prp(jan_tree_pruned_rf, faclen = 0, cex = 0.8, extra = 1)

#use the tree_pruned_rf to predict on the test set
jan_rf_predict <- predict(jan_tree_pruned_rf, listings_history_test_jan)
jan_ROCR_rf <- prediction(jan_rf_predict, listings_history_test_jan$has_reviews_2016)
jan_roc_perf_rf <- performance(jan_ROCR_rf, measure = "tpr", x.measure = "fpr")
#plot the ROC curve
plot(jan_roc_perf_rf)
abline(a=0, b= 1)
#find the area under the roc curve
jan_auc_rf <- performance(jan_ROCR_rf, measure = "auc")
jan_auc_rf@y.values
#0.9123

##review models
set.seed(123)
jan_tree_rv <- rpart(has_reviews_2016 ~ first_review_year + last_review_year +
                   num_as_of_2015 + num_reviews_in_2015 + has_reviews_2015 +
                   first_review_month_2015 + last_review_month_2015 + 
                   review_recency_2015_weeks + last_rating, 
                 data = listings_history_train_jan, 
                 control = rpart.control(maxdepth = 5))
printcp(jan_tree_rv)
jan_bestcp_rv <- jan_tree_rv$cptable[which.min(jan_tree_rv$cptable[,"xerror"]), "CP"]
#prune tree using best cp
jan_tree_pruned_rv <- prune(tree_rv, cp = bestcp_rv)

#plots
plot(jan_tree_pruned_rv, uniform = TRUE)
text(jan_tree_pruned_rv, cex = 0.8, use.n = TRUE, xpd = TRUE)
prp(jan_tree_pruned_rv, faclen = 0, cex = 0.8, extra = 1)

#use the tree_pruned_rf to predict on the test set
jan_rv_predict <- predict(jan_tree_pruned_rv, listings_history_test_jan)
jan_ROCR_rv <- prediction(jan_rv_predict, listings_history_test_jan$has_reviews_2016)
jan_roc_perf_rv <- performance(jan_ROCR_rv, measure = "tpr", x.measure = "fpr")
plot(jan_roc_perf_rv)
abline(a=0, b= 1)
jan_auc_rv <- performance(jan_ROCR_rv, measure = "auc")
jan_auc_rv@y.values
#0.89431

##price trees
set.seed(123)
jan_tree_p <- rpart(has_reviews_2016 ~ min_price + max_price + mean_price, 
                data = listings_history_train_jan, 
                control = rpart.control(maxdepth = 5))
printcp(jan_tree_p)
jan_bestcp_p <- jan_tree_p$cptable[which.min(jan_tree_p$cptable[,"xerror"]), "CP"]
#prune tree using best cp
jan_tree_pruned_p <- prune(jan_tree_p, cp = jan_bestcp_p)

#plots
prp(jan_tree_pruned_p, faclen = 0, cex = 0.8, extra = 1)

#use the tree_pruned_rf to predict on the test set
jan_p_predict <- predict(jan_tree_pruned_p, listings_history_test_jan)
jan_ROCR_p <- prediction(jan_p_predict, listings_history_test_jan$has_reviews_2016)
jan_roc_perf_p <- performance(jan_ROCR_p, measure = "tpr", x.measure = "fpr")
#plot the roc curve
plot(jan_roc_perf_p)
abline(a=0, b= 1)
#find the area under the roc curve
jan_auc_p <- performance(jan_ROCR_p, measure = "auc")
jan_auc_p@y.values
#0.5

# RF and RV trees
set.seed(123)
jan_tree_rf_rv <- rpart(has_reviews_2016 ~ host_listings_count + host_duration + 
                      first_seen_month + last_seen_month + 
                      listing_recency_2015_weeks + scrap_duration + 
                      total_occ_2015 + review_recency_2015_weeks + 
                      is_superhost_2015 + is_superhost_count_2015 + 
                      first_review_year + last_review_year +
                      num_as_of_2015 + num_reviews_in_2015 + has_reviews_2015 +
                      first_review_month_2015 + last_review_month_2015 + 
                      last_rating, 
                    data = listings_history_train_jan, 
                    control = rpart.control(maxdepth = 5))
printcp(jan_tree_rf_rv)
jan_bestcp_rf_rv <- jan_tree_rf_rv$cptable[which.min(jan_tree_rf_rv$cptable[,"xerror"]), "CP"]
#prune tree using best cp
jan_tree_pruned_rf_rv <- prune(jan_tree_rf_rv, cp = jan_bestcp_rf_rv)

#plots
plot(jan_tree_pruned_rf_rv, uniform = TRUE)
text(jan_tree_pruned_rf_rv, cex = 0.8, use.n = TRUE, xpd = TRUE)
prp(jan_tree_pruned_rf_rv, faclen = 0, cex = 0.8, extra = 1)

#use the tree_pruned_rf to predict on the test set
jan_rf_rv_predict <- predict(jan_tree_pruned_rf_rv, listings_history_test_jan)
jan_ROCR_rf_rv <- prediction(jan_rf_rv_predict, listings_history_test_jan$has_reviews_2016)
jan_roc_perf_rf_rv <- performance(jan_ROCR_rf_rv, measure = "tpr", x.measure = "fpr")
#plot the ROC curve
plot(jan_roc_perf_rf_rv)
abline(a=0, b= 1)
#find the area under the roc curve
jan_auc_rf_rv <- performance(jan_ROCR_rf_rv, measure = "auc")
jan_auc_rf_rv@y.values
#0.9115

#recency frequency & price trees
set.seed(123)
jan_tree_rf_p <- rpart(has_reviews_2016 ~ host_listings_count + host_duration + 
                     first_seen_month + last_seen_month + 
                     listing_recency_2015_weeks + scrap_duration + 
                     total_occ_2015 + review_recency_2015_weeks + 
                     is_superhost_2015 + is_superhost_count_2015 + 
                     min_price + max_price + mean_price, 
                   data = listings_history_train_jan, 
                   control = rpart.control(maxdepth = 5))
printcp(jan_tree_rf_p)
jan_bestcp_rf_p <- jan_tree_rf_p$cptable[which.min(jan_tree_rf_p$cptable[,"xerror"]), "CP"]
#prune tree using best cp
jan_tree_pruned_rf_p <- prune(jan_tree_rf_p, cp = jan_bestcp_rf_p)

#plots
plot(jan_tree_pruned_rf_p, uniform = TRUE)
text(jan_tree_pruned_rf_p, cex = 0.8, use.n = TRUE, xpd = TRUE)
prp(jan_tree_pruned_rf_p, faclen = 0, cex = 0.8, extra = 1)

#use the tree_pruned_rf to predict on the test set
jan_rf_p_predict <- predict(jan_tree_pruned_rf_p, listings_history_test_jan)
jan_ROCR_rf_p <- prediction(jan_rf_p_predict, listings_history_test_jan$has_reviews_2016)
jan_roc_perf_rf_p <- performance(jan_ROCR_rf_p, measure = "tpr", x.measure = "fpr")
#plot the ROC curve
plot(jan_roc_perf_rf_p)
abline(a=0, b= 1)
#find the area under the roc curve
jan_auc_rf_p <- performance(jan_ROCR_rf_p, measure = "auc")
jan_auc_rf_p@y.values
#0.9123

##review & price trees
set.seed(123)
jan_tree_rv_p <- rpart(has_reviews_2016 ~ first_review_year + last_review_year +
                     num_as_of_2015 + num_reviews_in_2015 + has_reviews_2015 +
                     first_review_month_2015 + last_review_month_2015 + 
                     review_recency_2015_weeks + last_rating + min_price + 
                     max_price + mean_price, 
                   data = listings_history_train_jan, 
                   control = rpart.control(maxdepth = 5))
printcp(jan_tree_rv_p)
jan_bestcp_rv_p<- jan_tree_rv_p$cptable[which.min(jan_tree_rv_p$cptable[,"xerror"]), "CP"]
#prune tree using best cp
jan_tree_pruned_rv_p <- prune(jan_tree_rv_p, cp = jan_bestcp_rv_p)

#plots
plot(jan_tree_pruned_rv_p, uniform = TRUE)
text(jan_tree_pruned_rv_p, cex = 0.8, use.n = TRUE, xpd = TRUE)
prp(jan_tree_pruned_rv_p, faclen = 0, cex = 0.8, extra = 1)

#use the tree_pruned_rf to predict on the test set
jan_rv_p_predict <- predict(jan_tree_pruned_rv_p, listings_history_test_jan)
jan_ROCR_rv_p <- prediction(jan_rv_p_predict, listings_history_test_jan$has_reviews_2016)
jan_roc_perf_rv_p <- performance(jan_ROCR_rv_p, measure = "tpr", x.measure = "fpr")
plot(jan_roc_perf_rv_p)
abline(a=0, b= 1)
jan_auc_rv_p <- performance(jan_ROCR_rv_p, measure = "auc")
jan_auc_rv_p@y.values
#0.89431

# RF, RV, and Price trees
set.seed(123)
jan_tree_rf_rv_p <- rpart(has_reviews_2016 ~ host_listings_count + host_duration + 
                        first_seen_month + last_seen_month + 
                        listing_recency_2015_weeks + scrap_duration + 
                        total_occ_2015 + review_recency_2015_weeks + 
                        is_superhost_2015 + is_superhost_count_2015 + 
                        first_review_year + last_review_year +
                        num_as_of_2015 + num_reviews_in_2015 + has_reviews_2015 +
                        first_review_month_2015 + last_review_month_2015 + 
                        last_rating + mean_price + max_price + min_price, 
                      data = listings_history_train_jan, 
                      control = rpart.control(maxdepth = 5))
printcp(jan_tree_rf_rv_p)
jan_bestcp_rf_rv_p <- jan_tree_rf_rv_p$cptable[which.min(jan_tree_rf_rv_p$cptable[,"xerror"]), "CP"]
#prune tree using best cp
jan_tree_pruned_rf_rv_p <- prune(jan_tree_rf_rv_p, cp = jan_bestcp_rf_rv_p)

#plots
plot(jan_tree_pruned_rf_rv_p, uniform = TRUE)
text(jan_tree_pruned_rf_rv_p, cex = 0.8, use.n = TRUE, xpd = TRUE)
prp(jan_tree_pruned_rf_rv_p, faclen = 0, cex = 0.8, extra = 1)

#use the tree_pruned_rf to predict on the test set
jan_rf_rv_p_predict <- predict(jan_tree_pruned_rf_rv_p, listings_history_test_jan)
jan_ROCR_rf_rv_p <- prediction(jan_rf_rv_p_predict, listings_history_test_jan$has_reviews_2016)
jan_roc_perf_rf_rv_p <- performance(jan_ROCR_rf_rv_p, measure = "tpr", x.measure = "fpr")
#plot the ROC curve
plot(jan_roc_perf_rf_rv_p)
abline(a=0, b= 1)
#find the area under the roc curve
jan_auc_rf_rv_p <- performance(jan_ROCR_rf_rv_p, measure = "auc")
jan_auc_rf_rv_p@y.values
#0.9115

#all and add in room type, multilisting;
jan_tree <- rpart(has_reviews_2016 ~ host_listings_count + host_duration + 
                            first_seen_month + last_seen_month + 
                            listing_recency_2015_weeks + scrap_duration + 
                            total_occ_2015 + review_recency_2015_weeks + 
                            is_superhost_2015 + is_superhost_count_2015 + 
                            first_review_year + last_review_year +
                            num_as_of_2015 + num_reviews_in_2015 + has_reviews_2015 +
                            first_review_month_2015 + last_review_month_2015 + 
                            last_rating + mean_price + max_price + min_price + 
                            room_type + is_multilisting , 
                          data = listings_history_train_jan, 
                          control = rpart.control(maxdepth = 5))
printcp(jan_tree)
jan_bestcp <- jan_tree$cptable[which.min(jan_tree$cptable[,"xerror"]), "CP"]
#prune tree using best cp
jan_tree_pruned <- prune(jan_tree, cp = jan_bestcp)

#plots
plot(jan_tree_pruned, uniform = TRUE)
text(jan_tree_pruned, cex = 0.8, use.n = TRUE, xpd = TRUE)
prp(jan_tree_pruned, faclen = 0, cex = 0.8, extra = 1)

#use the tree_pruned_rf to predict on the test set
jan_predict <- predict(jan_tree_pruned, listings_history_test_jan)
jan_ROCR <- prediction(jan_predict, listings_history_test_jan$has_reviews_2016)
jan_roc_perf <- performance(jan_ROCR, measure = "tpr", x.measure = "fpr")
#plot the ROC curve
plot(jan_roc_perf)
abline(a=0, b= 1)
#find the area under the roc curve
jan_auc <- performance(jan_ROCR, measure = "auc")
jan_auc@y.values
#0.9115

#added in all ammenities
jan_tree_amen <- rpart(exist_in_2016 ~ TV + Internet + Wireless.Internet +
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
                         translation.missing..en.hosting_amenity_50 + 
                         host_listings_count + host_duration + 
                         first_seen_month + last_seen_month + 
                    is_superhost_2015 + is_superhost_count_2015 + 
                         first_review_year + last_review_year +
                         num_as_of_2015 + num_reviews_in_2015 + has_reviews_2015 +
                         first_review_month_2015 + last_review_month_2015 + 
                         last_rating + mean_price + max_price + min_price +room_type , 
                  data = listings_history_train_jan2, 
                  control = rpart.control(cp = 0.001))
printcp(jan_tree_amen)
jan_amen_bestcp <- jan_tree_amen$cptable[which.min(jan_tree_amen$cptable[,"xerror"]), "CP"]
#prune tree using best cp
jan_amen_tree_pruned <- prune(jan_tree_amen, cp = jan_amen_bestcp)

#plots
plot(jan_amen_tree_pruned, uniform = TRUE)
text(jan_amen_tree_pruned, cex = 0.8, use.n = TRUE, xpd = TRUE)
prp(jan_amen_tree_pruned, faclen = 0, cex = 0.8, extra = 1)

#use the tree_pruned_rf to predict on the test set
jan_amen_predict <- predict(jan_amen_tree_pruned, test)
jan_amen_ROCR <- prediction(jan_amen_predict, test$exist_in_2016)
jan_amen_roc_perf <- performance(jan_amen_ROCR, measure = "tpr", x.measure = "fpr")
#plot the ROC curve
plot(jan_amen_roc_perf)
abline(a=0, b= 1)
#find the area under the roc curve
jan_amen_auc <- performance(jan_amen_ROCR, measure = "auc")
jan_amen_auc@y.values
#0.9115

#look at verification

 

jan_tree_ver <- rpart(has_reviews_2016 ~ TV + Internet + Wireless.Internet +
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
                        translation.missing..en.hosting_amenity_50 + 
                        host_listings_count + host_duration + 
                        first_seen_month + last_seen_month + 
                        listing_recency_2015_weeks + scrap_duration + 
                        total_occ_2015 + review_recency_2015_weeks + 
                        is_superhost_2015 + is_superhost_count_2015 + 
                        first_review_year + last_review_year +
                        num_as_of_2015 + num_reviews_in_2015 + has_reviews_2015 +
                        first_review_month_2015 + last_review_month_2015 + 
                        last_rating + mean_price + max_price + min_price + 
                        email + phone +
                        facebook + reviews + kba + google + jumio + sent_id + 
                        linkedin +
                        manual_offline + manual_online + weibo + photographer + 
                        None +
                        amex + verifications_count, 
                       data = listings_history_train_jan, 
                       control = rpart.control(maxdepth = 5))
printcp(jan_tree_ver)
jan_ver_bestcp <- jan_tree_ver$cptable[which.min(jan_tree_ver$cptable[,"xerror"]), "CP"]
#prune tree using best cp
jan_ver_tree_pruned <- prune(jan_tree_ver, cp = jan_ver_bestcp)

#plots
plot(jan_ver_tree_pruned, uniform = TRUE)
text(jan_ver_tree_pruned, cex = 0.8, use.n = TRUE, xpd = TRUE)
prp(jan_ver_tree_pruned, faclen = 0, cex = 0.8, extra = 1)

#use the tree_pruned_rf to predict on the test set
jan_ver_predict <- predict(jan_ver_tree_pruned, listings_history_test_jan)
jan_ver_ROCR <- prediction(jan_ver_predict, listings_history_test_jan$has_reviews_2016)
jan_ver_roc_perf <- performance(jan_ver_ROCR, measure = "tpr", x.measure = "fpr")
#plot the ROC curve
plot(jan_ver_roc_perf)
abline(a=0, b= 1)
#find the area under the roc curve
jan_ver_auc <- performance(jan_ver_ROCR, measure = "auc")
jan_ver_auc@y.values
#0.9115


##FINALIZED with amenities and RF, reviews and price
jan_tree_amen <- rpart(has_reviews_2016 ~ TV + Internet + Wireless.Internet +
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
translation.missing..en.hosting_amenity_50 + 
host_listings_count + host_duration + 
first_seen_month + last_seen_month + 
listing_recency_2015_weeks + scrap_duration + 
total_occ_2015 + review_recency_2015_weeks + 
is_superhost_2015 + is_superhost_count_2015 + 
first_review_year + last_review_year +
num_as_of_2015 + num_reviews_in_2015 + has_reviews_2015 +
first_review_month_2015 + last_review_month_2015 + 
last_rating + mean_price + max_price + min_price +room_type , 
data = train, 
control = rpart.control(cp = 0.001))

#predict purged and not purged -- remove the features that we chose the dependent variable on
#add in multilistings and purged (join)
#isolate all this with reviews and price, [put rpart.control(cp=0.001)]

#pick on listing/host (each host is a customer -- do they return?)
#for the hosts that have multiple listings -- randomly pick one