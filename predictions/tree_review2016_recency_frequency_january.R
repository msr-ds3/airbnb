#to create and generate the tree for if there was a review or not in 2016
#for the January cohort and generate the ROC curve
#using recency frequency data 
#then adding in reviews, price, ammenities, verifications, and room type

library(rpart)
library(rpart.plot)
library(readr)
library(dplyr)
library(ROCR)

#load the test and train data for january
#this file was created by  "create_test_train_january_cohort.R"

load("jan_test_train.RData")

set.seed(123)

#create the tree for recency frequency features
tree_rf <- rpart(has_reviews_2016 ~ host_listings_count + host_duration + 
                   first_seen_month + last_seen_month + 
                   listing_recency_2015_weeks + scrap_duration + 
                   total_occ_2015 + review_recency_2015_weeks + 
                   is_superhost_2015 + is_superhost_count_2015, 
                 data = jan_train, 
                 control = rpart.control(cp = 0.001))

#view the tree
printcp(tree_rf)

#find the best cp in the tree
bestcp_rf <- tree_rf$cptable[which.min(tree_rf$cptable[,"xerror"]), "CP"]

#prune tree using best cp
tree_pruned_rf <- prune(tree_rf, cp = bestcp_rf)

#plots
plot(tree_pruned_rf, uniform = TRUE)
text(tree_pruned_rf, cex = 0.8, use.n = TRUE, xpd = TRUE)
png(file = "../figures/reviews2016_jan_rf.png")
prp(tree_pruned_rf, main = "Predicting Reviews in 2016 for the January Cohort
Based on Recency and Frequency", faclen = 0, cex = 0.8, extra = 1)
dev.off()

#use the tree_pruned_rf to predict on the test set
rf_predict <- predict(tree_pruned_rf, jan_test)
ROCR_rf <- prediction(rf_predict, jan_test$has_reviews_2016)
roc.perf <- performance(ROCR_rf, measure = "tpr", x.measure = "fpr")

#plot the ROC curve
png(file = "../figures/reviews2016_jan_rf_ROC.png")
plot(roc.perf, main = "ROC for Reviews in 2016 for the January Cohort Based on
     Recency and Frequency" , sub = "AUC = 0.9331")
abline(a=0, b= 1)
dev.off

#find the area under the roc curve

auc_rf <- performance(ROCR_rf, measure = "auc")
auc_rf@y.values
#0.9331258

#add in additional features

tree_all <- rpart(has_reviews_2016 ~  TV + Internet + Wireless.Internet +
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
                  data = jan_train, 
                  control = rpart.control(cp = 0.001))
printcp(tree_all)
bestcp_all <- tree_all$cptable[which.min(tree_all$cptable[,"xerror"]), "CP"]
#prune tree using best cp
tree_pruned_all <- prune(tree_all, cp = bestcp_all)

#plots
plot(tree_pruned_all, uniform = TRUE)
text(tree_pruned_all, cex = 0.8, use.n = TRUE, xpd = TRUE)
prp(tree_pruned_all, faclen = 0, cex = 0.8, extra = 1)

#use the tree_pruned_rf to predict on the test set
predict_all <- predict(tree_pruned_all, jan_test)
ROCR_all <- prediction(predict_all, jan_test$has_reviews_2016)
roc_perf_all <- performance(ROCR_all, measure = "tpr", x.measure = "fpr")
#plot the ROC curve
plot(roc_perf_all)
abline(a=0, b= 1)
#find the area under the roc curve
auc_all <- performance(ROCR_all, measure = "auc")
auc_all@y.values
#0.946272

#just using the amenities, recency frequency, reviews, and price
tree <- rpart(has_reviews_2016 ~ TV + Internet + Wireless.Internet +
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
              data = jan_train, 
              control = rpart.control(cp = 0.001))
printcp(tree)
bestcp <- tree$cptable[which.min(tree$cptable[,"xerror"]), "CP"]
#prune tree using best cp
tree_pruned <- prune(tree, cp = bestcp)

#plots
plot(tree_pruned, uniform = TRUE)
text(tree_pruned, cex = 0.8, use.n = TRUE, xpd = TRUE)

png(file = "../figures/reviews2016_jan_rf_rv_p_amen.png")
tree_all <- prp(tree_pruned, main = "Predicting Reviews in 2016 for the January 
Cohort Using Amenities, 
Recency, Frequency, Reviews, and Price", faclen = 0, cex = 0.8, extra = 1)
dev.off()

#use the tree_pruned_rf to predict on the test set
predict <- predict(tree_pruned, jan_test)
ROCR_tree <- prediction(predict, jan_test$has_reviews_2016)
roc_perf <- performance(ROCR_tree, measure = "tpr", x.measure = "fpr")
#plot the ROC curve
png(file = "../figures/reviews2016_jan_rf_rv_p_amen_ROC.png")
plot(roc_perf, main = "ROC for Predicting Reviews in 2016 for the January Cohort
     Using Amenities, Recency, Frequency, Reviews and Price", sub = "AUC = 0.946")
abline(a=0, b= 1)
dev.off()
#find the area under the roc curve
auc <- performance(ROCR_tree, measure = "auc")
auc@y.values
#0.9457