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

#look at only ammenities

tree_amen <- rpart(has_reviews_2016 ~  TV + Internet + Wireless.Internet +
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
                    translation.missing..en.hosting_amenity_50,
                  data = jan_train, 
                  control = rpart.control(cp = 0.001))
printcp(tree_amen)
bestcp_amen <- tree_amen$cptable[which.min(tree_amen$cptable[,"xerror"]), "CP"]
#prune tree using best cp
tree_pruned_amen <- prune(tree_amen, cp = bestcp_amen)

#plots
plot(tree_pruned_amen, uniform = TRUE)
text(tree_pruned_amen, cex = 0.8, use.n = TRUE, xpd = TRUE)
prp(tree_pruned_amen, faclen = 0, cex = 0.8, extra = 1)

#use the tree_pruned_rf to predict on the test set
predict_amen <- predict(tree_pruned_amen, jan_test)
ROCR_amen <- prediction(predict_amen, jan_test$has_reviews_2016)
roc_perf_amen <- performance(ROCR_amen, measure = "tpr", x.measure = "fpr")
#plot the ROC curve
plot(roc_perf_amen)
abline(a=0, b= 1)
#find the area under the roc curve
auc_amen <- performance(ROCR_amen, measure = "auc")
auc_amen@y.values
#0.946272


#look at only verifications

tree_ver <- rpart(has_reviews_2016 ~  email + phone +
                    facebook + reviews + kba + google + jumio + sent_id + 
                    linkedin +
                    manual_offline + manual_online + weibo + photographer + 
                    None +
                    amex + verifications_count,
                   data = jan_train, 
                   control = rpart.control(cp = 0.001))
printcp(tree_ver)
bestcp_ver <- tree_ver$cptable[which.min(tree_ver$cptable[,"xerror"]), "CP"]
#prune tree using best cp
tree_pruned_ver <- prune(tree_ver, cp = bestcp_ver)

#plots
plot(tree_pruned_ver, uniform = TRUE)
text(tree_pruned_ver, cex = 0.8, use.n = TRUE, xpd = TRUE)
prp(tree_pruned_ver, faclen = 0, cex = 0.8, extra = 1)

#use the tree_pruned_rf to predict on the test set
predict_ver <- predict(tree_pruned_ver, jan_test)
ROCR_ver <- prediction(predict_ver, jan_test$has_reviews_2016)
roc_perf_ver <- performance(ROCR_ver, measure = "tpr", x.measure = "fpr")
#plot the ROC curve
plot(roc_perf_ver)
abline(a=0, b= 1)
#find the area under the roc curve
auc_ver <- performance(ROCR_ver, measure = "auc")
auc_ver@y.values
#0.946272


#add in ammenities and verifications to the recency frequency and reviews

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

#text data
tree_word <- rpart(has_reviews_2016 ~ word_stay + word_apartment + word_great + 
                     word_place + word_host + word_clean + word_location + word_nice + word_room + 
                     word_subway + word_recommend + word_de + word_comfortable + word_time + 
                     word_good + word_easy + word_perfect + word_close + word_restaurant + 
                     word_neighborhood + word_la + word_nyc + word_helpful + word_bed + 
                     word_manhattan + word_experience + word_made + word_home + word_walk + 
                     word_area + word_york + word_friendly + word_le + word_lot + word_check + 
                     word_city + word_est + word_need + word_super + word_en + word_quiet + 
                     word_back + word_feel + word_welcome + word_convenient + word_station + 
                     word_wonderful + word_space + word_enjoy + word_night + word_located + 
                     word_love + word_arrival + word_day + word_brooklyn + word_arrive + word_felt + 
                     word_amazing + word_bathroom + word_safe + word_park + word_highly + word_trs + 
                     word_kitchen + word_street + word_block + word_house + word_nous + word_lovely + 
                     word_days + word_airbnb + word_question + word_el + word_shop + word_didn + 
                     word_spacious + word_make + word_quick + word_bar + word_people + 
                     word_accommodating + word_walking + word_minutes + word_train + word_beautiful + 
                     word_key + word_ny + word_central + word_small + word_muy + word_visit + 
                     word_building + word_es + word_excellent + word_appartement + word_metro + 
                     word_bedroom + word_trip + word_times + word_kind,                 
                   data = jan_train, 
                   control = rpart.control(cp = 0.001))
printcp(tree_word)
bestcp_word <- tree_word$cptable[which.min(tree_word$cptable[,"xerror"]), "CP"]
#prune tree using best cp
tree_pruned_word <- prune(tree_word, cp = bestcp_word)

#plots
plot(tree_word, uniform = TRUE)
text(tree_word, cex = 0.8, use.n = TRUE, xpd = TRUE)
prp(tree_pruned_word, main = "Predicting Reviews in 2016 for January Cohort
    Using Words", faclen = 0, cex = 0.8, extra = 1)

#use the tree_pruned_rf to predict on the test set
word_predict <- predict(tree_pruned_word, jan_test)
ROCR_word <- prediction(word_predict, jan_test$has_reviews_2016)
roc_perf_word <- performance(ROCR_word, measure = "tpr", x.measure = "fpr")
plot(roc_perf_word, main = "ROC Predicting Reviews in 2016 for January Cohort Using
     Words")
abline(a=0, b= 1)
auc_word <- performance(ROCR_word, measure = "auc")
auc_word@y.values
#0.857

#text data
tree_word_all <- rpart(has_reviews_2016 ~ TV + Internet + Wireless.Internet +
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
                         room_type +word_stay + word_apartment + word_great + 
                     word_place + word_host + word_clean + word_location + word_nice + word_room + 
                     word_subway + word_recommend + word_de + word_comfortable + word_time + 
                     word_good + word_easy + word_perfect + word_close + word_restaurant + 
                     word_neighborhood + word_la + word_nyc + word_helpful + word_bed + 
                     word_manhattan + word_experience + word_made + word_home + word_walk + 
                     word_area + word_york + word_friendly + word_le + word_lot + word_check + 
                     word_city + word_est + word_need + word_super + word_en + word_quiet + 
                     word_back + word_feel + word_welcome + word_convenient + word_station + 
                     word_wonderful + word_space + word_enjoy + word_night + word_located + 
                     word_love + word_arrival + word_day + word_brooklyn + word_arrive + word_felt + 
                     word_amazing + word_bathroom + word_safe + word_park + word_highly + word_trs + 
                     word_kitchen + word_street + word_block + word_house + word_nous + word_lovely + 
                     word_days + word_airbnb + word_question + word_el + word_shop + word_didn + 
                     word_spacious + word_make + word_quick + word_bar + word_people + 
                     word_accommodating + word_walking + word_minutes + word_train + word_beautiful + 
                     word_key + word_ny + word_central + word_small + word_muy + word_visit + 
                     word_building + word_es + word_excellent + word_appartement + word_metro + 
                     word_bedroom + word_trip + word_times + word_kind,                 
                   data = jan_train, 
                   control = rpart.control(cp = 0.001))
printcp(tree_word_all)
bestcp_word_all <- tree_word_all$cptable[which.min(tree_word_all$cptable[,"xerror"]), "CP"]
#prune tree using best cp
tree_pruned_word_all <- prune(tree_word_all, cp = bestcp_word_all)

#plots
plot(tree_word_all, uniform = TRUE)
text(tree_word_all, cex = 0.8, use.n = TRUE, xpd = TRUE)
prp(tree_pruned_word_all, faclen = 0, cex = 0.8, extra = 1)

#use the tree_pruned_rf to predict on the test set
word_predict_all <- predict(tree_pruned_word_all, jan_test)
ROCR_word_all <- prediction(word_predict_all, jan_test$has_reviews_2016)
roc_perf_word_all <- performance(ROCR_word_all, measure = "tpr", x.measure = "fpr")
plot(roc_perf_word_all, main = "ROC Predicting Reviews in 2016 for January Cohort Using
     Words")
abline(a=0, b= 1)
auc_word_all <- performance(ROCR_word_all, measure = "auc")
auc_word_all@y.values
#0.857

#just purged

tree_purged <- rpart(has_reviews_2016 ~ purged, 
                 data = jan_train, 
                 control = rpart.control(cp = 0.001))

#view the tree
printcp(tree_purged)

#find the best cp in the tree
bestcp_purged <- tree_purged$cptable[which.min(tree_purged$cptable[,"xerror"]), "CP"]

#prune tree using best cp
tree_pruned_purged <- prune(tree_purged, cp = bestcp_purged)

#plots
plot(tree_pruned_purged, uniform = TRUE)
text(tree_pruned_purged, cex = 0.8, use.n = TRUE, xpd = TRUE)
prp(tree_pruned_purged, faclen = 0, cex = 0.8, extra = 1)

#use the tree_pruned_rf to predict on the test set
purged_predict <- predict(tree_pruned_purged, jan_test)
ROCR_purged <- prediction(purged_predict, jan_test$has_reviews_2016)
roc.purg <- performance(ROCR_purged, measure = "tpr", x.measure = "fpr")

#plot the ROC curve
plot(roc.purg)
abline(a=0, b= 1)

#find the area under the roc curve

auc_purged <- performance(ROCR_purged, measure = "auc")
auc_purged@y.values
#0.9331258

###just using the amenities, recency frequency, reviews, and price
tree_all_purged <- rpart(has_reviews_2016 ~ TV + Internet + Wireless.Internet +
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
                last_rating + mean_price + max_price + min_price +room_type + 
                purged, 
              data = jan_train, 
              control = rpart.control(cp = 0.001))
printcp(tree_all_purged)
bestcp_all_purged <- tree_all_purged$cptable[which.min(tree_all_purged$cptable[,"xerror"]), "CP"]
#prune tree using best cp
tree_pruned_all_purged <- prune(tree_all_purged, cp = bestcp_all_purged)

#plots
plot(tree_pruned_all_purged, uniform = TRUE)
text(tree_pruned_all_purged, cex = 0.8, use.n = TRUE, xpd = TRUE)

prp(tree_pruned_all_purged, faclen = 0, cex = 0.8, extra = 1)

#use the tree_pruned_rf to predict on the test set
predict_all_purged <- predict(tree_pruned_all_purged, jan_test)
ROCR_tree_all_purged <- prediction(predict_all_purged, jan_test$has_reviews_2016)
roc_perf_all_purged <- performance(ROCR_tree_all_purged, measure = "tpr", x.measure = "fpr")
#plot the ROC curve
plot(roc_perf_all_purged)
abline(a=0, b= 1)
#find the area under the roc curve
auc_all_purged <- performance(ROCR_tree_all_purged, measure = "auc")
auc_all_purged@y.values
#0.9457