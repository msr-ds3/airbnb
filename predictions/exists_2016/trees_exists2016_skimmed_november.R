#create trees for the skimmed listings for all listings in the November 
#cohort

#install.packages("rpart")
#install.packages("rpart.plot")
#install.packages("ROCR")

library(rpart)
library(rpart.plot)
library(readr)
library(dplyr)
library(ROCR)

#load the test and train data for November
#this file was created by  "create_test_train_nov_cohort.R"

load("../create_test_train/skimmed_nov_test_train.RData")

set.seed(123)

#######################Recency Frequency Tree###############################
#exists ~ recency frequency
#tree formula using the training set
tree_rf <- rpart(exist_in_2016 ~ host_listings_count + host_duration + 
                   first_seen_month + last_seen_month + 
                   listing_recency_2015_weeks + scrap_duration + 
                   total_occ_2015 + review_recency_2015_weeks + 
                   is_superhost_2015 + is_superhost_count_2015, 
                 data = skimmed_nov_train, 
                 control = rpart.control(cp = 0.001))
#print the summary of the tree
printcp(tree_rf)
bestcp_rf <- tree_rf$cptable[which.min(tree_rf$cptable[,"xerror"]), "CP"]
#prune tree using best cp
tree_pruned_rf <- prune(tree_rf, cp = bestcp_rf)

#plots
plot(tree_pruned_rf, uniform = TRUE)
text(tree_pruned_rf, cex = 0.8, use.n = TRUE, xpd = TRUE)
prp(tree_pruned_rf, faclen = 0, cex = 0.8, extra = 1)

#use the tree_pruned_rf to predict on the test set
rf_predict <- predict(tree_pruned_rf, skimmed_nov_test)
ROCR_rf <- prediction(rf_predict, skimmed_nov_test$exist_in_2016)
roc.perf <- performance(ROCR_rf, measure = "tpr", x.measure = "fpr")
#plot the ROC curve
plot(roc.perf)
abline(a=0, b= 1)
#find the area under the roc curve
auc_rf <- performance(ROCR_rf, measure = "auc")
auc_rf@y.values
#0.884

#########################REVIEWS TREE########################################
#exists_2016 ~ reviews
tree_rv <- rpart(exist_in_2016 ~ first_review_year + last_review_year +
                   num_as_of_2015 + num_reviews_in_2015 + has_reviews_2015 +
                   first_review_month_2015 + last_review_month_2015 + 
                   last_rating, 
                 data = skimmed_nov_train, 
                 control = rpart.control(cp = 0.001))
printcp(tree_rv)
bestcp_rv <- tree_rv$cptable[which.min(tree_rv$cptable[,"xerror"]), "CP"]
#prune tree using best cp
tree_pruned_rv <- prune(tree_rv, cp = bestcp_rv)

#plots
plot(tree_pruned_rv, uniform = TRUE)
text(tree_pruned_rv, cex = 0.8, use.n = TRUE, xpd = TRUE)
prp(tree_pruned_rv, faclen = 0, cex = 0.8, extra = 1)

#use the tree_pruned_rf to predict on the test set
rv_predict <- predict(tree_pruned_rv, skimmed_nov_test)
ROCR_rv <- prediction(rv_predict, skimmed_nov_test$exist_in_2016)
roc_perf_rv <- performance(ROCR_rv, measure = "tpr", x.measure = "fpr")
plot(roc_perf_rv)
abline(a=0, b= 1)
auc_rv <- performance(ROCR_rv, measure = "auc")
auc_rv@y.values
#0.724

##########################PRICE TREE#########################################
#exists ~ price
tree_p <- rpart(exist_in_2016 ~ min_price + max_price + mean_price, 
                data = skimmed_nov_train, 
                control = rpart.control(cp = 0.001))
printcp(tree_p)
bestcp_p <- tree_p$cptable[which.min(tree_p$cptable[,"xerror"]), "CP"]
#prune tree using best cp
tree_pruned_p <- prune(tree_p, cp = bestcp_p)

#plots
prp(tree_pruned_p, faclen = 0, cex = 0.8, extra = 1)

#use the tree_pruned_rf to predict on the test set
p_predict <- predict(tree_pruned_p, skimmed_nov_test)
ROCR_p <- prediction(p_predict, skimmed_nov_test$exist_in_2016)
roc_perf_p <- performance(ROCR_p, measure = "tpr", x.measure = "fpr")
#plot the roc curve
plot(roc_perf_p)
abline(a=0, b= 1)
#find the area under the roc curve
auc_p <- performance(ROCR_p, measure = "auc")
auc_p@y.values
#0.5

#########################RECENCY FREQUENCY & REVIEWS TREE#####################
#exists_2016 ~ rf + rv
tree_rf_rv <- rpart(exist_in_2016 ~ host_listings_count + host_duration + 
                      first_seen_month + last_seen_month + 
                      listing_recency_2015_weeks + scrap_duration + 
                      total_occ_2015 + review_recency_2015_weeks + 
                      is_superhost_2015 + is_superhost_count_2015 + 
                      first_review_year + last_review_year +
                      num_as_of_2015 + num_reviews_in_2015 + has_reviews_2015 +
                      first_review_month_2015 + last_review_month_2015 + 
                      last_rating, 
                    data = skimmed_nov_train, 
                    control = rpart.control(cp = 0.001))
printcp(tree_rf_rv)
bestcp_rf_rv <- tree_rf_rv$cptable[which.min(tree_rf_rv$cptable[,"xerror"]), "CP"]
#prune tree using best cp
tree_pruned_rf_rv <- prune(tree_rf_rv, cp = bestcp_rf_rv)

#plots
plot(tree_pruned_rf_rv, uniform = TRUE)
text(tree_pruned_rf_rv, cex = 0.8, use.n = TRUE, xpd = TRUE)
prp(tree_pruned_rf_rv, faclen = 0, cex = 0.8, extra = 1)

#use the tree_pruned_rf to predict on the test set
rf_rv_predict <- predict(tree_pruned_rf_rv, skimmed_nov_test)
ROCR_rf_rv <- prediction(rf_rv_predict, skimmed_nov_test$exist_in_2016)
roc_perf_rf_rv <- performance(ROCR_rf_rv, measure = "tpr", x.measure = "fpr")
#plot the ROC curve
plot(roc_perf_rf_rv)
abline(a=0, b= 1)
#find the area under the roc curve
auc_rf_rv <- performance(ROCR_rf_rv, measure = "auc")
auc_rf_rv@y.values
#0.884

#################RECENCY FREQUENCY & PRICE TREE###############################
#exists ~ rf + p
tree_rf_p <- rpart(exist_in_2016 ~ host_listings_count + host_duration + 
                     first_seen_month + last_seen_month + 
                     listing_recency_2015_weeks + scrap_duration + 
                     total_occ_2015 + review_recency_2015_weeks + 
                     is_superhost_2015 + is_superhost_count_2015 + 
                     min_price + max_price + mean_price, 
                   data = skimmed_nov_train, 
                   control = rpart.control(cp = 0.001))
printcp(tree_rf_p)
bestcp_rf_p <- tree_rf_p$cptable[which.min(tree_rf_p$cptable[,"xerror"]), "CP"]
#prune tree using best cp
tree_pruned_rf_p <- prune(tree_rf_p, cp = bestcp_rf_p)

#plots
plot(tree_pruned_rf_p, uniform = TRUE)
text(tree_pruned_rf_p, cex = 0.8, use.n = TRUE, xpd = TRUE)
prp(tree_pruned_rf_p, faclen = 0, cex = 0.8, extra = 1)

#use the tree_pruned_rf to predict on the test set
rf_p_predict <- predict(tree_pruned_rf_p, skimmed_nov_test)
ROCR_rf_p <- prediction(rf_p_predict, skimmed_nov_test$exist_in_2016)
roc_perf_rf_p <- performance(ROCR_rf_p, measure = "tpr", x.measure = "fpr")
#plot the ROC curve
plot(roc_perf_rf_p)
abline(a=0, b= 1)
#find the area under the roc curve
auc_rf_p <- performance(ROCR_rf_p, measure = "auc")
auc_rf_p@y.values
#0.884

########################REVIEWS & PRICE TREE#################################
#exists ~ rv + p
tree_rv_p <- rpart(exist_in_2016 ~ first_review_year + last_review_year +
                     num_as_of_2015 + num_reviews_in_2015 + has_reviews_2015 +
                     first_review_month_2015 + last_review_month_2015 + 
                     review_recency_2015_weeks + last_rating + min_price + 
                     max_price + mean_price, 
                   data = skimmed_nov_train, 
                   control = rpart.control(cp = 0.001))
printcp(tree_rv_p)
bestcp_rv_p<- tree_rv_p$cptable[which.min(tree_rv_p$cptable[,"xerror"]), "CP"]
#prune tree using best cp
tree_pruned_rv_p <- prune(tree_rv_p, cp = bestcp_rv_p)

#plots
plot(tree_pruned_rv_p, uniform = TRUE)
text(tree_pruned_rv_p, cex = 0.8, use.n = TRUE, xpd = TRUE)
prp(tree_pruned_rv_p, faclen = 0, cex = 0.8, extra = 1)

#use the tree_pruned_rf to predict on the test set
rv_p_predict <- predict(tree_pruned_rv_p, skimmed_nov_test)
ROCR_rv_p <- prediction(rv_p_predict, skimmed_nov_test$exist_in_2016)
roc_perf_rv_p <- performance(ROCR_rv_p, measure = "tpr", x.measure = "fpr")
plot(roc_perf_rv_p)
abline(a=0, b= 1)
auc_rv_p <- performance(ROCR_rv_p, measure = "auc")
auc_rv_p@y.values
#0.717

####################RECENCY, FREQUENCY, REVIEWS, AND PRICE####################
#exists ~ rf + rv + p
tree_rf_rv_p <- rpart(exist_in_2016 ~ host_listings_count + host_duration + 
                        first_seen_month + last_seen_month + 
                        listing_recency_2015_weeks + scrap_duration + 
                        total_occ_2015 + review_recency_2015_weeks + 
                        is_superhost_2015 + is_superhost_count_2015 + 
                        first_review_year + last_review_year +
                        num_as_of_2015 + num_reviews_in_2015 + has_reviews_2015 +
                        first_review_month_2015 + last_review_month_2015 + 
                        last_rating + mean_price + max_price + min_price, 
                      data = skimmed_nov_train, 
                      control = rpart.control(cp = 0.001))
printcp(tree_rf_rv_p)
bestcp_rf_rv_p <- tree_rf_rv_p$cptable[which.min(tree_rf_rv_p$cptable[,"xerror"]), "CP"]
#prune tree using best cp
tree_pruned_rf_rv_p <- prune(tree_rf_rv_p, cp = bestcp_rf_rv_p)

#plots
plot(tree_pruned_rf_rv_p, uniform = TRUE)
text(tree_pruned_rf_rv_p, cex = 0.8, use.n = TRUE, xpd = TRUE)
prp(tree_pruned_rf_rv_p, faclen = 0, cex = 0.8, extra = 1)

#use the tree_pruned_rf to predict on the test set
rf_rv_p_predict <- predict(tree_pruned_rf_rv_p, skimmed_nov_test)
ROCR_rf_rv_p <- prediction(rf_rv_p_predict, skimmed_nov_test$exist_in_2016)
roc_perf_rf_rv_p <- performance(ROCR_rf_rv_p, measure = "tpr", x.measure = "fpr")
#plot the ROC curve
plot(roc_perf_rf_rv_p)
abline(a=0, b= 1)
#find the area under the roc curve
auc_rf_rv_p <- performance(ROCR_rf_rv_p, measure = "auc")
auc_rf_rv_p@y.values
#0.884

#############################AMENITIES TREE##################################
#exists ~ amen
tree_amen <- rpart(exist_in_2016 ~  TV + Internet + Wireless.Internet +
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
                   data = skimmed_nov_train, 
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
predict_amen <- predict(tree_pruned_amen, skimmed_nov_test)
ROCR_amen <- prediction(predict_amen, skimmed_nov_test$exist_in_2016)
roc_perf_amen <- performance(ROCR_amen, measure = "tpr", x.measure = "fpr")
#plot the ROC curve
plot(roc_perf_amen)
abline(a=0, b= 1)
#find the area under the roc curve
auc_amen <- performance(ROCR_amen, measure = "auc")
auc_amen@y.values
#0.643

###############AMMENITIES, RECENCY FREQUENCY, REVIEWS, PRICE####################
#exists ~ amen + rf + rv + p
tree <- rpart(exist_in_2016 ~ TV + Internet + Wireless.Internet +
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
              data = skimmed_nov_train, 
              control = rpart.control(cp = 0.001))
printcp(tree)
bestcp <- tree$cptable[which.min(tree$cptable[,"xerror"]), "CP"]
#prune tree using best cp
tree_pruned <- prune(tree, cp = bestcp)

#plots
plot(tree_pruned, uniform = TRUE)
text(tree_pruned, cex = 0.8, use.n = TRUE, xpd = TRUE)

tree_all <- prp(tree_pruned,faclen = 0, cex = 0.8, extra = 1)

#use the tree_pruned_rf to predict on the test set
predict <- predict(tree_pruned, skimmed_nov_test)
ROCR_tree <- prediction(predict, skimmed_nov_test$exist_in_2016)
roc_perf <- performance(ROCR_tree, measure = "tpr", x.measure = "fpr")
#plot the ROC curve
plot(roc_perf)
abline(a=0, b= 1)
#find the area under the roc curve
auc <- performance(ROCR_tree, measure = "auc")
auc@y.values
#0.880

################VERIFICATION TREE#############################################
tree_ver <- rpart(exist_in_2016 ~  email + phone +
                    facebook + reviews + kba + google + jumio + sent_id + 
                    linkedin +
                    manual_offline + manual_online + weibo + photographer + 
                    None +
                    amex + verifications_count,
                  data = skimmed_nov_train, 
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
predict_ver <- predict(tree_pruned_ver, skimmed_nov_test)
ROCR_ver <- prediction(predict_ver, skimmed_nov_test$exist_in_2016)
roc_perf_ver <- performance(ROCR_ver, measure = "tpr", x.measure = "fpr")
#plot the ROC curve
plot(roc_perf_ver)
abline(a=0, b= 1)
#find the area under the roc curve
auc_ver <- performance(ROCR_ver, measure = "auc")
auc_ver@y.values
#0.657

############RECENCY FREQUENCY, REVIEWS, AMMENITIES, VERIFICATION##############
tree_all <- rpart(exist_in_2016 ~  TV + Internet + Wireless.Internet +
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
                  data = skimmed_nov_train, 
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
predict_all <- predict(tree_pruned_all, skimmed_nov_test)
ROCR_all <- prediction(predict_all, skimmed_nov_test$exist_in_2016)
roc_perf_all <- performance(ROCR_all, measure = "tpr", x.measure = "fpr")
#plot the ROC curve
plot(roc_perf_all)
abline(a=0, b= 1)
#find the area under the roc curve
auc_all <- performance(ROCR_all, measure = "auc")
auc_all@y.values
#0.880

#######################WORDS TREE#############################################
#exists ~ words
tree_word <- rpart(exist_in_2016 ~ word_stay + word_apartment + word_great + 
                     word_place + word_host + word_clean + word_location + 
                     word_nice + word_room + 
                     word_subway + word_recommend + word_de + word_comfortable + 
                     word_time + 
                     word_good + word_easy + word_perfect + word_close + 
                     word_restaurant + 
                     word_neighborhood + word_la + word_nyc + word_helpful + 
                     word_bed + 
                     word_manhattan + word_experience + word_made + word_home + 
                     word_walk + 
                     word_area + word_york + word_friendly + word_le + 
                     word_lot + word_check + 
                     word_city + word_est + word_need + word_super + word_en + 
                     word_quiet + 
                     word_back + word_feel + word_welcome + word_convenient + 
                     word_station + 
                     word_wonderful + word_space + word_enjoy + word_night + 
                     word_located + 
                     word_love + word_arrival + word_day + word_brooklyn + 
                     word_arrive + word_felt + 
                     word_amazing + word_bathroom + word_safe + word_park + 
                     word_highly + word_trs + 
                     word_kitchen + word_street + word_block + word_house + 
                     word_nous + word_lovely + 
                     word_days + word_airbnb + word_question + word_el + 
                     word_shop + word_didn + 
                     word_spacious + word_make + word_quick + word_bar + 
                     word_people + 
                     word_accommodating + word_walking + word_minutes + 
                     word_train + word_beautiful + 
                     word_key + word_ny + word_central + word_small + word_muy + 
                     word_visit + 
                     word_building + word_es + word_excellent + 
                     word_appartement + word_metro + 
                     word_bedroom + word_trip + word_times + word_kind,                 
                   data = skimmed_nov_train, 
                   control = rpart.control(cp = 0.001))
printcp(tree_word)
bestcp_word <- tree_word$cptable[which.min(tree_word$cptable[,"xerror"]), "CP"]
#prune tree using best cp
tree_pruned_word <- prune(tree_word, cp = bestcp_word)

#plots
plot(tree_word, uniform = TRUE)
text(tree_word, cex = 0.8, use.n = TRUE, xpd = TRUE)
prp(tree_pruned_word, faclen = 0, cex = 0.8, extra = 1)

#use the tree_pruned_rf to predict on the test set
word_predict <- predict(tree_pruned_word, skimmed_nov_test)
ROCR_word <- prediction(word_predict, skimmed_nov_test$exist_in_2016)
roc_perf_word <- performance(ROCR_word, measure = "tpr", x.measure = "fpr")
plot(roc_perf_word)
abline(a=0, b= 1)
auc_word <- performance(ROCR_word, measure = "auc")
auc_word@y.values
#0.652

########RECENCY FREQUENCY, REVIEWS, AMMENITIES, VERIFICATION & TEXT#############
#exists ~ rf + rv + amen + ver + text 
tree_all <- rpart(exist_in_2016 ~  TV + Internet + Wireless.Internet +
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
                    amex + verifications_count + ord_stay + word_apartment + 
                    word_great + 
                    word_place + word_host + word_clean + word_location + 
                    word_nice + word_room + 
                    word_subway + word_recommend + word_de + word_comfortable + 
                    word_time + 
                    word_good + word_easy + word_perfect + word_close + 
                    word_restaurant + 
                    word_neighborhood + word_la + word_nyc + word_helpful + 
                    word_bed + 
                    word_manhattan + word_experience + word_made + word_home + 
                    word_walk + 
                    word_area + word_york + word_friendly + word_le + word_lot + 
                    word_check + 
                    word_city + word_est + word_need + word_super + word_en + 
                    word_quiet + 
                    word_back + word_feel + word_welcome + word_convenient + 
                    word_station + 
                    word_wonderful + word_space + word_enjoy + word_night + 
                    word_located + 
                    word_love + word_arrival + word_day + word_brooklyn + 
                    word_arrive + word_felt + 
                    word_amazing + word_bathroom + word_safe + word_park + 
                    word_highly + word_trs + 
                    word_kitchen + word_street + word_block + word_house + 
                    word_nous + word_lovely + 
                    word_days + word_airbnb + word_question + word_el + 
                    word_shop + word_didn + 
                    word_spacious + word_make + word_quick + word_bar + 
                    word_people + 
                    word_accommodating + word_walking + word_minutes + 
                    word_train + word_beautiful + 
                    word_key + word_ny + word_central + word_small + word_muy + 
                    word_visit + 
                    word_building + word_es + word_excellent + 
                    word_appartement + word_metro + 
                    word_bedroom + word_trip + word_times + word_kind, 
                  data = skimmed_nov_train, 
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
predict_all <- predict(tree_pruned_all, skimmed_nov_test)
ROCR_all <- prediction(predict_all, skimmed_nov_test$exist_in_2016)
roc_perf_all <- performance(ROCR_all, measure = "tpr", x.measure = "fpr")
#plot the ROC curve
plot(roc_perf_all)
abline(a=0, b= 1)
#find the area under the roc curve
auc_all <- performance(ROCR_all, measure = "auc")
auc_all@y.values
#0.880

########################PURGED TREE############################################
# exists ~ purged
tree_purged <- rpart(exist_in_2016 ~ purged, 
                     data = skimmed_nov_train, 
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
purged_predict <- predict(tree_pruned_purged, skimmed_nov_test)
ROCR_purged <- prediction(purged_predict, skimmed_nov_test$exist_in_2016)
roc.purg <- performance(ROCR_purged, measure = "tpr", x.measure = "fpr")
#plot the ROC curve
plot(roc.purg)
abline(a=0, b= 1)
#find the area under the roc curve
auc_purged <- performance(ROCR_purged, measure = "auc")
auc_purged@y.values
#0.5

##############################LOCATION TREE####################################
# exists ~ location
tree_location <- rpart(exist_in_2016 ~ #neighbourhood_cleansed +
                         neighbourhood_group_cleansed + #smart_location + 
                         #city + 
                         zipcode + market + #country + 
                         is_location_exact,  
                       data = skimmed_nov_train, 
                       control = rpart.control(cp = 0.001))
#view the tree
printcp(tree_location)
#find the best cp in the tree
bestcp_location <- tree_location$cptable[which.min(tree_location$cptable[,"xerror"]), "CP"]
#prune tree using best cp
tree_pruned_location <- prune(tree_location, cp = bestcp_location)
#plots
plot(tree_pruned_location, uniform = TRUE)
text(tree_pruned_location, cex = 0.8, use.n = TRUE, xpd = TRUE)
prp(tree_pruned_location, faclen = 0, cex = 0.8, extra = 1)
#use the tree_pruned_rf to predict on the test set
location_predict <- predict(tree_pruned_location, skimmed_nov_test)
ROCR_location <- prediction(location_predict, skimmed_nov_test$exist_in_2016)
roc.loc <- performance(ROCR_location, measure = "tpr", x.measure = "fpr")
#plot the ROC curve
plot(roc.loc)
abline(a=0, b= 1)
#find the area under the roc curve
auc_location <- performance(ROCR_location, measure = "auc")
auc_location@y.values
#0.509

######################RF, RV, AMEN, VER, Location#############################
# exists ~ rf + rv + amen + location + ver
tree_all <- rpart(exist_in_2016 ~  TV + Internet + Wireless.Internet +
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
                    amex + verifications_count+ #neighbourhood_cleansed +
                    neighbourhood_group_cleansed + #smart_location + 
                    #city + 
                    zipcode + market + #country + 
                    is_location_exact,  
                  data = skimmed_nov_train, 
                  control = rpart.control(cp = 0.001))
#view the tree
printcp(tree_all)
#find the best cp in the tree
bestcp_all <- tree_all$cptable[which.min(tree_all$cptable[,"xerror"]), "CP"]
#prune tree using best cp
tree_pruned_all <- prune(tree_all, cp = bestcp_all)
#plots
plot(tree_pruned_all, uniform = TRUE)
text(tree_pruned_all, cex = 0.8, use.n = TRUE, xpd = TRUE)
prp(tree_pruned_all, faclen = 0, cex = 0.8, extra = 1)
#use the tree_pruned_rf to predict on the test set
all_predict <- predict(tree_pruned_all, skimmed_nov_test)
ROCR_all <- prediction(all_predict, skimmed_nov_test$exist_in_2016)
roc_all <- performance(ROCR_all, measure = "tpr", x.measure = "fpr")
#plot the ROC curve
plot(roc_all)
abline(a=0, b= 1)
#find the area under the roc curve
auc_all <- performance(ROCR_all, measure = "auc")
auc_all@y.values
#0.880

###########AMMENITIES, VERIFICATION, WORDS, LOCATION, AND ROOM TYPE TREE######
#EXISTS ~ AMEN + VER + WORDS + LOCATION + ROOM TYPE

tree_non_rfrv <- rpart(exist_in_2016 ~ min_price + 
                         max_price + mean_price + TV + Internet + Wireless.Internet +
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
                         facebook + reviews + kba + google + jumio + sent_id + 
                         linkedin +
                         manual_offline + manual_online + weibo + photographer + 
                         None +
                         amex + verifications_count + room_type +#neighbourhood_cleansed +
                         neighbourhood_group_cleansed + #smart_location + 
                         #city + 
                         zipcode + market + #country + 
                         is_location_exact + word_stay + word_apartment + 
                         word_great + 
                         word_place + word_host + word_clean + word_location + 
                         word_nice + word_room + 
                         word_subway + word_recommend + word_de + word_comfortable + 
                         word_time + 
                         word_good + word_easy + word_perfect + word_close + 
                         word_restaurant + 
                         word_neighborhood + word_la + word_nyc + word_helpful + 
                         word_bed + 
                         word_manhattan + word_experience + word_made + word_home + 
                         word_walk + 
                         word_area + word_york + word_friendly + word_le + word_lot + 
                         word_check + 
                         word_city + word_est + word_need + word_super + word_en + 
                         word_quiet + 
                         word_back + word_feel + word_welcome + word_convenient + 
                         word_station + 
                         word_wonderful + word_space + word_enjoy + word_night + 
                         word_located + 
                         word_love + word_arrival + word_day + word_brooklyn + 
                         word_arrive + word_felt + 
                         word_amazing + word_bathroom + word_safe + word_park + 
                         word_highly + word_trs + 
                         word_kitchen + word_street + word_block + word_house + 
                         word_nous + word_lovely + 
                         word_days + word_airbnb + word_question + word_el + 
                         word_shop + word_didn + 
                         word_spacious + word_make + word_quick + word_bar + 
                         word_people + 
                         word_accommodating + word_walking + word_minutes + 
                         word_train + word_beautiful + 
                         word_key + word_ny + word_central + word_small + word_muy + 
                         word_visit + 
                         word_building + word_es + word_excellent + 
                         word_appartement + word_metro + 
                         word_bedroom + word_trip + word_times + word_kind, 
                       data = skimmed_nov_train, 
                       control = rpart.control(cp = 0.01))
printcp(tree_non_rfrv)
bestcp_non<- tree_non_rfrv$cptable[which.min(tree_non_rfrv$cptable[,"xerror"]), "CP"]
#prune tree using best cp
tree_pruned_non <- prune(tree_non_rfrv, cp = bestcp_non)

#plots
plot(tree_pruned_non, uniform = TRUE)
text(tree_pruned_non, cex = 0.8, use.n = TRUE, xpd = TRUE)
prp(tree_pruned_non, faclen = 0, cex = 0.8, extra = 1)

#use the tree_pruned_rf to predict on the test set
non_predict <- predict(tree_pruned_non, skimmed_nov_test)
ROCR_non <- prediction(non_predict, skimmed_nov_test$exist_in_2016)
roc_perf_non <- performance(ROCR_non, measure = "tpr", x.measure = "fpr")
plot(roc_perf_non)
abline(a=0, b= 1)
auc_non <- performance(ROCR_non, measure = "auc")
auc_non@y.values
#0.657