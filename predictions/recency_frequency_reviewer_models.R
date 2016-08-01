library(readr)
library(rpart)
library(rpart.plot)
library(ROCR)
library(dplyr)
library(lubridate)

# expands nums from abbreviated to actual numbers
tot_count <- function(x, labs, digits, varlen)
{
  paste(labs, "\n\nn =", x$frame$n)
}

################################################################################
## Data Prep
################################################################################

reviewer_data <- read_csv("../raw_data/us_rev_data.csv", col_types = cols(word_na = "d"))
features <- read_csv("../raw_data/review_listing_features_full.csv")

reviewer_data <- mutate(reviewer_data, all_time_as_of_2015 = all_time_reviews - num_in_2016,
                        weight_lgl = num_in_2016 > 0)

before_2016 <- inner_join(reviewer_data, features, by = "reviewer_id")
# adding columns for all time reviews as of 2015 and number of places stayed
before_2016 <- mutate(before_2016, zipcode = as.character(zipcode))

# january cohort, 54508 obs
jan_2015 <- filter(before_2016, first_month >= as.Date("2015-01-01") &
                     first_month <= as.Date("2015-01-31"))

# num_in_2015 > 1
num_more_than_one <- filter(before_2016, num_in_2015 > 1)

################################################################################
## Sampling
################################################################################

# all (can only run recency frequency and words, lacks other features)
test <- sample_n(reviewer_data, size=0.2*nrow(reviewer_data))
train <- filter(reviewer_data, !reviewer_data$reviewer_id %in% test$reviewer_id)

train_true <- filter(train, weight_lgl == T)
train_false <- filter(train, weight_lgl == F)
train_oversample <- rbind(head(train_true, nrow(train_true)),
                          head(train_false, nrow(train_true)))

# january cohort
jan_test <- sample_n(jan_2015, size=0.2*nrow(jan_2015))
jan_train <- filter(jan_2015, !jan_2015$reviewer_id %in% jan_test$reviewer_id)

# !!! weights to 0 
# jan_train_true <- filter(jan_train, weight_lgl == T)
# jan_train_false <- filter(jan_train, weight_lgl == F)
# jan_train_oversample <- rbind(head(jan_train_true, nrow(jan_train_true)),
#                               head(jan_train_false, nrow(jan_train_true)))

# before 2016
before_test <- sample_n(before_2016, size=0.2*nrow(before_2016))
before_train <- filter(before_2016, !before_2016$reviewer_id %in% before_test$reviewer_id)

# !!! weights to 0 
# before_train_true <- filter(before_train, weight_lgl == T)
# before_train_false <- filter(before_train, weight_lgl == F)
# before_train_oversample <- rbind(head(before_train_true, nrow(before_train_true)),
#                                  head(before_train_false, nrow(before_train_true)))

# num_in_2015 > 1
num_test <- sample_n(num_more_than_one, size=0.2*nrow(num_more_than_one))
num_train <- filter(num_more_than_one, !num_more_than_one$reviewer_id %in% num_test$reviewer_id)

# !!! weights to 0 
# num_train_true <- filter(num_train, weight_lgl == T)
# num_train_false <- filter(num_train, weight_lgl == F)
# num_train_oversample <- rbind(head(num_train_true, nrow(num_train_true)),
#                               head(num_train_false, nrow(num_train_true)))

################################################################################
## Decision Trees
################################################################################

##### Recency Frequency

# function: pass train and test dataframes
get_fit_rf <- function(train_df, test_df) {
  fit_rf <- rpart(has_review_2016 ~ first_month +
                  first_diff_2015 +
                  last_month +
                  last_diff_2015 +
                  first_in_2015 +
                  first_month_in_2015 +
                  last_in_2015 +
                  last_month_in_2015 +
                  num_in_2015 +
                  all_time_as_of_2015,
                data = train_df, control = rpart.control(cp = 0.001, maxdepth = 5))
  printcp(fit_rf)
  bestcp_rf <- fit_rf$cptable[which.min(fit_rf$cptable[,"xerror"]), "CP"]
  
  #prune tree using best cp
  tree_pruned_rf <- prune(fit_rf, cp = bestcp_rf)
  prp(tree_pruned_rf, faclen = 0, cex = 0.8, node.fun=tot_count)
  
  # plot
  rpart.plot(tree_pruned_rf)
  
  # use predict to see the prediction on test set
  sample_predict_rf <- predict(tree_pruned_rf, test_df)
  
  # calculate AUC
  ROCR_rf <- prediction(sample_predict_rf, test_df$has_review_2016) 
  roc.perf_rf = performance(ROCR_rf, measure = "tpr", x.measure = "fpr")
  print(performance(ROCR_rf, measure = "auc"))
  plot(roc.perf_rf)
}

# calls
get_fit_rf(train, test)
get_fit_rf(train_oversample, test)

get_fit_rf(jan_train, jan_test)
get_fit_rf(jan_train_oversample, jan_test)

get_fit_rf(before_train, before_test)
get_fit_rf(before_train_oversample, before_test)

get_fit_rf(num_train, num_test)
get_fit_rf(num_train_oversample, num_test)

##### Words

# function: pass train and test dataframes
get_fit_words <- function(train_df, test_df) {
  fit_words <- rpart(has_review_2016 ~ word_stay +
                       word_na + word_great + word_place + word_host + word_house +
                       word_clean + word_nice + word_home + word_comfortable + 
                       word_location + word_room + word_apartment + word_time + 
                       word_recommend + word_perfect + word_beautiful +
                       word_made + word_area + word_easy + word_wonderful +
                       word_experience + word_enjoy + word_neighborhood + word_bed +
                       word_love + word_back + word_good + word_close + word_quiet +
                       word_welcome + word_need + word_walk + word_feel + word_helpful +
                       word_friendly + word_space + word_restaurant +
                       word_lovely + word_night + word_amazing +
                       word_beach + word_kitchen + word_highly +
                       word_lot + word_super + word_felt + word_make + word_day +
                       word_check + word_bathroom + word_airbnb + word_arrive + 
                       word_family + word_convenient + word_cozy + 
                       word_visit + word_view + word_accommodating +
                       word_downtown + word_trip + word_walking +
                       word_coffee + word_didn + word_provided + word_located +
                       word_spacious + word_quick + word_arrival +
                       word_street + word_question + word_breakfast +
                       word_town + word_warm + word_private + word_city +
                       word_excellent + word_la + word_parking +
                       word_shop + word_gave + word_short + word_people + 
                       word_awesome + word_distance + word_de + word_guest + 
                       word_fantastic + word_bedroom + word_work + word_weekend + 
                       word_morning + word_park + word_extremely + word_left +
                       word_kind + word_safe + word_minutes + word_long +
                       word_list,
                  data = train_df, control = rpart.control(cp = 0.001, maxdepth = 5))
  printcp(fit_words)
  bestcp_words <- fit_words$cptable[which.min(fit_words$cptable[,"xerror"]), "CP"]
  
  #prune tree using best cp
  tree_pruned_words <- prune(fit_words, cp = bestcp_words)
  prp(tree_pruned_words, faclen = 0, cex = 0.8, node.fun=tot_count)
  
  # plot
  rpart.plot(tree_pruned_words)
  
  # use predict to see the prediction on test set
  sample_predict_words <- predict(tree_pruned_words, test_df)
  
  # calculate AUC
  ROCR_words <- prediction(sample_predict_words, test_df$has_review_2016) 
  roc.perf_words = performance(ROCR_words, measure = "tpr", x.measure = "fpr")
  performance(ROCR_words, measure = "auc")
  plot(roc.perf_words)
}

# calls
get_fit_words(train, test)
get_fit_words(train_oversample, test)

get_fit_words(jan_train, jan_test)
get_fit_words(jan_train_oversample, jan_test)

get_fit_words(before_train, before_test)
get_fit_words(before_train_oversample, before_test)

get_fit_words(num_train, num_test)
get_fit_words(num_train_oversample, num_test)

##### Last Seen

# function: pass train and test dataframes
get_fit_ls <- function(train_df, test_df) {
  fit_ls <- rpart(has_review_2016 ~ host_since + host_response_time + 
                      host_response_rate + host_acceptance_rate + host_is_superhost +               
                      host_listings_count + host_total_listings_count + host_has_profile_pic +            
                      host_identity_verified + city + state +                           
                      zipcode + country_code + country +                         
                      property_type + room_type + accommodates +                    
                      bathrooms + bedrooms + beds +                            
                      bed_type + weekly_price + monthly_price +                   
                      security_deposit + cleaning_fee + guests_included +                 
                      extra_people + minimum_nights + maximum_nights +                  
                      number_of_reviews + first_review + last_review +                     
                      review_scores_rating + review_scores_accuracy + 
                      review_scores_cleanliness +       
                      review_scores_checkin + review_scores_communication + review_scores_location +          
                      review_scores_value + instant_bookable + cancellation_policy +             
                      require_guest_profile_picture + require_guest_phone_verification + region_id +                       
                      region_name + region_parent_id + region_parent_name +              
                      calculated_host_listings_count + reviews_per_month,
                  data = train_df, control = rpart.control(cp = 0.001, maxdepth = 5))
  printcp(fit_ls)
  bestcp_ls <- fit_ls$cptable[which.min(fit_ls$cptable[,"xerror"]), "CP"]
  
  #prune tree using best cp
  tree_pruned_ls <- prune(fit_ls, cp = bestcp_ls)
  prp(tree_pruned_ls, faclen = 0, cex = 0.8, node.fun=tot_count)
  
  # plot
  rpart.plot(tree_pruned_ls)
  
  # use predict to see the prediction on test set
  sample_predict_ls <- predict(tree_pruned_ls, test_df)
  
  # calculate AUC
  ROCR_ls <- prediction(sample_predict_ls, test_df$has_review_2016) 
  roc.perf_ls = performance(ROCR_ls, measure = "tpr", x.measure = "fpr")
  performance(ROCR_ls, measure = "auc")
  plot(roc.perf_ls)
}

# calls

get_fit_ls(jan_train, jan_test)
get_fit_ls(jan_train_oversample, jan_test)

get_fit_ls(before_train, before_test)
get_fit_ls(before_train_oversample, before_test)

get_fit_ls(num_train, num_test)
get_fit_ls(num_train_oversample, num_test)

##### Recency Frequency / Words

# function: pass train and test dataframes
get_fit_rf_words <- function(train_df, test_df) {
  fit_rf_words <- rpart(has_review_2016 ~ first_month +
                    first_diff_2015 +
                    last_month +
                    last_diff_2015 +
                    first_in_2015 +
                    first_month_in_2015 +
                    last_in_2015 +
                    last_month_in_2015 +
                    num_in_2015 +
                    all_time_as_of_2015 +
                    word_stay +
                    word_na + word_great + word_place + word_host + word_house +
                    word_clean + word_nice + word_home + word_comfortable + 
                    word_location + word_room + word_apartment + word_time + 
                    word_recommend + word_perfect + word_beautiful +
                    word_made + word_area + word_easy + word_wonderful +
                    word_experience + word_enjoy + word_neighborhood + word_bed +
                    word_love + word_back + word_good + word_close + word_quiet +
                    word_welcome + word_need + word_walk + word_feel + word_helpful +
                    word_friendly + word_space + word_restaurant +
                    word_lovely + word_night + word_amazing +
                    word_beach + word_kitchen + word_highly +
                    word_lot + word_super + word_felt + word_make + word_day +
                    word_check + word_bathroom + word_airbnb + word_arrive + 
                    word_family + word_convenient + word_cozy + 
                    word_visit + word_view + word_accommodating +
                    word_downtown + word_trip + word_walking +
                    word_coffee + word_didn + word_provided + word_located +
                    word_spacious + word_quick + word_arrival +
                    word_street + word_question + word_breakfast +
                    word_town + word_warm + word_private + word_city +
                    word_excellent + word_la + word_parking +
                    word_shop + word_gave + word_short + word_people + 
                    word_awesome + word_distance + word_de + word_guest + 
                    word_fantastic + word_bedroom + word_work + word_weekend + 
                    word_morning + word_park + word_extremely + word_left +
                    word_kind + word_safe + word_minutes + word_long +
                    word_list,
                  data = train_df, control = rpart.control(cp = 0.001, maxdepth = 5))
  printcp(fit_rf_words)
  bestcp_rf_words <- fit_rf_words$cptable[which.min(fit_rf_words$cptable[,"xerror"]), "CP"]
  
  #prune tree using best cp
  tree_pruned_rf_words <- prune(fit_rf_words, cp = bestcp_rf_words)
  prp(tree_pruned_rf_words, faclen = 0, cex = 0.8, node.fun=tot_count)
  
  # plot
  rpart.plot(tree_pruned_rf_words)
  
  # use predict to see the prediction on test set
  sample_predict_rf_words <- predict(tree_pruned_rf_words, test_df)
  
  # calculate AUC
  ROCR_rf_words <- prediction(sample_predict_rf_words, test_df$has_review_2016) 
  roc.perf_rf_words = performance(ROCR_rf_words, measure = "tpr", x.measure = "fpr")
  performance(ROCR_rf_words, measure = "auc")
  plot(roc.perf_rf_words)
}

# calls
get_fit_rf_words(train, test)
get_fit_rf_words(train_oversample, test)

get_fit_rf_words(jan_train, jan_test)
get_fit_rf_words(jan_train_oversample, jan_test)

get_fit_rf_words(before_train, before_test)
get_fit_rf_words(before_train_oversample, before_test)

get_fit_rf_words(num_train, num_test)
get_fit_rf_words(num_train_oversample, num_test)

##### Recency Frequency / Last Seen

# function: pass train and test dataframes
get_fit_rf_ls <- function(train_df, test_df) {
  fit_rf_ls <- rpart(has_review_2016 ~ first_month +
                          first_diff_2015 +
                          last_month +
                          last_diff_2015 +
                          first_in_2015 +
                          first_month_in_2015 +
                          last_in_2015 +
                          last_month_in_2015 +
                          num_in_2015 +
                          all_time_as_of_2015 +
                          host_since + host_response_time + 
                          host_response_rate + host_acceptance_rate + host_is_superhost +               
                          host_listings_count + host_total_listings_count + host_has_profile_pic +            
                          host_identity_verified + city + state +                           
                          zipcode + country_code + country +                         
                          property_type + room_type + accommodates +                    
                          bathrooms + bedrooms + beds +                            
                          bed_type + weekly_price + monthly_price +                   
                          security_deposit + cleaning_fee + guests_included +                 
                          extra_people + minimum_nights + maximum_nights +                  
                          number_of_reviews + first_review + last_review +                     
                          review_scores_rating + review_scores_accuracy + 
                          review_scores_cleanliness +       
                          review_scores_checkin + review_scores_communication + review_scores_location +          
                          review_scores_value + instant_bookable + cancellation_policy +             
                          require_guest_profile_picture + require_guest_phone_verification + region_id +                       
                          region_name + region_parent_id + region_parent_name +              
                          calculated_host_listings_count + reviews_per_month,
                        data = train_df, control = rpart.control(cp = 0.001, maxdepth = 5))
  printcp(fit_rf_ls)
  bestcp_rf_ls <- fit_rf_ls$cptable[which.min(fit_rf_ls$cptable[,"xerror"]), "CP"]
  
  #prune tree using best cp
  tree_pruned_rf_ls <- prune(fit_rf_ls, cp = bestcp_rf_ls)
  prp(tree_pruned_rf_ls, faclen = 0, cex = 0.8, node.fun=tot_count)
  
  # plot
  rpart.plot(tree_pruned_rf_ls)
  
  # use predict to see the prediction on test set
  sample_predict_rf_ls <- predict(tree_pruned_rf_ls, test_df)
  
  # calculate AUC
  ROCR_rf_ls <- prediction(sample_predict_rf_ls, test_df$has_review_2016) 
  roc.perf_rf_ls = performance(ROCR_rf_ls, measure = "tpr", x.measure = "fpr")
  performance(ROCR_rf_ls, measure = "auc")
  plot(roc.perf_rf_ls)
}

# calls
get_fit_rf_ls(jan_train, jan_test)
get_fit_rf_ls(jan_train_oversample, jan_test)

get_fit_rf_ls(before_train, before_test)
get_fit_rf_ls(before_train_oversample, before_test)

get_fit_rf_ls(num_train, num_test)
get_fit_rf_ls(num_train_oversample, num_test)

##### Words / Last Seen

# function: pass train and test dataframes
get_fit_words_ls <- function(train_df, test_df) {
  fit_words_ls <- rpart(has_review_2016 ~ word_stay +
                       word_na + word_great + word_place + word_host + word_house +
                       word_clean + word_nice + word_home + word_comfortable + 
                       word_location + word_room + word_apartment + word_time + 
                       word_recommend + word_perfect + word_beautiful +
                       word_made + word_area + word_easy + word_wonderful +
                       word_experience + word_enjoy + word_neighborhood + word_bed +
                       word_love + word_back + word_good + word_close + word_quiet +
                       word_welcome + word_need + word_walk + word_feel + word_helpful +
                       word_friendly + word_space + word_restaurant +
                       word_lovely + word_night + word_amazing +
                       word_beach + word_kitchen + word_highly +
                       word_lot + word_super + word_felt + word_make + word_day +
                       word_check + word_bathroom + word_airbnb + word_arrive + 
                       word_family + word_convenient + word_cozy + 
                       word_visit + word_view + word_accommodating +
                       word_downtown + word_trip + word_walking +
                       word_coffee + word_didn + word_provided + word_located +
                       word_spacious + word_quick + word_arrival +
                       word_street + word_question + word_breakfast +
                       word_town + word_warm + word_private + word_city +
                       word_excellent + word_la + word_parking +
                       word_shop + word_gave + word_short + word_people + 
                       word_awesome + word_distance + word_de + word_guest + 
                       word_fantastic + word_bedroom + word_work + word_weekend + 
                       word_morning + word_park + word_extremely + word_left +
                       word_kind + word_safe + word_minutes + word_long +
                       word_list +
                       host_since + host_response_time + 
                       host_response_rate + host_acceptance_rate + host_is_superhost +               
                       host_listings_count + host_total_listings_count + host_has_profile_pic +            
                       host_identity_verified + city + state +                           
                       zipcode + country_code + country +                         
                       property_type + room_type + accommodates +                    
                       bathrooms + bedrooms + beds +                            
                       bed_type + weekly_price + monthly_price +                   
                       security_deposit + cleaning_fee + guests_included +                 
                       extra_people + minimum_nights + maximum_nights +                  
                       number_of_reviews + first_review + last_review +                     
                       review_scores_rating + review_scores_accuracy + 
                       review_scores_cleanliness +       
                       review_scores_checkin + review_scores_communication + review_scores_location +          
                       review_scores_value + instant_bookable + cancellation_policy +             
                       require_guest_profile_picture + require_guest_phone_verification + region_id +                       
                       region_name + region_parent_id + region_parent_name +              
                       calculated_host_listings_count + reviews_per_month,
                     data = train_df, control = rpart.control(cp = 0.001, maxdepth = 5))
  printcp(fit_words_ls)
  bestcp_words_ls <- fit_words_ls$cptable[which.min(fit_words_ls$cptable[,"xerror"]), "CP"]
  
  #prune tree using best cp
  tree_pruned_words_ls <- prune(fit_words_ls, cp = bestcp_words_ls)
  prp(tree_pruned_words_ls, faclen = 0, cex = 0.8, node.fun=tot_count)
  
  # plot
  rpart.plot(tree_pruned_words_ls)
  
  # use predict to see the prediction on test set
  sample_predict_words_ls <- predict(tree_pruned_words_ls, test_df)
  
  # calculate AUC
  ROCR_words_ls <- prediction(sample_predict_words_ls, test_df$has_review_2016) 
  roc.perf_words_ls = performance(ROCR_words_ls, measure = "tpr", x.measure = "fpr")
  performance(ROCR_words_ls, measure = "auc")
  plot(roc.perf_words_ls)
}

# calls
get_fit_words_ls(jan_train, jan_test)
get_fit_words_ls(jan_train_oversample, jan_test)

get_fit_words_ls(before_train, before_test)
get_fit_words_ls(before_train_oversample, before_test)

get_fit_words_ls(num_train, num_test)
get_fit_words_ls(num_train_oversample, num_test)

##### All

# function: pass train and test dataframes
get_fit_all <- function(train_df, test_df) {
  fit_all <- rpart(has_review_2016 ~ first_month +
                          first_diff_2015 +
                          last_month +
                          last_diff_2015 +
                          first_in_2015 +
                          first_month_in_2015 +
                          last_in_2015 +
                          last_month_in_2015 +
                          num_in_2015 +
                          all_time_as_of_2015 +
                          word_stay +
                          word_na + word_great + word_place + word_host + word_house +
                          word_clean + word_nice + word_home + word_comfortable + 
                          word_location + word_room + word_apartment + word_time + 
                          word_recommend + word_perfect + word_beautiful +
                          word_made + word_area + word_easy + word_wonderful +
                          word_experience + word_enjoy + word_neighborhood + word_bed +
                          word_love + word_back + word_good + word_close + word_quiet +
                          word_welcome + word_need + word_walk + word_feel + word_helpful +
                          word_friendly + word_space + word_restaurant +
                          word_lovely + word_night + word_amazing +
                          word_beach + word_kitchen + word_highly +
                          word_lot + word_super + word_felt + word_make + word_day +
                          word_check + word_bathroom + word_airbnb + word_arrive + 
                          word_family + word_convenient + word_cozy + 
                          word_visit + word_view + word_accommodating +
                          word_downtown + word_trip + word_walking +
                          word_coffee + word_didn + word_provided + word_located +
                          word_spacious + word_quick + word_arrival +
                          word_street + word_question + word_breakfast +
                          word_town + word_warm + word_private + word_city +
                          word_excellent + word_la + word_parking +
                          word_shop + word_gave + word_short + word_people + 
                          word_awesome + word_distance + word_de + word_guest + 
                          word_fantastic + word_bedroom + word_work + word_weekend + 
                          word_morning + word_park + word_extremely + word_left +
                          word_kind + word_safe + word_minutes + word_long +
                          word_list +
                          host_since + host_response_time + 
                          host_response_rate + host_acceptance_rate + host_is_superhost +               
                          host_listings_count + host_total_listings_count + host_has_profile_pic +            
                          host_identity_verified + city + state +                           
                          zipcode + country_code + country +                         
                          property_type + room_type + accommodates +                    
                          bathrooms + bedrooms + beds +                            
                          bed_type + weekly_price + monthly_price +                   
                          security_deposit + cleaning_fee + guests_included +                 
                          extra_people + minimum_nights + maximum_nights +                  
                          number_of_reviews + first_review + last_review +                     
                          review_scores_rating + review_scores_accuracy + 
                          review_scores_cleanliness +       
                          review_scores_checkin + review_scores_communication + review_scores_location +          
                          review_scores_value + instant_bookable + cancellation_policy +             
                          require_guest_profile_picture + require_guest_phone_verification + region_id +                       
                          region_name + region_parent_id + region_parent_name +              
                          calculated_host_listings_count + reviews_per_month,
                        data = train_df, control = rpart.control(cp = 0.001, maxdepth = 5))
  printcp(fit_all)
  bestcp_all <- fit_all$cptable[which.min(fit_all$cptable[,"xerror"]), "CP"]
  
  #prune tree using best cp
  tree_pruned_all <- prune(fit_all, cp = bestcp_all)
  prp(tree_pruned_all, faclen = 0, cex = 0.8, node.fun=tot_count)
  
  # plot
  rpart.plot(tree_pruned_all)
  
  # use predict to see the prediction on test set
  sample_predict_all <- predict(tree_pruned_all, test_df)
  
  # calculate AUC
  ROCR_all <- prediction(sample_predict_all, test_df$has_review_2016) 
  roc.perf_all = performance(ROCR_all, measure = "tpr", x.measure = "fpr")
  performance(ROCR_all, measure = "auc")
  plot(roc.perf_all)
}

# calls
get_fit_all(jan_train, jan_test)
get_fit_all(jan_train_oversample, jan_test)

get_fit_all(before_train, before_test)
get_fit_all(before_train_oversample, before_test)

get_fit_all(num_train, num_test)
get_fit_all(num_train_oversample, num_test)