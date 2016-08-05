#create trees for the skimmed listings for all listings in the January 
#cohort

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
##created by file = "create_listings_history_skimmed.R"

#filter the listings for only those in the january cohort
#these listings were first seen in January 2015

listings_history_january <- skimmed_listings_history %>% filter(first_seen == "2015-01-01")

listings_history_january$zipcode <- as.character(listings_history_january$zipcode)


cv_listings <- 
  function(feature) {
    num_folds <- 10
    listings_auc <- nrow(listings_history_january)
    listings_history_january$fold <- sample(1:num_folds, listings_auc, replace=T)
    auc <- c()
    for (i in 1:num_folds) {
      set.seed(i)
      # use this data frame to fit your model
      listings_train <- filter(listings_history_january, fold != i)
      
      # pretend this doesn't exist (for now!)
      listings_test <- filter(listings_history_january, fold == i)
      
      tree <- rpart(feature, data = listings_train, 
                    control = rpart.control(cp = 0.005))
      
      # evaluate on the test data
      # evaluate on the test data
      bestcp <- tree$cptable[which.min(tree$cptable[,"xerror"]), "CP"]
      tree_pruned <- prune(tree, cp = bestcp)
      predicted <- predict(tree_pruned, listings_test)
      ROCR_predict <- prediction(predicted, listings_test$exist_in_2016)
      roc.perf <- performance(ROCR_predict, measure = "tpr", x.measure = "fpr")
      auc_pred <- performance(ROCR_predict, measure = "auc")
      auc[i]  <- auc_pred@y.values
    }
    mean_auc <- mean(as.numeric(auc)) 
    return(mean_auc)
  }  
#create all combos

rf <- (exist_in_2016 ~ host_listings_count + host_duration + 
         first_seen_month + last_seen_month + 
         listing_recency_2015_weeks + scrap_duration + 
         total_occ_2015 + review_recency_2015_weeks + 
         is_superhost_2015 + is_superhost_count_2015)

rv <- (exist_in_2016 ~ first_review_year + last_review_year +
         num_as_of_2015 + num_reviews_in_2015 + has_reviews_2015 +
         first_review_month_2015 + last_review_month_2015 + 
         last_rating)

price <- (exist_in_2016 ~ min_price + max_price + mean_price)

rf_rf <- (exist_in_2016 ~ host_listings_count + host_duration + 
            first_seen_month + last_seen_month + 
            listing_recency_2015_weeks + scrap_duration + 
            total_occ_2015 + review_recency_2015_weeks + 
            is_superhost_2015 + is_superhost_count_2015 + 
            first_review_year + last_review_year +
            num_as_of_2015 + num_reviews_in_2015 + has_reviews_2015 +
            first_review_month_2015 + last_review_month_2015 + 
            last_rating)

rf_p <- (exist_in_2016 ~ host_listings_count + host_duration + 
           first_seen_month + last_seen_month + 
           listing_recency_2015_weeks + scrap_duration + 
           total_occ_2015 + review_recency_2015_weeks + 
           is_superhost_2015 + is_superhost_count_2015 + 
           min_price + max_price + mean_price)

rv_p <- (exist_in_2016 ~ first_review_year + last_review_year +
           num_as_of_2015 + num_reviews_in_2015 + has_reviews_2015 +
           first_review_month_2015 + last_review_month_2015 + 
           review_recency_2015_weeks + last_rating + min_price + 
           max_price + mean_price)

rv_rf_p <- (exist_in_2016 ~ host_listings_count + host_duration + 
              first_seen_month + last_seen_month + 
              listing_recency_2015_weeks + scrap_duration + 
              total_occ_2015 + review_recency_2015_weeks + 
              is_superhost_2015 + is_superhost_count_2015 + 
              first_review_year + last_review_year +
              num_as_of_2015 + num_reviews_in_2015 + has_reviews_2015 +
              first_review_month_2015 + last_review_month_2015 + 
              last_rating + mean_price + max_price + min_price)

amen <- (exist_in_2016 ~  TV + Internet + Wireless.Internet +
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
           translation.missing..en.hosting_amenity_50)

amen_rv_rf_p <- (exist_in_2016 ~ TV + Internet + Wireless.Internet +
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
                   last_rating + mean_price + max_price + min_price +room_type)

ver <- (exist_in_2016 ~  email + phone +
          facebook + reviews + kba + google + jumio + sent_id + 
          linkedin +
          manual_offline + manual_online + weibo + photographer + 
          None +
          amex + verifications_count)

rv_rf_p_amen_ver <- (exist_in_2016 ~  TV + Internet + Wireless.Internet +
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
                       amex + verifications_count)

words <- (exist_in_2016 ~ word_stay + word_apartment + word_great + 
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
            word_bedroom + word_trip + word_times + word_kind)

words_amen_ver_rf_rv_p <- (exist_in_2016 ~  TV + Internet + Wireless.Internet +
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
                             amex + verifications_count + word_stay + word_apartment + 
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
                             word_bedroom + word_trip + word_times + word_kind)

loc <- (exist_in_2016 ~ #neighbourhood_cleansed +
          neighbourhood_group_cleansed + #smart_location + 
          #city + 
          #zipcode + market + #country + 
          is_location_exact)

amen_ver_loc_p_rv_rf <- (exist_in_2016 ~  TV + Internet + Wireless.Internet +
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
                           #zipcode + market + #country + 
                           is_location_exact)

non_rf <- (exist_in_2016 ~ min_price + 
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
             #zipcode + market + #country + 
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
             word_bedroom + word_trip + word_times + word_kind)

rf_words <- (exist_in_2016 ~ word_stay + word_apartment + word_great + 
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
               word_bedroom + word_trip + word_times + word_kind +
               host_listings_count + host_duration + 
               first_seen_month + last_seen_month + 
               listing_recency_2015_weeks + scrap_duration + 
               total_occ_2015 + review_recency_2015_weeks + 
               is_superhost_2015 + is_superhost_count_2015) 

rm_type <- (exist_in_2016 ~ room_type)

loc_p <- (exist_in_2016 ~ max_price + mean_price + min_price + 
            #neighbourhood_cleansed +
            neighbourhood_group_cleansed + #smart_location + 
            #city + 
            #zipcode + market + #country + 
            is_location_exact)


joined_reviews <- (exist_in_2016 ~ fr_num_reviews + fr_first_review_date + 
                     fr_last_review_date + fr_num_different_places + lr_num_reviews + 
                     lr_first_review_date + lr_last_review_date + lr_num_different_places + 
                     avg_reviewer_num_reviews + avg_reviewer_num_places + 
                     avg_reviewer_num_repeat_visits_here + num_visitors + num_repeat_visits + 
                     num_repeat_visitors + max_num_repeat_visits + percent_repeat_visitors + 
                     percent_repeat_visitors_laplace)


amen_rv_rf_p_joined <- (exist_in_2016 ~ TV + Internet + Wireless.Internet +
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
                          fr_num_reviews + fr_first_review_date + 
                          fr_last_review_date + fr_num_different_places + lr_num_reviews + 
                          lr_first_review_date + lr_last_review_date + lr_num_different_places + 
                          avg_reviewer_num_reviews + avg_reviewer_num_places + 
                          avg_reviewer_num_repeat_visits_here + num_visitors + num_repeat_visits + 
                          num_repeat_visitors + max_num_repeat_visits + percent_repeat_visitors + 
                          percent_repeat_visitors_laplace)


trees <- c(amen, amen_rv_rf_p, amen_ver_loc_p_rv_rf, loc, loc_p, non_rf, price, 
           rf, rf_p, rf_rf, rf_words, rm_type, rv, rv_p, rv_rf_p_amen_ver, 
           rv_rf_p, ver, words, words_amen_ver_rf_rv_p, joined_reviews,  
             amen_rv_rf_p_joined)


auc_all <- lapply(trees, cv_listings)
auc_all


########################all features#########################################
tree_all <- (exist_in_2016 ~ #property_type + 
               room_type + mean_price + 
               min_price + max_price + is_multilisting + host_since + host_listings_count + 
               host_duration + first_seen + last_seen + first_seen_month + last_seen_month + 
               listing_recency_2015_weeks + scrap_duration + total_occ_2015 + first_review + 
               first_review_year+ last_review + last_review_year + num_as_of_2015 + 
               num_reviews_in_2015 + has_reviews_2015 + first_review_2015 + 
               first_review_month_2015 + last_review_2015 + last_review_month_2015 + 
               review_recency_2015_weeks + last_rating + is_superhost_2015 + 
               is_superhost_count_2015 + TV + Internet + Wireless.Internet + Air.Conditioning + 
               Kitchen + Heating + Family.Kid.Friendly + Smoke.Detector + 
               Carbon.Monoxide.Detector + Essentials + Shampoo + Cable.TV + 
               Free.Parking.on.Premises + Breakfast + Pets.live.on.this.property + Dog.s. + 
               First.Aid.Kit + Buzzer.Wireless.Intercom + Washer + Dryer + Pets.Allowed + 
               Gym + Safety.Card + Fire.Extinguisher + Wheelchair.Accessible + Cat.s. + 
               Indoor.Fireplace + Suitable.for.Events + Doorman + Hot.Tub + 
               Elevator.in.Building + Pool + Smoking.Allowed + Other.pet.s. + Washer...Dryer + 
               Lock.on.Bedroom.Door + X24.Hour.Check.in + Hangers + Hair.Dryer + Iron + 
               Laptop.Friendly.Workspace + translation.missing..en.hosting_amenity_49 + 
               translation.missing..en.hosting_amenity_50 + email + phone + facebook + reviews + 
               kba + google + jumio + sent_id + linkedin + manual_offline + manual_online + 
               weibo + photographer + None + amex + verifications_count + purged + word_stay + 
               word_apartment + word_great + word_place + word_host + word_clean + 
               word_location + word_nice + word_room + word_subway + word_recommend + word_de + 
               word_comfortable + word_time + word_good + word_easy + word_perfect + 
               word_close + word_restaurant + word_neighborhood + word_la + word_nyc + 
               word_helpful + word_bed + word_manhattan + word_experience + word_made + 
               word_home + word_walk + word_area + word_york + word_friendly + word_le + 
               word_lot + word_check + word_city + word_est + word_need + word_super + 
               word_en + word_quiet + word_back + word_feel + word_welcome + word_convenient + 
               word_station + word_wonderful + word_space + word_enjoy + word_night + 
               word_located + word_love + word_arrival + word_day + word_brooklyn + 
               word_arrive + word_felt + word_amazing + word_bathroom + word_safe + 
               word_park + word_highly + word_trs + word_kitchen + word_street + word_block + 
               word_house + word_nous + word_lovely + word_days + word_airbnb + word_question + 
               word_el + word_shop + word_didn + word_spacious + word_make + word_quick + 
               word_bar + word_people + word_accommodating + word_walking + word_minutes + 
               word_train + word_beautiful + word_key + word_ny + word_central + word_small + 
               word_muy + word_visit + word_building + word_es + word_excellent + 
               word_appartement + word_metro + word_bedroom + word_trip + word_times + 
               word_kind + #neighbourhood + neighbourhood_cleansed + 
               neighbourhood_group_cleansed + #smart_location 
               + #city + state + #zipcode + 
               market + #country + latitude + longitude + 
               is_location_exact + 
               #host_neighbourhood + 
               fr_num_reviews + fr_first_review_date + 
               fr_last_review_date + fr_num_different_places + lr_num_reviews + 
               lr_first_review_date + lr_last_review_date + lr_num_different_places + 
               avg_reviewer_num_reviews + avg_reviewer_num_places + 
               avg_reviewer_num_repeat_visits_here + num_visitors + num_repeat_visits + 
               num_repeat_visitors + max_num_repeat_visits + percent_repeat_visitors + 
               percent_repeat_visitors_laplace + host_response_rate + 
               host_acceptance_rate + host_has_profile_pic + 
               host_identity_verified)

tree_nonrf <- (exist_in_2016 ~ mean_price + 
                 min_price + max_price + is_multilisting + last_rating + is_superhost_2015 + 
                 is_superhost_count_2015 + TV + Internet + Wireless.Internet + Air.Conditioning + 
                 Kitchen + Heating + Family.Kid.Friendly + Smoke.Detector + 
                 Carbon.Monoxide.Detector + Essentials + Shampoo + Cable.TV + 
                 Free.Parking.on.Premises + Breakfast + Pets.live.on.this.property + Dog.s. + 
                 First.Aid.Kit + Buzzer.Wireless.Intercom + Washer + Dryer + Pets.Allowed + 
                 Gym + Safety.Card + Fire.Extinguisher + Wheelchair.Accessible + Cat.s. + 
                 Indoor.Fireplace + Suitable.for.Events + Doorman + Hot.Tub + 
                 Elevator.in.Building + Pool + Smoking.Allowed + Other.pet.s. + Washer...Dryer + 
                 Lock.on.Bedroom.Door + X24.Hour.Check.in + Hangers + Hair.Dryer + Iron + 
                 Laptop.Friendly.Workspace + translation.missing..en.hosting_amenity_49 + 
                 translation.missing..en.hosting_amenity_50 + email + phone + facebook + reviews + 
                 kba + google + jumio + sent_id + linkedin + manual_offline + manual_online + 
                 weibo + photographer + None + amex + verifications_count + purged + word_stay + 
                 word_apartment + word_great + word_place + word_host + word_clean + 
                 word_location + word_nice + word_room + word_subway + word_recommend + word_de + 
                 word_comfortable + word_time + word_good + word_easy + word_perfect + 
                 word_close + word_restaurant + word_neighborhood + word_la + word_nyc + 
                 word_helpful + word_bed + word_manhattan + word_experience + word_made + 
                 word_home + word_walk + word_area + word_york + word_friendly + word_le + 
                 word_lot + word_check + word_city + word_est + word_need + word_super + 
                 word_en + word_quiet + word_back + word_feel + word_welcome + word_convenient + 
                 word_station + word_wonderful + word_space + word_enjoy + word_night + 
                 word_located + word_love + word_arrival + word_day + word_brooklyn + 
                 word_arrive + word_felt + word_amazing + word_bathroom + word_safe + 
                 word_park + word_highly + word_trs + word_kitchen + word_street + word_block + 
                 word_house + word_nous + word_lovely + word_days + word_airbnb + word_question + 
                 word_el + word_shop + word_didn + word_spacious + word_make + word_quick + 
                 word_bar + word_people + word_accommodating + word_walking + word_minutes + 
                 word_train + word_beautiful + word_key + word_ny + word_central + word_small + 
                 word_muy + word_visit + word_building + word_es + word_excellent + 
                 word_appartement + word_metro + word_bedroom + word_trip + word_times + 
                 word_kind + neighbourhood_group_cleansed + market + is_location_exact +
                 fr_num_reviews + fr_first_review_date + 
                 fr_last_review_date + fr_num_different_places + lr_num_reviews + 
                 lr_first_review_date + lr_last_review_date + lr_num_different_places + 
                 avg_reviewer_num_reviews + avg_reviewer_num_places + 
                 avg_reviewer_num_repeat_visits_here + num_repeat_visits + 
                 num_repeat_visitors + max_num_repeat_visits + percent_repeat_visitors + 
                 percent_repeat_visitors_laplace + host_response_rate + 
                 host_acceptance_rate + host_has_profile_pic + 
                 host_identity_verified)

cv_listings <- 
  function(feature) {
    num_folds <- 10
    listings_auc <- nrow(listings_history_january)
    listings_history_january$fold <- sample(1:num_folds, listings_auc, replace=T)
    auc <- c()
    for (i in 1:num_folds) {
      set.seed(i)
      # use this data frame to fit your model
      listings_train <- filter(listings_history_january, fold != i)
      
      # pretend this doesn't exist (for now!)
      listings_test <- filter(listings_history_january, fold == i)
      
      tree <- rpart(feature, data = listings_train, 
                    control = rpart.control(cp = 0.001))
      
      # evaluate on the test data
      # evaluate on the test data
      bestcp <- tree$cptable[which.min(tree$cptable[,"xerror"]), "CP"]
      tree_pruned <- prune(tree, cp = bestcp)
      predicted <- predict(tree_pruned, listings_test)
      ROCR_predict <- prediction(predicted, listings_test$exist_in_2016)
      roc.perf <- performance(ROCR_predict, measure = "tpr", x.measure = "fpr")
      auc_pred <- performance(ROCR_predict, measure = "auc")
      auc[i]  <- auc_pred@y.values
    }
    mean_auc <- mean(as.numeric(auc)) 
    return(mean_auc)
  }  


cv_listings(tree_all)
cv_listings(tree_nonrf)



#plot tree_all
tree_all <- rpart(exist_in_2016 ~ room_type + mean_price + 
                    min_price + max_price + is_multilisting + host_since + host_listings_count + 
                    host_duration + first_seen + last_seen + first_seen_month + last_seen_month + 
                    listing_recency_2015_weeks + scrap_duration + total_occ_2015 + first_review + 
                    first_review_year+ last_review + last_review_year + num_as_of_2015 + 
                    num_reviews_in_2015 + has_reviews_2015 + first_review_2015 + 
                    first_review_month_2015 + last_review_2015 + last_review_month_2015 + 
                    review_recency_2015_weeks + last_rating + is_superhost_2015 + 
                    is_superhost_count_2015 + TV + Internet + Wireless.Internet + Air.Conditioning + 
                    Kitchen + Heating + Family.Kid.Friendly + Smoke.Detector + 
                    Carbon.Monoxide.Detector + Essentials + Shampoo + Cable.TV + 
                    Free.Parking.on.Premises + Breakfast + Pets.live.on.this.property + Dog.s. + 
                    First.Aid.Kit + Buzzer.Wireless.Intercom + Washer + Dryer + Pets.Allowed + 
                    Gym + Safety.Card + Fire.Extinguisher + Wheelchair.Accessible + Cat.s. + 
                    Indoor.Fireplace + Suitable.for.Events + Doorman + Hot.Tub + 
                    Elevator.in.Building + Pool + Smoking.Allowed + Other.pet.s. + Washer...Dryer + 
                    Lock.on.Bedroom.Door + X24.Hour.Check.in + Hangers + Hair.Dryer + Iron + 
                    Laptop.Friendly.Workspace + translation.missing..en.hosting_amenity_49 + 
                    translation.missing..en.hosting_amenity_50 + email + phone + facebook + reviews + 
                    kba + google + jumio + sent_id + linkedin + manual_offline + manual_online + 
                    weibo + photographer + None + amex + verifications_count + purged + word_stay + 
                    word_apartment + word_great + word_place + word_host + word_clean + 
                    word_location + word_nice + word_room + word_subway + word_recommend + word_de + 
                    word_comfortable + word_time + word_good + word_easy + word_perfect + 
                    word_close + word_restaurant + word_neighborhood + word_la + word_nyc + 
                    word_helpful + word_bed + word_manhattan + word_experience + word_made + 
                    word_home + word_walk + word_area + word_york + word_friendly + word_le + 
                    word_lot + word_check + word_city + word_est + word_need + word_super + 
                    word_en + word_quiet + word_back + word_feel + word_welcome + word_convenient + 
                    word_station + word_wonderful + word_space + word_enjoy + word_night + 
                    word_located + word_love + word_arrival + word_day + word_brooklyn + 
                    word_arrive + word_felt + word_amazing + word_bathroom + word_safe + 
                    word_park + word_highly + word_trs + word_kitchen + word_street + word_block + 
                    word_house + word_nous + word_lovely + word_days + word_airbnb + word_question + 
                    word_el + word_shop + word_didn + word_spacious + word_make + word_quick + 
                    word_bar + word_people + word_accommodating + word_walking + word_minutes + 
                    word_train + word_beautiful + word_key + word_ny + word_central + word_small + 
                    word_muy + word_visit + word_building + word_es + word_excellent + 
                    word_appartement + word_metro + word_bedroom + word_trip + word_times + 
                    word_kind + #neighbourhood + neighbourhood_cleansed + 
                    neighbourhood_group_cleansed + #smart_location 
                    + #city + state + #zipcode + 
                    market + #country + latitude + longitude + 
                    is_location_exact + 
                    #host_neighbourhood + 
                    fr_num_reviews + fr_first_review_date + 
                    fr_last_review_date + fr_num_different_places + lr_num_reviews + 
                    lr_first_review_date + lr_last_review_date + lr_num_different_places + 
                    avg_reviewer_num_reviews + avg_reviewer_num_places + 
                    avg_reviewer_num_repeat_visits_here + num_visitors + num_repeat_visits + 
                    num_repeat_visitors + max_num_repeat_visits + percent_repeat_visitors + 
                    percent_repeat_visitors_laplace + host_response_rate + 
                    host_acceptance_rate + host_has_profile_pic + 
                    host_identity_verified, data = train, 
                  control = rpart.control(cp = 0.001) )
bestcp_all <- tree_all$cptable[which.min(tree_all$cptable[,"xerror"]), "CP"]
tree_pruned_all <- prune(tree_all, cp = bestcp_all)

#save the tree file
jpeg(filename = "../../figures/all_features_exists2016.jpeg")
rpart.plot(tree_pruned_all)
title("Exists 2016 Using All Features for the January Cohort")
dev.off()


predicted_all <- predict(tree_pruned_all, test)
ROCR_predict_all <- prediction(predicted_all, test$exist_in_2016)
roc.perf_all <- performance(ROCR_predict_all, measure = "tpr", x.measure = "fpr")
auc_pred_all <- performance(ROCR_predict_all, measure = "auc")
auc_pred_all@y.values

#plot and save the roc curve
png(filename = "../../figures/all_features_exists2016_roc.png")
plot(roc.perf_all)
title(main = "ROC for Exists 2016 Using All Features for the January Cohort", 
      sub = "AUC = 0.911")
dev.off()


tree_nonrf <- rpart(exist_in_2016 ~ mean_price + min_price + max_price + 
                      is_multilisting + last_rating + is_superhost_2015 + 
                      is_superhost_count_2015 + TV + Internet + Wireless.Internet + Air.Conditioning + 
                      Kitchen + Heating + Family.Kid.Friendly + Smoke.Detector + 
                      Carbon.Monoxide.Detector + Essentials + Shampoo + Cable.TV + 
                      Free.Parking.on.Premises + Breakfast + Pets.live.on.this.property + Dog.s. + 
                      First.Aid.Kit + Buzzer.Wireless.Intercom + Washer + Dryer + Pets.Allowed + 
                      Gym + Safety.Card + Fire.Extinguisher + Wheelchair.Accessible + Cat.s. + 
                      Indoor.Fireplace + Suitable.for.Events + Doorman + Hot.Tub + 
                      Elevator.in.Building + Pool + Smoking.Allowed + Other.pet.s. + Washer...Dryer + 
                      Lock.on.Bedroom.Door + X24.Hour.Check.in + Hangers + Hair.Dryer + Iron + 
                      Laptop.Friendly.Workspace + translation.missing..en.hosting_amenity_49 + 
                      translation.missing..en.hosting_amenity_50 + email + phone + facebook + reviews + 
                      kba + google + jumio + sent_id + linkedin + manual_offline + manual_online + 
                      weibo + photographer + None + amex + verifications_count + purged + word_stay + 
                      word_apartment + word_great + word_place + word_host + word_clean + 
                      word_location + word_nice + word_room + word_subway + word_recommend + word_de + 
                      word_comfortable + word_time + word_good + word_easy + word_perfect + 
                      word_close + word_restaurant + word_neighborhood + word_la + word_nyc + 
                      word_helpful + word_bed + word_manhattan + word_experience + word_made + 
                      word_home + word_walk + word_area + word_york + word_friendly + word_le + 
                      word_lot + word_check + word_city + word_est + word_need + word_super + 
                      word_en + word_quiet + word_back + word_feel + word_welcome + word_convenient + 
                      word_station + word_wonderful + word_space + word_enjoy + word_night + 
                      word_located + word_love + word_arrival + word_day + word_brooklyn + 
                      word_arrive + word_felt + word_amazing + word_bathroom + word_safe + 
                      word_park + word_highly + word_trs + word_kitchen + word_street + word_block + 
                      word_house + word_nous + word_lovely + word_days + word_airbnb + word_question + 
                      word_el + word_shop + word_didn + word_spacious + word_make + word_quick + 
                      word_bar + word_people + word_accommodating + word_walking + word_minutes + 
                      word_train + word_beautiful + word_key + word_ny + word_central + word_small + 
                      word_muy + word_visit + word_building + word_es + word_excellent + 
                      word_appartement + word_metro + word_bedroom + word_trip + word_times + 
                      word_kind + neighbourhood_group_cleansed + market + is_location_exact +
                      fr_num_reviews + fr_first_review_date + 
                      fr_last_review_date + fr_num_different_places + lr_num_reviews + 
                      lr_first_review_date + lr_last_review_date + lr_num_different_places + 
                      avg_reviewer_num_reviews + avg_reviewer_num_places + 
                      avg_reviewer_num_repeat_visits_here + num_repeat_visits + 
                      num_repeat_visitors + max_num_repeat_visits + percent_repeat_visitors + 
                      percent_repeat_visitors_laplace + host_response_rate + 
                      host_acceptance_rate + host_has_profile_pic + 
                      host_identity_verified, data = train, 
                    control = rpart.control(cp = 0.001) )
bestcp_nonrf <- tree_nonrf$cptable[which.min(tree_nonrf$cptable[,"xerror"]), "CP"]
tree_pruned_nonrf <- prune(tree_nonrf, cp = bestcp_nonrf)

jpeg(filename = "../../figures/nonrf_features_exists2016.jpeg")
rpart.plot(tree_pruned_nonrf)
title("Exists 2016 Using Non-Recency Frequency Features for the January Cohort")
dev.off()

predicted_nonrf <- predict(tree_pruned_nonrf, test)
ROCR_predict_nonrf <- prediction(predicted_nonrf, test$exist_in_2016)
roc.perf_nonrf <- performance(ROCR_predict_nonrf, measure = "tpr", x.measure = "fpr")
auc_pred_nonrf <- performance(ROCR_predict_nonrf, measure = "auc")

#plot and save the roc curve
png(filename = "../../figures/nonrf_features_exists2016_roc.png")
plot(roc.perf_nonrf)
title(main = "ROC for Exists 2016 Using Non-Recency Frequency Features for the 
      January Cohort", 
      sub = "AUC = 0.802")
dev.off()

words <- rpart(exist_in_2016 ~ word_stay + word_apartment + word_great + 
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
                 word_bedroom + word_trip + word_times + word_kind,  data = train, 
               control = rpart.control(cp = 0.001) )
bestcp_words <- words$cptable[which.min(words$cptable[,"xerror"]), "CP"]
tree_pruned_words <- prune(words, cp = bestcp_words)

jpeg(filename = "../../figures/word_features_exists2016.jpeg")
rpart.plot(tree_pruned_words)
title("Exists 2016 Using Word Features for the January Cohort")
dev.off()

predicted_words <- predict(tree_pruned_words, test)
ROCR_predict_words <- prediction(predicted_words, test$exist_in_2016)
roc.perf_words <- performance(ROCR_predict_words, measure = "tpr", x.measure = "fpr")
auc_pred_words <- performance(ROCR_predict_words, measure = "auc")

#plot and save the roc curve
jpeg(filename = "../../figures/word_features_exists2016_roc.jpeg")
plot(roc.perf_words)
title(main = "ROC for Exists 2016 Using Word Features for the 
      January Cohort", 
      sub = "AUC = 0.73")
dev.off()


#interplay

interplay <- rpart(exist_in_2016 ~ fr_num_reviews + fr_first_review_date + 
                     fr_last_review_date + fr_num_different_places + 
                     lr_num_reviews + 
                     lr_first_review_date + lr_last_review_date + 
                     lr_num_different_places + 
                     avg_reviewer_num_reviews + avg_reviewer_num_places + 
                     avg_reviewer_num_repeat_visits_here + num_visitors + 
                     num_repeat_visits + 
                     num_repeat_visitors + max_num_repeat_visits + 
                     percent_repeat_visitors + 
                     percent_repeat_visitors_laplace + host_response_rate + 
                     host_acceptance_rate + host_has_profile_pic + 
                     host_identity_verified, data = train, 
                   control = rpart.control(cp = 0.001) )
bestcp_interplay <- interplay$cptable[which.min(interplay$cptable[,"xerror"]), "CP"]
tree_pruned_interplay <- prune(interplay, cp = bestcp_interplay)

jpeg(filename = "../../figures/interplay_features_exists2016.jpeg")
rpart.plot(tree_pruned_interplay)
title("Exists 2016 Using Interplay Features for the January Cohort")
dev.off()

predicted_interplay <- predict(tree_pruned_interplay, test)
ROCR_predict_interplay <- prediction(predicted_interplay, test$exist_in_2016)
roc.perf_interplay <- performance(ROCR_predict_interplay, measure = "tpr", x.measure = "fpr")
auc_pred_interplay <- performance(ROCR_predict_interplay, measure = "auc")

#plot and save the roc curve
jpeg(filename = "../../figures/interplay_features_exists2016_roc.jpeg")
plot(roc.perf_words)
title(main = "ROC for Exists 2016 Using Interplay Frequency Features for the 
      January Cohort", 
      sub = "AUC = 0.74")
dev.off()

#best
best <- rpart(exist_in_2016 ~ host_listings_count + host_duration + 
                         first_seen_month + last_seen_month + 
                         listing_recency_2015_weeks + scrap_duration + 
                         total_occ_2015 + review_recency_2015_weeks + 
                         is_superhost_2015 + is_superhost_count_2015 + 
                         min_price + max_price + mean_price, data = train, 
              control = rpart.control(cp = 0.001) )
bestcp_best<- best$cptable[which.min(best$cptable[,"xerror"]), "CP"]
tree_pruned_best <- prune(best, cp = bestcp_best)

jpeg(filename = "../../figures/best_features_exists2016.jpeg")
rpart.plot(tree_pruned_best)
title("Exists 2016 Using Best Model for the January Cohort")
dev.off()

predicted_best <- predict(tree_pruned_best, test)
ROCR_predict_best <- prediction(predicted_best, test$exist_in_2016)
roc.perf_best <- performance(ROCR_predict_best, measure = "tpr", x.measure = "fpr")
auc_pred_best <- performance(ROCR_predict_best, measure = "auc")

#plot and save the roc curve
jpeg(filename = "../../figures/best_features_exists2016_roc.jpeg")
plot(roc.perf_best)
title(main = "ROC for Exists 2016 Using Best Features for the 
      January Cohort", 
      sub = "AUC = 0.93")
dev.off()
