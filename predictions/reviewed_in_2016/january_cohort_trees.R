#to find the true january cohort (the people who started in January 2015 and
#were seen there for the very first time ever)

listings_history <- read_csv("../../raw_data/listing_history.csv")
january_start <- listings_history %>% filter(host_since >= "2015-01-01" & 
                                               host_since <= "2015-01-31") %>%
  filter(purged == "FALSE")


january <- january_start %>% group_by(host_id.x) %>% 
  filter(row_number() == sample(1:row_number(), 1)) `


#deal with all listings first
indexes <- sample(1:nrow(january), 
                  size=0.2*nrow(january))



#apply the indexes to listings history to generate test and train
test=january[indexes, ]
train=january[-indexes, ]

#all the trees which need to be run

rf <- (has_reviews_2016 ~ host_listings_count + host_duration + 
         first_seen_month + last_seen_month + 
         listing_recency_2015_weeks + scrap_duration + 
         total_occ_2015 + review_recency_2015_weeks + 
         is_superhost_2015 + is_superhost_count_2015)

rv <- (has_reviews_2016 ~ first_review_year + last_review_year +
         num_as_of_2015 + num_reviews_in_2015 + has_reviews_2015 +
         first_review_month_2015 + last_review_month_2015 + 
         last_rating)

price <- (has_reviews_2016 ~ min_price + max_price + mean_price)

rf_rf <- (has_reviews_2016 ~ host_listings_count + host_duration + 
            first_seen_month + last_seen_month + 
            listing_recency_2015_weeks + scrap_duration + 
            total_occ_2015 + review_recency_2015_weeks + 
            is_superhost_2015 + is_superhost_count_2015 + 
            first_review_year + last_review_year +
            num_as_of_2015 + num_reviews_in_2015 + has_reviews_2015 +
            first_review_month_2015 + last_review_month_2015 + 
            last_rating)

rf_p <- (has_reviews_2016 ~ host_listings_count + host_duration + 
           first_seen_month + last_seen_month + 
           listing_recency_2015_weeks + scrap_duration + 
           total_occ_2015 + review_recency_2015_weeks + 
           is_superhost_2015 + is_superhost_count_2015 + 
           min_price + max_price + mean_price)

rv_p <- (has_reviews_2016 ~ first_review_year + last_review_year +
           num_as_of_2015 + num_reviews_in_2015 + has_reviews_2015 +
           first_review_month_2015 + last_review_month_2015 + 
           review_recency_2015_weeks + last_rating + min_price + 
           max_price + mean_price)

rv_rf_p <- (has_reviews_2016 ~ host_listings_count + host_duration + 
              first_seen_month + last_seen_month + 
              listing_recency_2015_weeks + scrap_duration + 
              total_occ_2015 + review_recency_2015_weeks + 
              is_superhost_2015 + is_superhost_count_2015 + 
              first_review_year + last_review_year +
              num_as_of_2015 + num_reviews_in_2015 + has_reviews_2015 +
              first_review_month_2015 + last_review_month_2015 + 
              last_rating + mean_price + max_price + min_price)

amen <- (has_reviews_2016 ~  TV + Internet + Wireless.Internet +
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

amen_rv_rf_p <- (has_reviews_2016 ~ TV + Internet + Wireless.Internet +
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

ver <- (has_reviews_2016 ~  email + phone +
          facebook + reviews + kba + google + jumio + sent_id + 
          linkedin +
          manual_offline + manual_online + weibo + photographer + 
          None +
          amex + verifications_count)

rv_rf_p_amen_ver <- (has_reviews_2016 ~  TV + Internet + Wireless.Internet +
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

words <- (has_reviews_2016 ~ word_stay + word_apartment + word_great + 
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

words_amen_ver_rf_rv_p <- (has_reviews_2016 ~  TV + Internet + Wireless.Internet +
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

loc <- (has_reviews_2016 ~ #neighbourhood_cleansed +
          neighbourhood_group_cleansed + #smart_location + 
          #city + 
          zipcode + market + #country + 
          is_location_exact)

amen_ver_loc_p_rv_rf <- (has_reviews_2016 ~  TV + Internet + Wireless.Internet +
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
                           is_location_exact)

non_rf <- (has_reviews_2016 ~ min_price + 
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
             word_bedroom + word_trip + word_times + word_kind)

rf_words <- (has_reviews_2016 ~ word_stay + word_apartment + word_great + 
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

rm_type <- (has_reviews_2016 ~ room_type)

loc_p <- (has_reviews_2016 ~ max_price + mean_price + min_price + 
            #neighbourhood_cleansed +
            neighbourhood_group_cleansed + #smart_location + 
            #city + 
            zipcode + market + #country + 
            is_location_exact)

#create a list of all the trees

trees <- c(amen, amen_rv_rf_p, amen_ver_loc_p_rv_rf, loc, loc_p, non_rf, price, 
           rf, rf_p, rf_rf, rf_words, rm_type, rv, rv_p, rv_rf_p_amen_ver, 
           rv_rf_p, ver, words, words_amen_ver_rf_rv_p)

#run a loop over each of these trees and return the AUC value

auc <- c()

for (i in trees) {
  tree <- rpart(i, data = train, 
                control = rpart.control(cp = 0.005))
  
  # evaluate on the test data
  # evaluate on the test data
  bestcp <- tree$cptable[which.min(tree$cptable[,"xerror"]), "CP"]
  tree_pruned <- prune(tree, cp = bestcp)
  predicted <- predict(tree_pruned, test)
  ROCR_predict <- prediction(predicted, test$has_reviews_2016)
  roc.perf <- performance(ROCR_predict, measure = "tpr", x.measure = "fpr")
  auc_pred <- performance(ROCR_predict, measure = "auc")
  print(auc_pred@y.values)
}

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
          zipcode + market + #country + 
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
                           zipcode + market + #country + 
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
            zipcode + market + #country + 
            is_location_exact)

trees <- c(amen, amen_rv_rf_p, amen_ver_loc_p_rv_rf, loc, loc_p, non_rf, price, 
           rf, rf_p, rf_rf, rf_words, rm_type, rv, rv_p, rv_rf_p_amen_ver, 
           rv_rf_p, ver, words, words_amen_ver_rf_rv_p)


auc <- c()

for (i in trees) {
  tree <- rpart(i, data = train, 
                control = rpart.control(cp = 0.005))
  
  # evaluate on the test data
  # evaluate on the test data
  bestcp <- tree$cptable[which.min(tree$cptable[,"xerror"]), "CP"]
  tree_pruned <- prune(tree, cp = bestcp)
  predicted <- predict(tree_pruned, test)
  ROCR_predict <- prediction(predicted, test$exist_in_2016)
  roc.perf <- performance(ROCR_predict, measure = "tpr", x.measure = "fpr")
  auc_pred <- performance(ROCR_predict, measure = "auc")
  print(auc_pred@y.values)
}
