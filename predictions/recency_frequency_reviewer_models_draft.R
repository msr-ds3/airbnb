library(readr)
library(dplyr)

# prepping the data
# load US review data from create_reviewer_history run
rev_data <- read_csv("../raw_data/us_reviewer_data.csv")
rev_words <- read_csv("../raw_data/us_rev_wordFeatures.csv")
rev_data <- left_join(rev_data, rev_words, by = "reviewer_id")

# rename columns (change from generic to specific for 2015 time period)
names(rev_data)[names(rev_data) == "first_within_time_period"] <- "first_in_2015"
names(rev_data)[names(rev_data) == "first_month_within_time_period"] <- "first_month_in_2015"
names(rev_data)[names(rev_data) == "last_within_time_period"] <- "last_in_2015"
names(rev_data)[names(rev_data) == "last_month_within_time_period"] <- "last_month_in_2015"
names(rev_data)[names(rev_data) == "num_within_time_period"] <- "num_in_2015"
names(rev_data)[names(rev_data) == "text_within_time_period"] <- "text_in_2015"
names(rev_data)[names(rev_data) == "locations_within_time_period"] <- "locations_in_2015"

# mutate to create logical column of whether there is a review in 2016 or not
rev_data <- mutate(rev_data, has_review_2016 = num_in_2016 > 0)

# filtering out NAs
rev_data_filtered <- filter(rev_data, !is.na(text_in_2015))

# write to csv
#write_csv(rev_data, "../raw_data/us_rev_data.csv")


# modeling the data
# read the csv
rev_data <- read_csv("../raw_data/us_rev_data.csv", col_types = cols(word_na = "d"))
# need to specify col_type, parsing failures otherwise

################################################################################
# logistic regression model for whether there is a review or not in 2016 based
# on when the last review in 2015 was and the number of reviews in 2015
################################################################################
model1 <- glm(has_review_2016 ~ last_in_2015 + num_in_2015, data = rev_data,
              family = "binomial")

summary(model1)
# Call:
#   glm(formula = has_review_2016 ~ last_in_2015 + num_in_2015, family = "binomial", 
#       data = rev_data)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -5.5374  -0.5407  -0.5099  -0.4723   2.1784  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  -2.460e+01  4.298e-01  -57.24   <2e-16 ***
#   last_in_2015  1.335e-03  2.581e-05   51.74   <2e-16 ***
#   num_in_2015   3.779e-01  2.093e-03  180.55   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 1280259  on 1569094  degrees of freedom
# Residual deviance: 1241778  on 1569092  degrees of freedom
# (1432362 observations deleted due to missingness)
# AIC: 1241784
# 
# Number of Fisher Scoring iterations: 4

# !!! ^ roughly half were removed due to missingness

################################################################################
# linear model
################################################################################
model2 <- lm(num_in_2016 ~ last_month_in_2015 + num_in_2015, data = rev_data)
# Call:
#   lm(formula = num_in_2016 ~ last_month_in_2015 + num_in_2015, 
#      data = rev_data)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -7.997 -0.177 -0.148 -0.109 37.419 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        -0.1001320  0.0015120  -66.22   <2e-16 ***
#   last_month_in_2015  0.0096282  0.0001684   57.17   <2e-16 ***
#   num_in_2015         0.1710568  0.0005331  320.88   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.6371 on 1569092 degrees of freedom
# (1432362 observations deleted due to missingness)
# Multiple R-squared:  0.06518,	Adjusted R-squared:  0.06518 
# F-statistic: 5.47e+04 on 2 and 1569092 DF,  p-value: < 2.2e-16

# !!! ^ roughly half were removed due to missingness

################################################################################
# decision trees
################################################################################
library(rpart)
library(rpart.plot)
library(ROCR)

# expands nums from abbreviated to actual numbers
tot_count <- function(x, labs, digits, varlen)
{
  paste(labs, "\n\nn =", x$frame$n)
}

# substituted in before_2016, set of reviewers before 2016
before_2016 <- filter(rev_data, !is.na(first_month))
# change NAs in num_in_2015 column to 0 and empty text_in_2015 to empty strings
before_2016$num_in_2015[is.na(before_2016$num_in_2015)] <- 0
before_2016$text_in_2015[is.na(before_2016$text_in_2015)] <- ""
# adding columns for all time reviews as of 2015 and number of places stayed
before_2016 <- mutate(before_2016, all_time_as_of_2015 = all_time_reviews - num_in_2016,
                      weight = as.numeric(num_in_2016 > 0),
                      weight_lgl = num_in_2016 > 0)
                      #num_places_stayed = length(strsplit(locations_in_2015, "|")))
                      # doesn't work, have find a way to count values after split
# balancing weights
before_2016$weight[before_2016$weight == 1] <- 2
before_2016$weight[before_2016$weight == 0] <- 1

# january cohort, 54508 obs
jan_2015 <- filter(before_2016, first_month >= as.Date("2015-01-01") &
                     first_month <= as.Date("2015-01-31"))

# old sampling without weights
#indexes <- sample(1:nrow(jan_2015), size=0.2*nrow(jan_2015))
#rev_test=jan_2015[indexes, ]
#rev_train=jan_2015[-indexes, ]

# sampling with weights (january cohort)
rev_test <- sample_n(jan_2015, size=0.2*nrow(jan_2015))
rev_train <- filter(jan_2015, !jan_2015$reviewer_id %in% rev_test$reviewer_id)

rev_train_true <- filter(rev_train, weight_lgl == T)
rev_train_false <- filter(rev_train, weight_lgl == F)
rev_train_oversample <- rbind(head(rev_train_true, 6000), head(rev_train_false, 6000))

# sampling with weights
rev_test <- sample_n(before_2016, size=0.2*nrow(before_2016))
rev_train <- filter(before_2016, !before_2016$reviewer_id %in% rev_test$reviewer_id)

rev_train_true <- filter(rev_train, weight_lgl == T)
rev_train_false <- filter(rev_train, weight_lgl == F)
rev_train_oversample <- rbind(head(rev_train_true, 8000), head(rev_train_false, 8000))



# modeling for num in 2016
fit1 <- rpart(num_in_2016 ~ last_month_in_2015 + num_in_2015,
             data = rev_train, method = "anova")

plot(fit1)
text(fit1)
rpart.plot(fit1)
printcp(fit1)
# Regression tree:
#   rpart(formula = num_in_2016 ~ last_month_in_2015 + num_in_2015, 
#         data = rev_train, method = "anova")
# 
# Variables actually used in tree construction:
#   [1] num_in_2015
# 
# Root node error: 547787/1255464 = 0.43632
# 
# n=1255464 (1145702 observations deleted due to missingness)
# 
# CP nsplit rel error  xerror      xstd
# 1 0.033877      0   1.00000 1.00000 0.0103437
# 2 0.013978      1   0.96612 0.96613 0.0099477
# 3 0.010000      2   0.95215 0.95281 0.0094415

bestcp1 <- fit1$cptable[which.min(fit1$cptable[,"xerror"]), "CP"]

#prune tree using best cp
tree_pruned1 <- prune(fit1, cp = bestcp1)

plot(tree_pruned1, uniform = TRUE)
text(tree_pruned1, cex = 0.8, use.n = TRUE, xpd = TRUE)

prp(tree_pruned1, faclen = 0, cex = 0.8, extra = 1)

prp(tree_pruned1, faclen = 0, cex = 0.8, node.fun=tot_count)

#use predict to see the prediction
sample_predict1 <- predict(tree_pruned1, rev_test)

ROCR1 <- prediction(sample_predict1, rev_test$num_in_2016) # error
# comparing to itself for now, error rev_test is shorter than rev_train
roc.perf1 = performance(ROCR1, measure = "tpr", x.measure = "fpr")
plot(roc.perf1)



# with every feature, still only uses num_in_2015
fit1a <- rpart(num_in_2016 ~ first_month + first_diff_2015 + last_month + 
                 last_diff_2015 + first_in_2015 + first_month_in_2015 + 
                 last_in_2015 + last_month_in_2015 + num_in_2015,
              data = rev_train_oversample, method = "anova")

rpart.plot(fit1a)
plot(fit1a)
text(fit1a)

bestcp1a <- fit1a$cptable[which.min(fit1a$cptable[,"xerror"]), "CP"]

#prune tree using best cp
tree_pruned1a <- prune(fit1a, cp = bestcp1a)

plot(tree_pruned1a, uniform = TRUE)
text(tree_pruned1a, cex = 0.8, use.n = TRUE, xpd = TRUE)

prp(tree_pruned1a, faclen = 0, cex = 0.8, extra = 1)

prp(tree_pruned1a, faclen = 0, cex = 0.8, node.fun=tot_count)

#use predict to see the prediction
sample_predict1a <- predict(tree_pruned1a, rev_test)

ROCR1a <- prediction(sample_predict1a, rev_test$num_in_2015) # error
# error rev_test is shorter than rev_train
roc.perf1a = performance(ROCR1a, measure = "tpr", x.measure = "fpr")
plot(roc.perf1a)

# modeling for has review 2016
fit2 <- rpart(has_review_2016 ~ first_month + first_diff_2015 + last_month + 
                last_diff_2015 + first_in_2015 + first_month_in_2015 + 
                last_in_2015 + last_month_in_2015 + num_in_2015,
             data = rev_train, control = rpart.control(cp = 0.005, maxdepth = 5))

plot(fit2)
text(fit2)
rpart.plot(fit2)
printcp(fit2)
# Regression tree:
#   rpart(formula = has_review_2016 ~ last_month_in_2015 + num_in_2015 + 
#           last_diff_2015, data = rev_train, control = rpart.control(maxdepth = 5))
# 
# Variables actually used in tree construction:
#   [1] num_in_2015
# 
# Root node error: 190387/1815250 = 0.10488
# 
# n=1815250 (585916 observations deleted due to missingness)
# 
# CP nsplit rel error  xerror      xstd
# 1 0.026848      0   1.00000 1.00000 0.0017461
# 2 0.010000      1   0.97315 0.97315 0.0016852

bestcp2 <- fit2$cptable[which.min(fit2$cptable[,"xerror"]), "CP"]

#prune tree using best cp
tree_pruned2 <- prune(fit2, cp = bestcp2)

plot(tree_pruned2, uniform = TRUE)
text(tree_pruned2, cex = 0.8, use.n = TRUE, xpd = TRUE)

prp(tree_pruned2, faclen = 0, cex = 0.8, extra = 1)

prp(tree_pruned2, faclen = 0, cex = 0.8, node.fun=tot_count)

#use predict to see the prediction
sample_predict2 <- predict(tree_pruned2, rev_test)

ROCR2 <- prediction(sample_predict2, rev_test$has_review_2016) 
# comparing to itself for now, error rev_test is shorter than rev_train
roc.perf2 = performance(ROCR2, measure = "tpr", x.measure = "fpr")
performance(ROCR2, measure = "auc")
plot(roc.perf2)



# using all features
fit3 <- rpart(has_review_2016 ~ 
                fs_is_superhost_2015 +
                fs_mean_price +
                fs_host_listings_count +
                fs_host_since +
                fs_first_review_month_2015 +
                fs_min_price +
                fs_max_price +
                fs_room_type +
                fs_is_multilisting +
                ls_is_superhost_2015 +
                ls_mean_price +
                ls_host_listings_count +
                ls_host_since +
                ls_first_review_month_2015 +
                ls_min_price +
                ls_max_price +
                ls_room_type +
                ls_first_review_month_2015
                # first_month + first_diff_2015 + last_month +
                # last_diff_2015 + first_in_2015 + first_month_in_2015 +
                # last_in_2015 + last_month_in_2015 + num_in_2015 + all_time_as_of_2015 +
                # word_stay +
                # word_na +
                # word_great +
                # word_place +
                # word_host + word_house +
                # word_clean + word_nice +
                # word_home +
                # word_comfortable + word_location + word_room + word_apartment +
                # word_time + word_recommend +
                # word_perfect + word_beautiful +
                # word_made + word_area + word_easy + word_wonderful +
                # word_experience + word_enjoy + word_neighborhood + word_bed +
                # word_love + word_back + word_good + word_close + word_quiet +
                # word_welcome + word_need + word_walk + word_feel + word_helpful +
                # word_friendly + word_space + word_restaurant +
                # word_lovely +
                # word_night + word_amazing + word_beach + word_kitchen + word_highly +
                # word_lot + word_super + word_felt + word_make + word_day +
                # word_check + word_bathroom + word_airbnb + word_arrive + word_family +
                # word_convenient + word_cozy + word_visit + word_view +
                # word_accommodating + word_downtown + word_trip + word_walking +
                # word_coffee + word_didn + word_provided + word_located +
                # word_spacious + word_quick + word_arrival +
                # word_street +
                # word_question + word_breakfast + word_town + word_warm +
                # word_private + word_city + word_excellent + word_la + word_parking +
                # word_shop + word_gave + word_short + word_people + word_awesome +
                # word_distance + word_de + word_guest + word_fantastic + word_bedroom +
                # word_work + word_weekend + word_morning + word_park + word_extremely +
                # word_left + word_kind + word_safe + word_minutes + word_long +
                # word_list
              ,
              data = rev_train_oversample, control = rpart.control(cp = 0.005))

plot(fit3)
text(fit3)
rpart.plot(fit3)
printcp(fit3)

bestcp3 <- fit3$cptable[which.min(fit3$cptable[,"xerror"]), "CP"]

#prune tree using best cp
tree_pruned3 <- prune(fit3, cp = bestcp3)

plot(tree_pruned3, uniform = TRUE)
text(tree_pruned3, cex = 0.8, use.n = TRUE, xpd = TRUE)

prp(tree_pruned3, faclen = 0, cex = 0.8, extra = 1)

prp(tree_pruned3, faclen = 0, cex = 0.8, node.fun=tot_count)

rpart.plot(tree_pruned3)

#use predict to see the prediction
sample_predict3 <- predict(tree_pruned3, rev_test)

ROCR3 <- prediction(sample_predict3, rev_test$has_review_2016) 
roc.perf3 = performance(ROCR3, measure = "tpr", x.measure = "fpr")
performance(ROCR3, measure = "auc")

plot(roc.perf3)


# using words
fit4 <- rpart(has_review_2016 ~ 
                word_stay +
                word_na +
                word_great +
                word_place +
                word_host + word_house +
                word_clean + word_nice +
                word_home +
                word_comfortable + word_location + word_room + word_apartment +
                word_time + word_recommend +
                word_perfect + word_beautiful +
                word_made + word_area + word_easy + word_wonderful +
                word_experience + word_enjoy + word_neighborhood + word_bed +
                word_love + word_back + word_good + word_close + word_quiet +
                word_welcome + word_need + word_walk + word_feel + word_helpful +
                word_friendly + word_space + word_restaurant +
                word_lovely +
                word_night + word_amazing + word_beach + word_kitchen + word_highly +
                word_lot + word_super + word_felt + word_make + word_day +
                word_check + word_bathroom + word_airbnb + word_arrive + word_family +
                word_convenient + word_cozy + word_visit + word_view +
                word_accommodating + word_downtown + word_trip + word_walking +
                word_coffee + word_didn + word_provided + word_located +
                word_spacious + word_quick + word_arrival +
                word_street +
                word_question + word_breakfast + word_town + word_warm +
                word_private + word_city + word_excellent + word_la + word_parking +
                word_shop + word_gave + word_short + word_people + word_awesome +
                word_distance + word_de + word_guest + word_fantastic + word_bedroom +
                word_work + word_weekend + word_morning + word_park + word_extremely +
                word_left + word_kind + word_safe + word_minutes + word_long +
                word_list,
              data = rev_train_sample, control = rpart.control(cp = 0.005))

plot(fit4)
text(fit4)
rpart.plot(fit4)
printcp(fit4)


bestcp4 <- fit4$cptable[which.min(fit4$cptable[,"xerror"]), "CP"]

#prune tree using best cp
tree_pruned4 <- prune(fit4, cp = bestcp4)

plot(tree_pruned4, uniform = TRUE)
text(tree_pruned4, cex = 0.8, use.n = TRUE, xpd = TRUE)

prp(tree_pruned4, faclen = 0, cex = 0.8, extra = 1)

prp(tree_pruned4, faclen = 0, cex = 0.8, node.fun=tot_count)

rpart.plot(tree_pruned4)

#use predict to see the prediction
sample_predict4 <- predict(tree_pruned4, rev_test)

ROCR4 <- prediction(sample_predict4, rev_test$has_review_2016) 
roc.perf4 = performance(ROCR4, measure = "tpr", x.measure = "fpr")
performance(ROCR4, measure = "auc")
plot(roc.perf4)




################################################################################
# adding in features for first and last stay

features <- read_csv("../raw_data/review_listing_features.csv")

before_2016 <- inner_join(before_2016, features, by = "reviewer_id")

# resample

fit5 <- rpart(has_review_2016 ~ 
                fs_is_superhost_2015 +
                fs_mean_price +
                fs_host_listings_count +
                fs_host_since +
                fs_first_review_month_2015 + 
                fs_min_price +
                fs_max_price +
                fs_room_type +
                fs_is_multilisting + 
                ls_is_superhost_2015 + 
                ls_mean_price +
                ls_host_listings_count +
                ls_host_since +
                ls_first_review_month_2015 +
                ls_min_price +
                ls_max_price +
                ls_room_type +
                last_month_in_2015 + num_in_2015 +
                word_great,
              data = rev_train_oversample, control = rpart.control(cp = 0.001, maxdepth = 5))

plot(fit5)
text(fit5)
rpart.plot(fit5)
printcp(fit5)


prp(fit5, faclen = 0, cex = 0.8, node.fun=tot_count)


bestcp5 <- fit5$cptable[which.min(fit5$cptable[,"xerror"]), "CP"]

#prune tree using best cp
tree_pruned5 <- prune(fit5, cp = bestcp5)

plot(tree_pruned5, uniform = TRUE)
text(tree_pruned5, cex = 0.8, use.n = TRUE, xpd = TRUE)

prp(tree_pruned5, faclen = 0, cex = 0.8, extra = 1)

prp(tree_pruned5, faclen = 0, cex = 0.8, node.fun=tot_count)

rpart.plot(tree_pruned5)

#use predict to see the prediction
sample_predict5 <- predict(tree_pruned5, rev_test)

ROCR5 <- prediction(sample_predict5, rev_test$has_review_2016) 
roc.perf5 = performance(ROCR5, measure = "tpr", x.measure = "fpr")
performance(ROCR5, measure = "auc")
# Slot "y.values":
#   [[1]]
# [1] 0.6730168

plot(roc.perf5)


# regression
model <- glm(has_review_2016 ~ 
               # fs_is_superhost_2015 + 
               # fs_mean_price +
               # fs_host_listings_count +
               ls_is_superhost_2015 + 
               ls_mean_price +
               ls_host_listings_count +
               ls_host_since,
             data = rev_train, family = "binomial")
summary(model)

################################################################################
####### num_in_2015 > 1
#num_more_than_one <- filter(before_2016, num_in_2015 > 1)

#write_csv(num_more_than_one, "num_more_than_one.csv")

num_more_than_one <- read_csv("num_more_than_one.csv")

# sampling num_in_2015 > 0 cohort
rev_test <- sample_n(num_more_than_one, size=0.2*nrow(num_more_than_one))
rev_train <- filter(num_more_than_one, !num_more_than_one$reviewer_id %in% rev_test$reviewer_id)

rev_train <- mutate(rev_train, set = as.factor(sample(1:5,nrow(rev_train),replace=TRUE)))

# oversampling
rev_train_true <- filter(rev_train, weight_lgl == T)
rev_train_false <- filter(rev_train, weight_lgl == F)
rev_train_oversample <- rbind(head(rev_train_true, 8500), head(rev_train_false, 8500))
  
fit6 <- rpart(has_review_2016 ~ 
                fs_is_superhost_2015 +
                fs_mean_price +
                fs_host_listings_count +
                fs_host_since +
                fs_first_review_month_2015 +
                fs_min_price +
                fs_max_price +
                fs_room_type +
                fs_is_multilisting +
                ls_is_superhost_2015 +
                ls_mean_price +
                ls_host_listings_count +
                ls_host_since +
                ls_first_review_month_2015 +
                ls_min_price +
                ls_max_price +
                ls_room_type +
                ls_first_review_month_2015 +
                first_month + first_diff_2015 + last_month +
                last_diff_2015 + first_in_2015 + first_month_in_2015 +
                last_in_2015 + last_month_in_2015 + num_in_2015 + all_time_as_of_2015 +
                word_stay +
                word_na +
                word_great +
                word_place +
                word_host + word_house +
                word_clean + word_nice +
                word_home +
                word_comfortable + word_location + word_room + word_apartment +
                word_time + word_recommend +
                word_perfect + word_beautiful +
                word_made + word_area + word_easy + word_wonderful +
                word_experience + word_enjoy + word_neighborhood + word_bed +
                word_love + word_back + word_good + word_close + word_quiet +
                word_welcome + word_need + word_walk + word_feel + word_helpful +
                word_friendly + word_space + word_restaurant +
                word_lovely +
                word_night + word_amazing + word_beach + word_kitchen + word_highly +
                word_lot + word_super + word_felt + word_make + word_day +
                word_check + word_bathroom + word_airbnb + word_arrive + word_family +
                word_convenient + word_cozy + word_visit + word_view +
                word_accommodating + word_downtown + word_trip + word_walking +
                word_coffee + word_didn + word_provided + word_located +
                word_spacious + word_quick + word_arrival +
                word_street +
                word_question + word_breakfast + word_town + word_warm +
                word_private + word_city + word_excellent + word_la + word_parking +
                word_shop + word_gave + word_short + word_people + word_awesome +
                word_distance + word_de + word_guest + word_fantastic + word_bedroom +
                word_work + word_weekend + word_morning + word_park + word_extremely +
                word_left + word_kind + word_safe + word_minutes + word_long +
                word_list
              ,
              data = rev_train, control = rpart.control(cp = 0.001, maxdepth = 5))

plot(fit6)
text(fit6)
rpart.plot(fit6)
printcp(fit6)

prp(fit6, faclen = 0, cex = 0.8, node.fun=tot_count)

bestcp6 <- fit6$cptable[which.min(fit6$cptable[,"xerror"]), "CP"]

#prune tree using best cp
tree_pruned6 <- prune(fit6, cp = bestcp6)

plot(tree_pruned6, uniform = TRUE)
text(tree_pruned6, cex = 0.8, use.n = TRUE, xpd = TRUE)

prp(tree_pruned6, faclen = 0, cex = 0.8, extra = 1)

prp(tree_pruned6, faclen = 0, cex = 0.8, node.fun=tot_count)

rpart.plot(tree_pruned6)

#use predict to see the prediction
sample_predict6 <- predict(tree_pruned6, rev_test)

ROCR6 <- prediction(sample_predict6, rev_test$has_review_2016) 
roc.perf6 = performance(ROCR6, measure = "tpr", x.measure = "fpr")
performance(ROCR6, measure = "auc")
plot(roc.perf6)



# cross validation
performance <- list()

for(i in seq(1,5)) {
  
  rev_train_set <- filter(rev_train, set == i)
  
  fit6 <- rpart(has_review_2016 ~ 
                  fs_is_superhost_2015 +
                  fs_mean_price +
                  fs_host_listings_count +
                  fs_host_since +
                  fs_first_review_month_2015 +
                  ls_is_superhost_2015 + 
                  ls_mean_price +
                  ls_host_listings_count +
                  ls_host_since +
                  ls_first_review_month_2015 +
                  first_month + first_diff_2015 + last_month + 
                  last_diff_2015 + first_in_2015 + first_month_in_2015 + 
                  last_in_2015 + last_month_in_2015 + num_in_2015 +
                  word_stay +
                  word_na +
                  word_great +
                  word_place +
                  word_host + word_house +
                  word_clean + word_nice +
                  word_home +
                  word_comfortable + word_location + word_room + word_apartment +
                  word_time + word_recommend +
                  word_perfect + word_beautiful +
                  word_made + word_area + word_easy + word_wonderful +
                  word_experience + word_enjoy + word_neighborhood + word_bed +
                  word_love + word_back + word_good + word_close + word_quiet +
                  word_welcome + word_need + word_walk + word_feel + word_helpful +
                  word_friendly + word_space + word_restaurant +
                  word_lovely +
                  word_night + word_amazing + word_beach + word_kitchen + word_highly +
                  word_lot + word_super + word_felt + word_make + word_day +
                  word_check + word_bathroom + word_airbnb + word_arrive + word_family +
                  word_convenient + word_cozy + word_visit + word_view +
                  word_accommodating + word_downtown + word_trip + word_walking +
                  word_coffee + word_didn + word_provided + word_located +
                  word_spacious + word_quick + word_arrival +
                  word_street +
                  word_question + word_breakfast + word_town + word_warm +
                  word_private + word_city + word_excellent + word_la + word_parking +
                  word_shop + word_gave + word_short + word_people + word_awesome +
                  word_distance + word_de + word_guest + word_fantastic + word_bedroom +
                  word_work + word_weekend + word_morning + word_park + word_extremely +
                  word_left + word_kind + word_safe + word_minutes + word_long +
                  word_list,
                data = rev_train_set, control = rpart.control(cp = 0.001, maxdepth = 5))
  
  #plot(fit6)
  #text(fit6)
  #rpart.plot(fit6)
  printcp(fit6)
  
  #prp(fit6, faclen = 0, cex = 0.8, node.fun=tot_count)
  
  bestcp6 <- fit6$cptable[which.min(fit6$cptable[,"xerror"]), "CP"]
  
  #prune tree using best cp
  tree_pruned6 <- prune(fit6, cp = bestcp6)
  
  #plot(tree_pruned6, uniform = TRUE)
  #text(tree_pruned6, cex = 0.8, use.n = TRUE, xpd = TRUE)
  
  #prp(tree_pruned6, faclen = 0, cex = 0.8, extra = 1)
  
  #prp(tree_pruned6, faclen = 0, cex = 0.8, node.fun=tot_count)
  
  #rpart.plot(tree_pruned6)
  
  #use predict to see the prediction
  sample_predict6 <- predict(tree_pruned6, rev_test)
  
  ROCR6 <- prediction(sample_predict6, rev_test$has_review_2016) 
  roc.perf6 = performance(ROCR6, measure = "tpr", x.measure = "fpr")
  performance(ROCR6, measure = "auc")
  
  print("set performance")
  performance[i] = roc.perf6@y.values
}