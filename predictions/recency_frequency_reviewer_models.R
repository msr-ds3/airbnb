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
write_csv(rev_data, "../raw_data/us_rev_data.csv")


# modeling the data
# read the csv
rev_data <- read_csv("../raw_data/us_rev_data.csv")

# logistic regression model for whether there is a review or not in 2016 based
# on when the last review in 2015 was and the number of reviews in 2015
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
# decision tree(s)
library(rpart)
library(rpart.plot)
library(ROCR)

set.seed(123)

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
                      num_places_stayed = length(strsplit(locations_in_2015, "|")))

# january cohort, 54508 obs
jan_2015 <- filter(before_2016, first_month >= as.Date("2015-01-01") & first_month <= as.Date("2015-01-31"))

indexes <- sample(1:nrow(jan_2015), size=0.2*nrow(jan_2015))
rev_test=jan_2015[indexes, ]
rev_train=jan_2015[-indexes, ]
#rev_train_sample <- sample_n(rev_train, 100000)

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
              data = na.omit(rev_train), method = "anova")
              # experimenting with filtering out NAs because of errors

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
sample_predict1a <- predict(tree_pruned1a, rev_train)

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

tot_count <- function(x, labs, digits, varlen) {
  paste(labs, "\n\nn =", x$frame$n)
}

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
                #first_month + first_diff_2015 + last_month + 
                #last_diff_2015 + first_in_2015 + first_month_in_2015 + 
                #last_in_2015 + last_month_in_2015 + num_in_2015 +
                #num_places_stayed + all_time_as_of_2015,
              data = rev_train, control = rpart.control(cp = 0.005))

plot(fit3)
text(fit3)
rpart.plot(fit3)
printcp(fit3)
# Regression tree:
#   rpart(formula = has_review_2016 ~ word_stay + word_na + word_great + 
#           word_place + word_host + word_house + word_clean + word_nice + 
#           word_home + word_comfortable + word_location + word_room + 
#           word_apartment + word_time + word_recommend + word_perfect + 
#           word_beautiful + word_made + word_area + word_easy + word_wonderful + 
#           word_experience + word_enjoy + word_neighborhood + word_bed + 
#           word_love + word_back + word_good + word_close + word_quiet + 
#           word_welcome + word_need + word_walk + word_feel + word_helpful + 
#           word_friendly + word_space + word_restaurant + word_lovely + 
#           word_night + word_amazing + word_beach + word_kitchen + word_highly + 
#           word_lot + word_super + word_felt + word_make + word_day + 
#           word_check + word_bathroom + word_airbnb + word_arrive + 
#           word_family + word_convenient + word_cozy + word_visit + 
#           word_view + word_accommodating + word_downtown + word_trip + 
#           word_walking + word_coffee + word_didn + word_provided + 
#           word_located + word_spacious + word_quick + word_arrival + 
#           word_street + word_question + word_breakfast + word_town + 
#           word_warm + word_private + word_city + word_excellent + word_la + 
#           word_parking + word_shop + word_gave + word_short + word_people + 
#           word_awesome + word_distance + word_de + word_guest + word_fantastic + 
#           word_bedroom + word_work + word_weekend + word_morning + 
#           word_park + word_extremely + word_left + word_kind + word_safe + 
#           word_minutes + word_long + word_list + first_month + first_diff_2015 + 
#           last_month + last_diff_2015 + first_in_2015 + first_month_in_2015 + 
#           last_in_2015 + last_month_in_2015 + num_in_2015 + num_places_stayed + 
#           all_time_as_of_2015, data = rev_train_sample, control = rpart.control(cp = 0.005))
# 
# Variables actually used in tree construction:
#   [1] all_time_as_of_2015 last_month         
# 
# Root node error: 10421/100000 = 0.10421
# 
# n= 100000 
# 
# CP nsplit rel error  xerror      xstd
# 1 0.0292866      0   1.00000 1.00000 0.0074805
# 2 0.0076324      1   0.97071 0.97074 0.0072250
# 3 0.0073413      2   0.96308 0.96374 0.0071851
# 4 0.0050000      3   0.95574 0.95598 0.0071258

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
