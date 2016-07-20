library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(scales)

listings1509 <- read_csv("../../raw_data/2015-09-01-listings.csv", na='\\N')
listings1510 <- read_csv("../../raw_data/2015-10-01-listings.csv", na='\\N')
listings1511 <- read_csv("../../raw_data/2015-11-01-listings.csv", na='\\N')
listings151120 <- read_csv("../../raw_data/2015-11-20-listings.csv", na='\\N') 
#watch out for this one! This is still November
listings1512 <- read_csv("../../raw_data/2015-12-02-listings.csv", na='\\N')
listings1601 <- read_csv("../../raw_data/2016-01-01-listings.csv", na='\\N')
listings1602 <- read_csv("../../raw_data/2016-02-02-listings.csv", na='\\N')
# No data for March 2016 :(
listings1604 <- read_csv("../../raw_data/2016-04-03-listings.csv", na='\\N') 
#APRIL! This is not March. March has been skipped.
listings1605 <- read_csv("../../raw_data/2016-05-02-listings.csv", na='\\N')
listings1606 <- read_csv("../../raw_data/2016-06-02-listings.csv", na='\\N')

#fix prices for listings
listings1511 <- mutate(listings1511, price = as.numeric(gsub("[$,]", "", price)), 
                       weekly_price = as.numeric(gsub("[$,]", "", weekly_price)),
                       monthly_price = as.numeric(gsub("[$,]", "", monthly_price)),
                       security_deposit = as.numeric(gsub("[$,]", "", security_deposit)),
                       cleaning_fee = as.numeric(gsub("[$,]", "", cleaning_fee)),
                       extra_people = as.numeric(gsub("[$,]", "", extra_people)))
listings151120 <- mutate(listings151120, price = as.numeric(gsub("[$,]", "", price)), 
                         weekly_price = as.numeric(gsub("[$,]", "", weekly_price)),
                         monthly_price = as.numeric(gsub("[$,]", "", monthly_price)),
                         security_deposit = as.numeric(gsub("[$,]", "", security_deposit)),
                         cleaning_fee = as.numeric(gsub("[$,]", "", cleaning_fee)),
                         extra_people = as.numeric(gsub("[$,]", "", extra_people)))


#to find listings that were purged
purged_listings <- anti_join(prepurge, postpurge, by = 'id')
View(purged_listings)

purged_tf <- c()
for(i in 1:nrow(prepurge)){
  if(prepurge[i,]$id %in% purged_listings$id){
    purged_tf <- c(purged_tf, TRUE)
  } else {
    purged_tf <- c(purged_tf, FALSE)
  }
}

prepurge$purged <- purged_tf
View(prepurge)

nrow(prepurge)
prepurge <- data.frame(prepurge)

summary(purged_listings)


# looking at pre and post purge

prepurge <- listings1511 %>% filter(room_type == "Entire home/apt") %>% 
  group_by(host_id)%>% mutate(host_count = n()) %>% filter(host_count > 1) %>% 
  arrange(host_id)

View(prepurge)
nrow(prepurge) #3331

#find number of hostings per host before the purge
host_1511 <- data.frame(prepurge$host_id, prepurge$host_count)
host_count_1511 <- host_1511 %>% distinct(prepurge.host_id)
colnames(host_count_1511) <- c("host_id", "host_count")
View(host_count_1511)


postpurge <- listings151120 %>% filter(room_type == "Entire home/apt") %>% 
  group_by(host_id) %>% mutate(host_count = n()) %>% filter(host_count > 1) %>% 
  arrange(host_id)
View(postpurge)
nrow(postpurge) #1829

#find number of hostings per host after the purge
host_151120 <- data.frame(postpurge$host_id, postpurge$host_count)
host_count_151120 <- host_151120 %>% distinct(postpurge.host_id)
colnames(host_count_151120) <- c("host_id", "host_count")


#create a table of the host counts before and after purge
purge_host_count <- inner_join(host_count_1511, host_count_151120, 
                               by = "host_id")
#rename columns
colnames(purge_host_count) <- c("host_id", "count_prepurge", "count_postpurge")
#add column to find the change
purge_host_count$change <- 
  purge_host_count$count_postpurge - purge_host_count$count_prepurge
#all the 2s stay the same 
#some 3s were fine and some were purged
#no one was wiped down to 1


#filtered out the one outlier for 28

purge_host_count_clean <- purge_host_count %>% filter(count_postpurge < 28)
#plot to see the pre v post host count
ggplot(purge_host_count_clean, aes(x = count_prepurge, y = count_postpurge)) + 
  geom_point() + geom_abline(col = "red")

#look at those that were not in the pre and post
pre_post_host_count <- anti_join(host_count_1511, host_count_151120, 
                                 by = "host_id")

#filter for just those that occur 3 times
purge_host_count_clean_3 <- purge_host_count_clean %>% 
  filter(count_prepurge == 3)
View(purge_host_count_clean_3)

#find the number of listings that increased by 1, 0 change, and decreased by 1
purge_3_count <- purge_host_count_clean_3 %>% group_by(change) %>% 
  summarize(total = n()) 
#add column to calculate the percentage
purge_3_count$percent <- purge_3_count$total / sum(purge_3_count$total)
#72% had no change

#Why did they purge those and not others

#create new table of just those that have host count of three to isolate this 
#group
host_count_three <- prepurge %>% filter(host_count == 3)

#join that with host count clean 3 to find change
three_listings <- inner_join(host_count_three, purge_host_count_clean_3, 
                             by = "host_id")
#find those listings which were purged
three_listings_purge <- three_listings %>% filter(change == -1)
summary(three_listings_purge)
#there are 96 of these hosts
#price 65 to 699, mean 208.4
#review scores 50 to 100, mean 88.89
#find those listings not purged
three_listings_notpurged <- three_listings %>% filter(change == 0)
summary(three_listings_notpurged)
#there are 282 of these hosts
#price 79 to 1600, mean 270.1


#create a bare prepurge file for the logistic regression

prepurge_regression <- prepurge %>% select(host_since, host_response_time, 
                                           host_response_rate, 
                                           host_acceptance_rate, 
host_is_superhost, neighbourhood_cleansed, neighbourhood_group_cleansed, 
is_location_exact, property_type, accommodates, 
bathrooms, bedrooms, beds, bed_type, price, guests_included, extra_people, 
minimum_nights, maximum_nights, availability_30, 
availability_60, availability_90, availability_365, number_of_reviews, 
first_review, last_review, review_scores_rating, 
review_scores_accuracy, review_scores_cleanliness, review_scores_checkin, 
review_scores_communication, review_scores_location, 
review_scores_value, instant_bookable, reviews_per_month, cancellation_policy, 
require_guest_profile_picture, 
require_guest_phone_verification, host_count, purged)

prepurge_regression2 <- na.omit(prepurge_regression)

#####
#create matrix for glmnet
prepurge_matrix <- model.matrix(purged~., prepurge_regression2)
purge_matrix <- prepurge_regression2$purged

#ridge logistic regression

#create test and train set
train <- sample(1:nrow(prepurge_matrix), nrow(prepurge_matrix)/2)
test <- (-train)
purge_matrix.test=purge_matrix[test]
#creates model
ridge_mod=glmnet(prepurge_matrix[train,], purge_matrix[train], 
                 family = "binomial", alpha = 0)
#predict on the test data
ridge_pred=predict(ridge_mod, s=4, newx = prepurge_matrix[test,])
set.seed(1)
cv_out=cv_glmnet(prepurge_matrix[train,], purge_matrix[train], alpha=0)
#view the output of best lambda
plot(cv_out)
#find the best lambda
bestlam=cv_out$lambda.min
bestlam
#5.237242
#use this best lambda to predict on test
ridge_pred=predict(ridge_mod, s= bestlam, newx=prepurge_matrix[test,], 
                   type = "response")
#find the MSE on the test set
mean((ridge_pred-purge_matrix.test)^2)
#0.4612706
#View the new coefficients based on the best lambda
out=glmnet(prepurge_matrix, purge_matrix,alpha = 0)
predict(out, type = "coefficients", s=bestlam)

#lasso regression
#model
lasso.mod = glmnet(prepurge_matrix[train,], purge_matrix[train], 
                   family = "binomial", alpha = 1)
#perform cross validation on the training datat
set.seed(1)
cv.out=cv.glmnet(prepurge_matrix[train,], purge_matrix[train], alpha=1)
#view the plot of the different MSE's and lambda values
plot(cv.out)
#find the best lambda value
bestlam=cv.out$lambda.min
bestlam
#0.01755315
#use the best lambda to predict on the test data
lasso.pred=predict(lasso.mod, s=bestlam, newx = prepurge_matrix[test,])
#find the MSE on the test data
mean((lasso.pred-purge_matrix.test)^2)
#0.9216128
#using the best lambda view the coefficients
out=glmnet(prepurge_matrix, purge_matrix, alpha = 1)
lasso_coef= predict(out, type = "coefficients", s=bestlam)
lasso_coef


