#To generate linear regression for # of reviews 

library(readr)
library(lubridate)

listings_history <- read_csv("../raw_data/listing_history.csv")

#convert num reviews to a numeric
listings_history$num_reviews_2016 <- as.numeric(listings_history$num_reviews_2016)

#removed negative number of reviews 
#to replace NA's with 0's
listings_history$num_reviews_2016[is.na(listings_history$num_reviews_2016)] <- 0
listings_history_review <- listings_history  %>% filter(num_reviews_2016 >= 0) #%>% filter(!is.na(num_reviews_2016))

#create test and train set
indexes <- sample(1:nrow(listings_history_review), 
                  size=0.2*nrow(listings_history_review))
listings_history_test=listings_history_review[indexes, ]
listings_history_train=listings_history_review[-indexes, ]

model16 <- lm((log(num_reviews_2016 + 1)) ~ is_superhost_count_2015 + 
                is_superhost_2015,
              data = listings_history_train)
summary(model16)
View(listings_history)
#to predict number of reviews based on recency frequency in days
model1 <- lm((log(num_reviews_2016 + 1)) ~ is_superhost_count_2015 + 
               is_superhost_2015 + listing_recency_2015_days + 
               review_recency_2015_days + total_occ_2015, 
             data = listings_history_train)

summary(model1)
#This gives us an r-squared of .4414 and an adjusted r-squared of 0.4296

#same regression but using recency frequency in weeks

model2 <- lm((log(num_reviews_2016 + 1)) ~ is_superhost_count_2015 + 
               is_superhost_2015 + listing_recency_2015_weeks + 
               review_recency_2015_weeks + total_occ_2015, 
             data = listings_history_train)

summary(model2)
#R-squared of 0.4301 and adjusted r-squared of 4.181 (same as days)


#to predict number of reviews 2016 based on reviews in 2015

#first look at the correlation between reviews in 2015 and reviews in 2016
cor(listings_history$reviews_total_2015, listings_history$num_reviews_2016)
  #.42
cor(listings_history$num_reviews_2015, listings_history$num_reviews_2016)
  #.56 
#there is a stronger correlation between reviews in 2016 and the reviews 
#received in just 2015


#first create a model with running total of reviews through 2015
model3 <- lm((log(num_reviews_2016 + 1)) ~ last_rating + has_reviews_2015 + 
               reviews_total_2015, data = listings_history_train)
summary(model3)
#R-squared 0.1985, R-squared 0.1976


#create a model with reviews in 2015
model4 <- lm((log(num_reviews_2016 + 1)) ~ last_rating + has_reviews_2015 + 
               num_reviews_2015, data = listings_history_train)
summary(model4)
#R-squared 0.312, R-squared 0.3112

#create a model with using price to predict 2016 reviews

model5 <- lm((log(num_reviews_2016 + 1)) ~ mean_price + min_price + max_price,
             data = listings_history_train)
summary(model5)
#R-squared 0.00522 and adjusted r-squared 0.005162

#to create a model with recency frequency + reviews
#first with frequency in days
model6 <- lm((log(num_reviews_2016 + 1)) ~ is_superhost_count_2015 + 
               is_superhost_2015 + listing_recency_2015_days + 
               review_recency_2015_days + total_occ_2015 + last_rating + 
               has_reviews_2015 + num_reviews_2015, 
             data = listings_history_train)
summary(model6)
# R-squared = .4809 and adjusted r-squared 0.4694

#then with frequency in weeks (same results, different inputs)

model7 <- lm((log(num_reviews_2016 + 1)) ~ is_superhost_count_2015 + 
               is_superhost_2015 + listing_recency_2015_weeks + 
               review_recency_2015_weeks + total_occ_2015 + last_rating + 
               has_reviews_2015 + num_reviews_2015, 
             data = listings_history_train)

summary(model7)
# R-squared = .4809 and adjusted r-squared 0.4694

#create a model to calculate number of listings based on recency frequency 
#and price
#to predict number of reviews based on recency frequency in days
model8 <- lm((log(num_reviews_2016 + 1)) ~ is_superhost_count_2015 + 
               is_superhost_2015 + listing_recency_2015_days + 
               review_recency_2015_days + total_occ_2015 + mean_price + 
               min_price + max_price, 
             data = listings_history_train)
summary(model8)
#This gives us an r-squared of .4304 and an adjusted r-squared of 0.4184

#same regression but using recency frequency in weeks

model9 <- lm((log(num_reviews_2016 + 1)) ~ is_superhost_count_2015 + 
               is_superhost_2015 + listing_recency_2015_weeks + 
               review_recency_2015_weeks + total_occ_2015 + mean_price + 
               min_price + max_price, 
             data = listings_history_train)
summary(model9)
#R-squared of 0.4304 and an adjusted r-squared of 0.4184 (same as days)

#create a model to calculate number of listings based on reviews and price
#using reviews only in 2015
model10 <-lm((log(num_reviews_2016 + 1)) ~ last_rating + has_reviews_2015 + 
                num_reviews_2015 + mean_price + min_price + max_price, 
             data = listings_history_train)
summary(model10)
#r-squared 0.3124 and adj 0.3115

#using aggregate number of reviews
model11 <- lm((log(num_reviews_2016 + 1)) ~ last_rating + has_reviews_2015 + 
                          reviews_total_2015 + mean_price + min_price + max_price,
              data = listings_history_train)
summary(model11)
#r-squared 0.1994 adj 0.1984

#create a model to predict number of reviews in 2016 based on recency frequency,
#reviews, and price

#using recency in weeks and review number just in 2015
model12 <-lm((log(num_reviews_2016 + 1)) ~ is_superhost_count_2015 + 
               is_superhost_2015 + listing_recency_2015_weeks + 
               review_recency_2015_weeks + total_occ_2015 + 
               last_rating + has_reviews_2015 + num_reviews_2015 + mean_price + 
               min_price + max_price, data = listings_history_train)
summary(model12)
#rsquared 0.4811 adj 0.4695

#using recency in days and review number just in 2015
model13 <-lm((log(num_reviews_2016 + 1)) ~ is_superhost_count_2015 + 
               is_superhost_2015 + listing_recency_2015_days + 
               review_recency_2015_days + total_occ_2015 + 
               last_rating + has_reviews_2015 + num_reviews_2015 + mean_price + 
               min_price + max_price, data = listings_history_train)
summary(model13)
#rsquared 0.4811 adj 0.4695

#using recency in weeks and aggregate number of reviews
model14 <-lm((log(num_reviews_2016 + 1)) ~ is_superhost_count_2015 + 
               is_superhost_2015 + month(listings_history_train$last_seen) + 
               review_recency_2015_weeks + total_occ_2015 + 
               last_rating + has_reviews_2015 + reviews_total_2015 + mean_price + 
               min_price + max_price, data = listings_history_train)
summary(model14)
#rsquared 0.4603 adj 0.4483

#using recency in days and aggregate number of reviews
model15 <-lm((log(num_reviews_2016 + 1)) ~ is_superhost_count_2015 + 
               is_superhost_2015 + listing_recency_2015_days + 
               review_recency_2015_days + total_occ_2015 + 
               last_rating + has_reviews_2015 + reviews_total_2015 + mean_price + 
               min_price + max_price, data = listings_history_train)

summary(model15)
#rsquared 0.4603 adj 0.4483

