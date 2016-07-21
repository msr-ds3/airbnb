library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(scales)


listings1501 <- read_csv("raw_data/2015-01-01-listings.csv", na='\\N')
#no date for february
listings1503 <- read_csv("raw_data/2015-03-01-listings.csv", na='\\N')
listings1504 <- read_csv("raw_data/2015-04-01-listings.csv", na='\\N')
listings1505 <- read_csv("raw_data/2015-05-01-listings.csv", na='\\N')
listings1506 <- read_csv("raw_data/2015-06-01-listings.csv", na='\\N')
#no date for july
listings1508 <- read_csv("raw_data/2015-08-01-listings.csv", na='\\N')
listings1509 <- read_csv("raw_data/2015-09-01-listings.csv", na='\\N')
listings1510 <- read_csv("raw_data/2015-10-01-listings.csv", na='\\N')
listings1511 <- read_csv("raw_data/2015-11-01-listings.csv", na='\\N')
listings151120 <- read_csv("raw_data/2015-11-20-listings.csv", na='\\N') #watch out for this one! This is still November
listings1512 <- read_csv("raw_data/2015-12-02-listings.csv", na='\\N')


listings1601 <- read_csv("raw_data/2016-01-01-listings.csv", na='\\N')
listings1602 <- read_csv("raw_data/2016-02-02-listings.csv", na='\\N')
# No data for March 2016 :(
listings1604 <- read_csv("raw_data/2016-04-03-listings.csv", na='\\N') #APRIL! This is not March. March has been skipped.
listings1605 <- read_csv("raw_data/2016-05-02-listings.csv", na='\\N')
listings1606 <- read_csv("raw_data/2016-06-02-listings.csv", na='\\N')




###rbind data frames####

#get all column names
column_names <- colnames(listings1606)
missing <- setdiff(column_names, names(listings1501))  # Find names of missing columns
listings1501[missing] <- NA   


column_names <- colnames(listings1501)
missing <- setdiff(column_names, names(listings1503))  # Find names of missing columns
listings1503[missing] <- NA   

missing <- setdiff(column_names, names(listings1504))  # Find names of missing columns
listings1504[missing] <- NA   

missing <- setdiff(column_names, names(listings1505))  # Find names of missing columns
listings1505[missing] <- NA   

missing <- setdiff(column_names, names(listings1506))  # Find names of missing columns
listings1506[missing] <- NA   

missing <- setdiff(column_names, names(listings1508))  # Find names of missing columns
listings1508[missing] <- NA   

missing <- setdiff(column_names, names(listings1509))  # Find names of missing columns
listings1509[missing] <- NA   

missing <- setdiff(column_names, names(listings1510))  # Find names of missing columns
listings1510[missing] <- NA   

missing <- setdiff(column_names, names(listings1511))  # Find names of missing columns
listings1511[missing] <- NA

missing <- setdiff(column_names, names(listings1512))  # Find names of missing columns
listings1512[missing] <- NA   

missing <- setdiff(column_names, names(listings1601))  # Find names of missing columns
listings1601[missing] <- NA   

missing <- setdiff(column_names, names(listings1602))  # Find names of missing columns
listings1602[missing] <- NA   

missing <- setdiff(column_names, names(listings1604))  # Find names of missing columns
listings1604[missing] <- NA   

missing <- setdiff(column_names, names(listings1605))  # Find names of missing columns
listings1605[missing] <- NA   

missing <- setdiff(column_names, names(listings1606))  # Find names of missing columns
listings1606[missing] <- NA   

all_listings <- rbind(listings1501, listings1503, listings1504, listings1505, listings1506, 
                      listings1508, listings1509, listings1510, listings1511, listings1512,
                      listings1601, listings1602, listings1604, listings1605, listings1606 ) 
#run this for 2015#
all_listings_2015 <- rbind(listings1501, listings1503, listings1504, listings1505, listings1506, 
                      listings1508, listings1509, listings1510, listings1511, listings1512) 
#run this for 2016#
all_listings_2016 <- rbind(listings1601, listings1602, listings1604, listings1605, listings1606) 



# change price fields to numeric
all_listings <- mutate(all_listings, price = as.numeric(gsub("[$,]", "", price)), 
                   weekly_price = as.numeric(gsub("[$,]", "", weekly_price)),
                   monthly_price = as.numeric(gsub("[$,]", "", monthly_price)),
                   security_deposit = as.numeric(gsub("[$,]", "", security_deposit)),
                   cleaning_fee = as.numeric(gsub("[$,]", "", cleaning_fee)),
                   extra_people = as.numeric(gsub("[$,]", "", extra_people)))



last_seen <- aggregate(last_scraped ~ id, all_listings, max)
first_seen <- aggregate(last_scraped ~ id, all_listings, min)
last_seen_review <- aggregate(last_review ~ id, all_listings, max)
first_seen_review <- aggregate(first_review ~ id, all_listings, min)
mean_price <- aggregate(price ~ id, all_listings, mean)
min_price <- aggregate(price ~ id, all_listings, min)
max_price <- aggregate(price ~ id, all_listings, max)
total_occ <- all_listings %>% count(id)

names(last_seen)[names(last_seen) == 'last_scraped'] <- 'last_seen'
names(first_seen)[names(first_seen) == 'last_scraped'] <- 'first_seen'
names(last_seen_review)[names(last_seen_review) == 'last_review'] <- 'last_seen_review'
names(first_seen_review)[names(first_seen_review) == 'first_review'] <- 'first_seen_review'
names(mean_price)[names(mean_price) == 'price'] <- 'mean_price'
names(min_price)[names(min_price) == 'price'] <- 'min_price'
names(max_price)[names(max_price) == 'price'] <- 'max_price'
names(total_occ)[names(total_occ) == 'n'] <- 'total_occ'

listings_history <- full_join(first_seen, last_seen, by="id")
listings_history <- full_join(listings_history, total_occ, by="id")
listings_history <- full_join(listings_history, first_seen_review, by="id")
listings_history <- full_join(listings_history, last_seen_review, by="id")
listings_history <- full_join(listings_history, mean_price, by="id")
listings_history <- full_join(listings_history, min_price, by="id")
listings_history <- full_join(listings_history, max_price, by="id")

rm(first_seen, last_seen, total_occ, first_seen_review, last_seen_review, mean_price, min_price, max_price)

all_listings_temp <- full_join(all_listings, listings_history, by= "id")

last_record <- filter(all_listings_temp, last_scraped == last_seen)

last_rating <- data.frame()
last_rating <- distinct(select(last_record, host_id, id, host_since, review_scores_rating, number_of_reviews))
names(last_rating)[names(last_rating) == 'review_scores_rating'] <- 'last_rating'
listings_history <- full_join(listings_history, last_rating, by="id")

rm(last_rating, last_record, all_listings_temp)

names(listings_history)[names(listings_history) == 'id'] <- 'listing_id'
names(listings_history)[names(listings_history) == 'host_since'] <- 'start_date'
listing_history <- listings_history[,c("host_id", "listing_id", "start_date", "first_seen", "last_seen", 
                 "number_of_reviews", "first_seen_review", "last_seen_review", 
                 "last_rating", "total_occ", "mean_price", "min_price", "max_price")]


