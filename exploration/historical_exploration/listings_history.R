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

# save to RData
save(all_listings, all_listings_2015, all_listings_2016, file = "listings_history.RData")

load("listings_history.RData")

# change price fields to numeric
all_listings <- mutate(all_listings, price = as.numeric(gsub("[$,]", "", price)), 
                   weekly_price = as.numeric(gsub("[$,]", "", weekly_price)),
                   monthly_price = as.numeric(gsub("[$,]", "", monthly_price)),
                   security_deposit = as.numeric(gsub("[$,]", "", security_deposit)),
                   cleaning_fee = as.numeric(gsub("[$,]", "", cleaning_fee)),
                   extra_people = as.numeric(gsub("[$,]", "", extra_people)))

all_listings_2015 <- mutate(all_listings_2015, price = as.numeric(gsub("[$,]", "", price)), 
                       weekly_price = as.numeric(gsub("[$,]", "", weekly_price)),
                       monthly_price = as.numeric(gsub("[$,]", "", monthly_price)),
                       security_deposit = as.numeric(gsub("[$,]", "", security_deposit)),
                       cleaning_fee = as.numeric(gsub("[$,]", "", cleaning_fee)),
                       extra_people = as.numeric(gsub("[$,]", "", extra_people)))

all_listings_2016 <- mutate(all_listings_2016, price = as.numeric(gsub("[$,]", "", price)), 
                       weekly_price = as.numeric(gsub("[$,]", "", weekly_price)),
                       monthly_price = as.numeric(gsub("[$,]", "", monthly_price)),
                       security_deposit = as.numeric(gsub("[$,]", "", security_deposit)),
                       cleaning_fee = as.numeric(gsub("[$,]", "", cleaning_fee)),
                       extra_people = as.numeric(gsub("[$,]", "", extra_people)))



last_seen <- aggregate(last_scraped ~ id, all_listings_2015, max)
first_seen <- aggregate(last_scraped ~ id, all_listings_2015, min)
last_seen_review <- aggregate(last_review ~ id, all_listings_2015, max)
first_seen_review <- aggregate(first_review ~ id, all_listings_2015, min)
mean_price <- aggregate(price ~ id, all_listings_2015, mean)
min_price <- aggregate(price ~ id, all_listings_2015, min)
max_price <- aggregate(price ~ id, all_listings_2015, max)
total_occ_2015 <- all_listings_2015 %>% count(id)

names(last_seen)[names(last_seen) == 'last_scraped'] <- 'last_seen'
names(first_seen)[names(first_seen) == 'last_scraped'] <- 'first_seen'
names(last_seen_review)[names(last_seen_review) == 'last_review'] <- 'last_seen_review'
names(first_seen_review)[names(first_seen_review) == 'first_review'] <- 'first_seen_review'
names(mean_price)[names(mean_price) == 'price'] <- 'mean_price'
names(min_price)[names(min_price) == 'price'] <- 'min_price'
names(max_price)[names(max_price) == 'price'] <- 'max_price'
names(total_occ_2015)[names(total_occ_2015) == 'n'] <- 'total_occ_2015'



listings_history <- full_join(first_seen, last_seen, by="id")
listings_history <- mutate(listings_history, first_seen_month = format(as.Date(first_seen,format="%Y-%m-%d"),"%Y-%m"), 
                           last_seen_month = format(as.Date(last_seen,format="%Y-%m-%d"),"%Y-%m"))

listings_history <- full_join(listings_history, total_occ_2015, by="id")
listings_history <- full_join(listings_history, first_seen_review, by="id")
listings_history <- full_join(listings_history, last_seen_review, by="id")
listings_history <- full_join(listings_history, mean_price, by="id")
listings_history <- full_join(listings_history, min_price, by="id")
listings_history <- full_join(listings_history, max_price, by="id")

rm(first_seen, last_seen, total_occ_2015, first_seen_review, last_seen_review, mean_price, min_price, max_price)

all_listings_2015_temp <- full_join(all_listings_2015, listings_history, by= "id")

last_record <- filter(all_listings_2015_temp, last_scraped == last_seen)

last_rating <- distinct(select(last_record, host_id, id, host_since, review_scores_rating, number_of_reviews, host_is_superhost))
names(last_rating)[names(last_rating) == 'review_scores_rating'] <- 'last_rating'
names(last_rating)[names(last_rating) == 'host_is_superhost'] <- 'is_superhost_2015'

listings_history <- full_join(listings_history, last_rating, by="id")

rm(last_rating, last_record, all_listings_2015_temp)

names(listings_history)[names(listings_history) == 'id'] <- 'listing_id'
names(listings_history)[names(listings_history) == 'host_since'] <- 'start_date'
names(listings_history)[names(listings_history) == 'number_of_reviews'] <- 'number_of_reviews_2015'
listings_history <- listings_history[,c("host_id", "listing_id", "start_date", "first_seen", "last_seen", 
                                        "first_seen_month", "last_seen_month", "number_of_reviews_2015", 
                                        "first_seen_review", "last_seen_review","last_rating", 
                                        "total_occ_2015", "mean_price", "min_price", "max_price", "is_superhost_2015")]

####2016 listings history####

last_seen_2016 <- aggregate(last_scraped ~ id, all_listings_2016, max)
names(last_seen_2016)[names(last_seen_2016) == 'last_scraped'] <- 'last_seen_2016'

all_listings_2016_temp <- full_join(all_listings_2016, last_seen_2016, by= "id")

last_record_2016 <- filter(all_listings_2016_temp, last_scraped == last_seen_2016)
last_rating_2016 <- distinct(select(last_record_2016, id, number_of_reviews, host_is_superhost))
total_occ_2016 <- all_listings_2016 %>% count(id)

names(last_rating_2016)[names(last_rating_2016) == 'number_of_reviews'] <- 'number_of_reviews_2016'
names(last_rating_2016)[names(last_rating_2016) == 'host_is_superhost'] <- 'is_superhost_2016'
names(last_rating_2016)[names(last_rating_2016) == 'id'] <- 'listing_id'
names(total_occ_2016)[names(total_occ_2016) == 'n'] <- 'total_occ_2016'
names(total_occ_2016)[names(total_occ_2016) == 'id'] <- 'listing_id'

listings_history_2016 <- full_join(total_occ_2016, last_rating_2016, by="listing_id")
listings_history_2016 <- listings_history_2016[,c("listing_id","number_of_reviews_2016","total_occ_2016", "is_superhost_2016")]

listings_history <- full_join(listings_history, listings_history_2016, by="listing_id")


rm(last_seen_2016, all_listings_2016_temp, last_record_2016, last_rating_2016, total_occ_2016)

####purge####




