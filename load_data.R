library(dplyr)
library(readr)

# load data into dataframes
reviews <- read_csv("raw_data/reviews.csv", na='\\N')
listings <- read_csv("raw_data/listings.csv", na='\\N')
calendar <- read_csv("raw_data/calendar.csv", na='\\N')
neighbourhoods <- read_csv("raw_data/neighbourhoods.csv", na='\\N')

# change price fields to numeric
listings <- mutate(listings, price = as.numeric(gsub("[$,]", "", price)), 
              weekly_price = as.numeric(gsub("[$,]", "", weekly_price)),
              monthly_price = as.numeric(gsub("[$,]", "", monthly_price)),
              security_deposit = as.numeric(gsub("[$,]", "", security_deposit)),
              cleaning_fee = as.numeric(gsub("[$,]", "", cleaning_fee)),
              extra_people = as.numeric(gsub("[$,]", "", extra_people)))

calendar <- mutate(calendar, price = as.numeric(gsub("[$,]", "", price)))

# save to RData
save(listings, calendar, neighbourhoods, reviews, file = "airbnb.RData")