library(dplyr)
library(readr)

listings1509 <- read_csv("../../raw_data/2015-09-01-listings.csv", na='\\N')
listings1510 <- read_csv("../../raw_data/2015-10-01-listings.csv", na='\\N')
listings1511 <- read_csv("../../raw_data/2015-11-01-listings.csv", na='\\N')
listings151120 <- read_csv("../../raw_data/2015-11-20-listings.csv", na='\\N')
listings1512 <- read_csv("../../raw_data/2015-12-02-listings.csv", na='\\N')
listings1601 <- read_csv("../../raw_data/2016-01-01-listings.csv", na='\\N')
listings1602 <- read_csv("../../raw_data/2016-02-02-listings.csv", na='\\N')
listings1604 <- read_csv("../../raw_data/2016-04-03-listings.csv", na='\\N')
listings1605 <- read_csv("../../raw_data/2016-05-02-listings.csv", na='\\N')
listings1606 <- read_csv("../../raw_data/2016-06-02-listings.csv", na='\\N')

# change price fields to numeric
all_listings = c(listings1509, listings1510, listings1511, listings151120, listings1512,
                listings1601, listings1602, listings1604, listings1605, listings1606)

for (i in all_listings){
  all_listings[i] <- mutate(all_listings[i], price = as.numeric(gsub("[$,]", "", price)), 
                     weekly_price = as.numeric(gsub("[$,]", "", weekly_price)),
                     monthly_price = as.numeric(gsub("[$,]", "", monthly_price)),
                     security_deposit = as.numeric(gsub("[$,]", "", security_deposit)),
                     cleaning_fee = as.numeric(gsub("[$,]", "", cleaning_fee)),
                     extra_people = as.numeric(gsub("[$,]", "", extra_people)))
}

View(all_listings)
