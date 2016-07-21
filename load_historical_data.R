library(dplyr)
library(readr)

# load data into dataframes
dates <- c("2015-09-01","2015-10-01","2015-11-01","2015-11-20", "2015-12-02",
           "2016-01-01","2016-02-02","2016-04-03","2016-05-02","2016-06-02",
           "2016-07-02")

listing_names <- c("201509_listing","201510_listing", "201511_listing",
                   "201511_listing", "201512_listing", "201601_listing",
                   "201602_listing","201604_listing","201605_listing",
                   "201606_listing", "201607_listing")

for(date in dates){
  i = 1
  listing_names[i] <- read_csv(paste("dates", "listings.csv", sep="-"), na='\\N')
 
  # change price fields to numeric
  listing_names[i] <- mutate(listing_names[i], price = as.numeric(gsub("[$,]", "", price)), 
                     weekly_price = as.numeric(gsub("[$,]", "", weekly_price)),
                     monthly_price = as.numeric(gsub("[$,]", "", monthly_price)),
                     security_deposit = as.numeric(gsub("[$,]", "", security_deposit)),
                     cleaning_fee = as.numeric(gsub("[$,]", "", cleaning_fee)),
                     extra_people = as.numeric(gsub("[$,]", "", extra_people)))
  i = i + 1
}



# save to RData
save(listing_names, file = "airbnb_historical.RData")

View(listing_names[1])
