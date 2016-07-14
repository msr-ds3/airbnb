library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)

listings1509 <- read_csv("../../raw_data/2015-09-01-listings.csv", na='\\N')
listings1510 <- read_csv("../../raw_data/2015-10-01-listings.csv", na='\\N')
listings1511 <- read_csv("../../raw_data/2015-11-01-listings.csv", na='\\N')
listings151120 <- read_csv("../../raw_data/2015-11-20-listings.csv", na='\\N') #watch out for this one! This is still November
listings1512 <- read_csv("../../raw_data/2015-12-02-listings.csv", na='\\N')
listings1601 <- read_csv("../../raw_data/2016-01-01-listings.csv", na='\\N')
listings1602 <- read_csv("../../raw_data/2016-02-02-listings.csv", na='\\N')
# No data for March 2016 :(
listings1604 <- read_csv("../../raw_data/2016-04-03-listings.csv", na='\\N') #APRIL! This is not March. March has been skipped.
listings1605 <- read_csv("../../raw_data/2016-05-02-listings.csv", na='\\N')
listings1606 <- read_csv("../../raw_data/2016-06-02-listings.csv", na='\\N')

#function to find % of multilistings
percent_multilistings <- function(listings){
  multilistings <- listings %>% select(host_id, room_type) %>% filter(room_type == "Entire home/apt") %>% group_by(host_id) %>% mutate(host_count = n()) %>% filter(host_count > 1) %>% arrange(host_id)
  listings_entire_apt <- listings %>% select(host_id, room_type) %>% filter(room_type == "Entire home/apt")
  nrow(multilistings)/ nrow(listings_entire_apt) * 100
}

#percentage of multilistings
dates <- as.Date(c("2015-09-01","2015-10-01","2015-11-01","2015-11-20", "2015-12-02", "2016-01-01","2016-02-02","2016-04-03","2016-05-02","2016-06-02"))
percent_of_multilistings <- c(september15, october15, november15, november_late15, december15, january16, february16, april16, may16, june16)

df_multilistings <- data.frame(dates, percent_of_multilistings)
View(df_multilistings)

ggplot(aes(dates, percent_of_multilistings), data=df_multilistings) + geom_point() +geom_line()
  
september15 <- percent_multilistings(listings1509)
october15<- percent_multilistings(listings1510)
november15 <- percent_multilistings(listings1511)
november_late15 <- percent_multilistings(listings151120)
december15 <- percent_multilistings(listings1512)
january16 <- percent_multilistings(listings1601)
february16 <- percent_multilistings(listings1602)
# March missing
april16 <- percent_multilistings(listings1604)
may16 <- percent_multilistings(listings1605)
june16 <-percent_multilistings(listings1606)
