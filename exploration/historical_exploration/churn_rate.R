library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(scales)

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

#how many people leave Airbnb that month?


#how many people (that have multi-listings of entire homes) leave Airbnb that month? 
this_month <- listings1509 %>% filter(room_type == "Entire home/apt") %>% group_by(host_id) %>% mutate(host_count = n()) %>% filter(host_count > 1) 
nrow(this_month) #3047

next_month <- listings1510 %>% filter(room_type == "Entire home/apt") %>% group_by(host_id) %>% mutate(host_count = n()) %>% filter(host_count > 1)
nrow(next_month) #3088

gone_listings <- anti_join(this_month, next_month, by = 'id')
View(gone_listings)
nrow(gone_listings)

#make a function out of the above
left_entire <- function(this_month, next_month){
  this_month <- this_month %>% filter(room_type == "Entire home/apt") %>% group_by(host_id) %>% mutate(host_count = n()) %>% filter(host_count > 1) 
  next_month <- next_month %>% filter(room_type == "Entire home/apt") %>% group_by(host_id) %>% mutate(host_count = n()) %>% filter(host_count > 1)
  
  gone_listings <- anti_join(this_month, next_month, by = 'id')
  nrow(gone_listings)
}



#make database
grand_left <- c()
fraction_of_entire_homes <- 
create_left_database <- function(l1, l2, l3, l4, l5, l6, l7, l8, l9, l10){
  scrape_date <- c(scrape_date, "2015-09")
  scrape_date <- c(scrape_date, "2015-10")
  scrape_date <- c(scrape_date, "2015-11")
  scrape_date <- c(scrape_date, "2015-11-20")
  scrape_date <- c(scrape_date, "2015-12")
  scrape_date <- c(scrape_date, "2016-01")
  scrape_date <- c(scrape_date, "2016-02")
  scrape_date <- c(scrape_date, "2016-04")
  scrape_date <- c(scrape_date, "2016-05")

  grand_left <- c(grand_left, left_entire(l1, l2))
  grand_left <- c(grand_left, left_entire(l2, l3))
  grand_left <- c(grand_left, left_entire(l3, l4))
  grand_left <- c(grand_left, left_entire(l4, l5))
  grand_left <- c(grand_left, left_entire(l5, l6))
  grand_left <- c(grand_left, left_entire(l6, l7))
  grand_left <- c(grand_left, left_entire(l7, l8))
  grand_left <- c(grand_left, left_entire(l8, l9))
  grand_left <- c(grand_left, left_entire(l9, l10))
  
  grand_left <- data.frame(scrape_date, grand_left)
}

how_many_entire_left <- create_left_database(listings1509, listings1510, listings1511, listings151120, listings1512, 
            listings1601, listings1602, listings1604, listings1605, listings1606)

View(how_many_entire_left)

#create a fraction of how many left
#first, find out how many entire homes are listed per scrape date
how_many_entire_homes <- function(listings){
  entire_homes <- listings %>% filter(room_type == "Entire home/apt") %>% group_by(host_id) %>% mutate(host_count = n())
  nrow(entire_homes)
}
how_many_entire_homes(listings1509)