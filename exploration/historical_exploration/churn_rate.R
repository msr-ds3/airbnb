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
listings_gone <- c()
scrape_date <- c()
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

  listings_gone <- c(listings_gone, left_entire(l1, l2))
  listings_gone <- c(listings_gone, left_entire(l2, l3))
  listings_gone <- c(listings_gone, left_entire(l3, l4))
  listings_gone <- c(listings_gone, left_entire(l4, l5))
  listings_gone <- c(listings_gone, left_entire(l5, l6))
  listings_gone <- c(listings_gone, left_entire(l6, l7))
  listings_gone <- c(listings_gone, left_entire(l7, l8))
  listings_gone <- c(listings_gone, left_entire(l8, l9))
  listings_gone <- c(listings_gone, left_entire(l9, l10))
  
  listings_gone <- data.frame(scrape_date, listings_gone)
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

listings_gone_fraction <- c()
what_fraction_left <- function(database, l1, l2, l3, l4, l5, l6, l7, l8, l9){
  listings_gone_fraction <- c(listings_gone_fraction, (database$listings_gone[1] / how_many_entire_homes(l1)) * 100)
  listings_gone_fraction <- c(listings_gone_fraction, (database$listings_gone[2] / how_many_entire_homes(l2)) * 100)
  listings_gone_fraction <- c(listings_gone_fraction, (database$listings_gone[3] / how_many_entire_homes(l3)) * 100)
  listings_gone_fraction <- c(listings_gone_fraction, (database$listings_gone[4] / how_many_entire_homes(l4)) * 100)
  listings_gone_fraction <- c(listings_gone_fraction, (database$listings_gone[5] / how_many_entire_homes(l5)) * 100)
  listings_gone_fraction <- c(listings_gone_fraction, (database$listings_gone[6] / how_many_entire_homes(l6)) * 100)
  listings_gone_fraction <- c(listings_gone_fraction, (database$listings_gone[7] / how_many_entire_homes(l7)) * 100)
  listings_gone_fraction <- c(listings_gone_fraction, (database$listings_gone[8] / how_many_entire_homes(l8)) * 100)
  listings_gone_fraction <- c(listings_gone_fraction, (database$listings_gone[9] / how_many_entire_homes(l9)) * 100)
  listings_gone_fraction
}
how_many_entire_left$percent_that_left <- what_fraction_left(how_many_entire_left, listings1509, listings1510, listings1511, listings151120, listings1512, 
                                        listings1601, listings1602, listings1604, listings1605)
View(how_many_entire_left)

ggplot(aes(x=scrape_date), data = how_many_entire_left) + geom_point(aes(y=how_many_entire_left$percent_that_left))

###########################################################
#how many listings left Airbnb that month?
listings_that_left <- function(this_month, next_month){
  gone_listings <- anti_join(this_month, next_month, by = 'id')
  nrow(gone_listings)
}

listings_gone <- c()
create_left_database_all <- function(l1, l2, l3, l4, l5, l6, l7, l8, l9, l10){
  scrape_date <- c(scrape_date, "2015-09")
  scrape_date <- c(scrape_date, "2015-10")
  scrape_date <- c(scrape_date, "2015-11")
  scrape_date <- c(scrape_date, "2015-11-20")
  scrape_date <- c(scrape_date, "2015-12")
  scrape_date <- c(scrape_date, "2016-01")
  scrape_date <- c(scrape_date, "2016-02")
  scrape_date <- c(scrape_date, "2016-04")
  scrape_date <- c(scrape_date, "2016-05")
  
  listings_gone <- c(listings_gone, listings_that_left(l1, l2))
  listings_gone <- c(listings_gone, listings_that_left(l2, l3))
  listings_gone <- c(listings_gone, listings_that_left(l3, l4))
  listings_gone <- c(listings_gone, listings_that_left(l4, l5))
  listings_gone <- c(listings_gone, listings_that_left(l5, l6))
  listings_gone <- c(listings_gone, listings_that_left(l6, l7))
  listings_gone <- c(listings_gone, listings_that_left(l7, l8))
  listings_gone <- c(listings_gone, listings_that_left(l8, l9))
  listings_gone <- c(listings_gone, listings_that_left(l9, l10))
  
  listings_gone <- data.frame(scrape_date, listings_gone)
}

how_many_left <- create_left_database_all(listings1509, listings1510, listings1511, listings151120, listings1512, 
                                          listings1601, listings1602, listings1604, listings1605, listings1606)

View(how_many_left)

#plot of how many total listings (of any listing type) left that month
ggplot(aes(scrape_date, listings_gone),data=how_many_left) + geom_point()

#what percent of all listings did the 'leave' constitute
all_listings_gone_fraction <- c()
what_fraction_left <- function(database, l1, l2, l3, l4, l5, l6, l7, l8, l9){
  all_listings_gone_fraction <- c(all_listings_gone_fraction, (database$listings_gone[1] / nrow(l1)) * 100)
  all_listings_gone_fraction <- c(all_listings_gone_fraction, (database$listings_gone[2] / nrow(l2)) * 100)
  all_listings_gone_fraction <- c(all_listings_gone_fraction, (database$listings_gone[3] / nrow(l3)) * 100)
  all_listings_gone_fraction <- c(all_listings_gone_fraction, (database$listings_gone[4] / nrow(l4)) * 100)
  all_listings_gone_fraction <- c(all_listings_gone_fraction, (database$listings_gone[5] / nrow(l5)) * 100)
  all_listings_gone_fraction <- c(all_listings_gone_fraction, (database$listings_gone[6] / nrow(l6)) * 100)
  all_listings_gone_fraction <- c(all_listings_gone_fraction, (database$listings_gone[7] / nrow(l7)) * 100)
  all_listings_gone_fraction <- c(all_listings_gone_fraction, (database$listings_gone[8] / nrow(l8)) * 100)
  all_listings_gone_fraction <- c(all_listings_gone_fraction, (database$listings_gone[9] / nrow(l9)) * 100)
  all_listings_gone_fraction
}

how_many_left$total_listings <- c(nrow(listings1509), nrow(listings1510), nrow(listings1511), nrow(listings151120), nrow(listings1512), 
                                                      nrow(listings1601), nrow(listings1602), nrow(listings1604), nrow(listings1605))
how_many_left$percent_that_left <- what_fraction_left(how_many_left, listings1509, listings1510, listings1511, listings151120, listings1512, 
                                                             listings1601, listings1602, listings1604, listings1605)
View(how_many_entire_left)

####################################################3
#plotting total people that left vs total (entire, multi-) listings that left
ggplot() + geom_line(aes(scrape_date, percent_that_left, group=1), colour="blue", data=how_many_entire_left) + geom_line(aes(scrape_date, percent_that_left, group=1), colour="red", data=how_many_left)
#red = gone listings of all 3 categories
#blue = gone listings that are entire homes && multi-listings
# huge revelation! The 'purge' seen in feb-march 2016 is NOT  a purge. The blue line 
# fall in sync with the red line. Meaning: multi-entire are leaving at the same rate 
# as all the othe categories. The apparent 'purge' is just a reflection of general
# listings leaving Airbnb!
# However, graph also solidifies the fact that Airbnb purged in Nov 17 2015. There
# is an abnormal amount of listings that leave that aren't in sync with the rest of Airbnb
# listings leaving. BOOM.
