library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(scales)

listings1501 <- read_csv("../../raw_data/2015-01-01-listings.csv", na='\\N')
# No data for Feb 2015
listings1503 <- read_csv("../../raw_data/2015-03-01-listings.csv", na='\\N')
listings1504 <- read_csv("../../raw_data/2015-04-01-listings.csv", na='\\N')
listings1505 <- read_csv("../../raw_data/2015-05-01-listings.csv", na='\\N')
listings1506 <- read_csv("../../raw_data/2015-06-01-listings.csv", na='\\N')
# No data for July 2015
listings1508 <- read_csv("../../raw_data/2015-08-01-listings.csv", na='\\N')
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

#============================== functions ========================

#left_entire: returns how many listings are in this month, but not in the next month. Listings are entire home multilistings.
#             i.e. how many listings are gone from scrape date to scrape date
left_entire <- function(this_month, next_month){
  this_month <- this_month %>% filter(room_type == "Entire home/apt") %>% group_by(host_id) %>% mutate(host_count = n()) %>% filter(host_count > 1) 
  next_month <- next_month %>% filter(room_type == "Entire home/apt") %>% group_by(host_id) %>% mutate(host_count = n()) %>% filter(host_count > 1)
  
  gone_listings <- anti_join(this_month, next_month, by = 'id')
  nrow(gone_listings)
}

#create_left_database: create database of how many airbnbs are gone for that scrape date
listings_gone <- c()
scrape_date <- c()
create_left_database <- function(l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, l15, l16){
  scrape_date <- as.Date(c("2015-01-01", "2015-03-01", "2015-04-01", "2015-05-01", "2015-06-01", 
                           "2015-08-01", "2015-09-01","2015-10-01","2015-11-01","2015-11-20", 
                           "2015-12-02", "2016-01-01","2016-02-02","2016-04-03","2016-05-02", "2016-06-01"))

  listings_gone <- c(left_entire(l1, l2), left_entire(l2, l3), left_entire(l3, l4), left_entire(l4, l5), left_entire(l5, l6), 
                     left_entire(l6, l7), left_entire(l7, l8), left_entire(l8, l9), left_entire(l9, l10), left_entire(l10, l11),
                     left_entire(l10, l12), left_entire(l12, l13), left_entire(l13, l14), left_entire(l14, l15), left_entire(l15,l16),
                     0)
  
  listings_gone <- data.frame(scrape_date, listings_gone)
}

how_many_entire_left <- create_left_database(listings1501, listings1503, listings1504, listings1505, listings1506, 
                                             listings1508, listings1509, listings1510, listings1511, listings151120, 
                                             listings1512, listings1601, listings1602, listings1604, listings1605, listings1606)

View(how_many_entire_left)

#create a fraction of how many left
#first, find out how many entire homes are listed per scrape date
how_many_entire_homes <- function(listings){
  entire_homes <- listings %>% filter(room_type == "Entire home/apt") %>% group_by(host_id) %>% mutate(host_count = n())
  nrow(entire_homes)
}
how_many_entire_homes(listings1509) #17026 (check)

listings_gone_fraction <- c()
what_fraction_left <- function(database, l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, l15, l16){
  listings_gone_fraction <- c((database$listings_gone[1] / how_many_entire_homes(l1)) * 100,
                              (database$listings_gone[2] / how_many_entire_homes(l2)) * 100,
                              (database$listings_gone[3] / how_many_entire_homes(l3)) * 100,
                              (database$listings_gone[4] / how_many_entire_homes(l4)) * 100,
                              (database$listings_gone[5] / how_many_entire_homes(l5)) * 100,
                              (database$listings_gone[6] / how_many_entire_homes(l6)) * 100,
                              (database$listings_gone[7] / how_many_entire_homes(l7)) * 100,
                              (database$listings_gone[8] / how_many_entire_homes(l8)) * 100,
                              (database$listings_gone[9] / how_many_entire_homes(l9)) * 100,
                              (database$listings_gone[10] / how_many_entire_homes(l10)) * 100,
                              (database$listings_gone[11] / how_many_entire_homes(l11)) * 100,
                              (database$listings_gone[12] / how_many_entire_homes(l12)) * 100,
                              (database$listings_gone[13] / how_many_entire_homes(l13)) * 100,
                              (database$listings_gone[14] / how_many_entire_homes(l14)) * 100,
                              (database$listings_gone[15] / how_many_entire_homes(l15)) * 100,
                              (database$listings_gone[16] / how_many_entire_homes(l16)) * 100)
  listings_gone_fraction
}
how_many_entire_left$percent_that_left <- what_fraction_left(how_many_entire_left, 
                                                             listings1501, listings1503, listings1504, listings1505, listings1506, 
                                                             listings1508, listings1509, listings1510, listings1511, listings151120, 
                                                             listings1512, listings1601, listings1602, listings1604, listings1605, listings1606)
View(how_many_entire_left)

ggplot(aes(x=scrape_date), data = how_many_entire_left) + geom_point(aes(y=how_many_entire_left$percent_that_left))

###########################################################
#how many listings left Airbnb that month?
listings_that_left <- function(this_month, next_month){
  gone_listings <- anti_join(this_month, next_month, by = 'id')
  nrow(gone_listings)
}

listings_gone <- c()
create_left_database_all <- function(l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, l15, l16){
  scrape_date <- as.Date(c("2015-01-01", "2015-03-01", "2015-04-01", "2015-05-01", "2015-06-01", 
                           "2015-08-01", "2015-09-01","2015-10-01","2015-11-01","2015-11-20", 
                           "2015-12-02", "2016-01-01","2016-02-02","2016-04-03","2016-05-02", "2016-06-01"))
  
  listings_gone <- c(listings_that_left(l1, l2), listings_that_left(l2, l3), listings_that_left(l3, l4), listings_that_left(l4, l5), listings_that_left(l5, l6), 
                     listings_that_left(l6, l7), listings_that_left(l7, l8), listings_that_left(l8, l9), listings_that_left(l9, l10), listings_that_left(l10, l11),
                     listings_that_left(l11, l12), listings_that_left(l12, l13), listings_that_left(l13, l14), listings_that_left(l14, l15), listings_that_left(l15,l16),
                     0)
  
  listings_gone <- data.frame(scrape_date, listings_gone)
}

how_many_left <- create_left_database_all(listings1501, listings1503, listings1504, listings1505, listings1506, 
                                          listings1508, listings1509, listings1510, listings1511, listings151120, 
                                          listings1512, listings1601, listings1602, listings1604, listings1605, listings1606)

View(how_many_left)

#plot of how many total listings (of any listing type) left that month
ggplot(aes(scrape_date, listings_gone),data=how_many_left) + geom_line(group=1) + geom_point()

#what percent of all listings did the 'leave' constitute
all_listings_gone_fraction <- c()
what_fraction_left <- function(database, l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, l15){
  all_listings_gone_fraction <- c(database$listings_gone[1] / nrow(l1) * 100,
                                  (database$listings_gone[2] / nrow(l2)) * 100,
                                  (database$listings_gone[3] / nrow(l3)) * 100,
                                  (database$listings_gone[4] / nrow(l4)) * 100,
                                  (database$listings_gone[5] / nrow(l5)) * 100,
                                  (database$listings_gone[6] / nrow(l6)) * 100,
                                  (database$listings_gone[7] / nrow(l7)) * 100,
                                  (database$listings_gone[8] / nrow(l8)) * 100,
                                  (database$listings_gone[9] / nrow(l9)) * 100,
                                  (database$listings_gone[10] / nrow(l10)) * 100,
                                  (database$listings_gone[11] / nrow(l11)) * 100,
                                  (database$listings_gone[12] / nrow(l12)) * 100,
                                  (database$listings_gone[13] / nrow(l13)) * 100,
                                  (database$listings_gone[14] / nrow(l14)) * 100,
                                  (database$listings_gone[15] / nrow(l15)) * 100,
                                  (database$listings_gone[16] / nrow(l16)) * 100,
                                  0)
  all_listings_gone_fraction
}

how_many_left$total_listings <- c(nrow(listings1501), nrow(listings1503), nrow(listings1504), nrow(listings1505), nrow(listings1506), 
                                  nrow(listings1508), nrow(listings1509), nrow(listings1510), nrow(listings1511), nrow(listings151120),
                                  nrow(listings1512), nrow(listings1601), nrow(listings1602), nrow(listings1604), nrow(listings1605), nrow(listings1606))

how_many_left$percent_that_left <- what_fraction_left(how_many_left, 
                                                      listings1501, listings1503, listings1504, listings1505, listings1506, 
                                                      listings1508, listings1509, listings1510, listings1511, listings151120, 
                                                      listings1512, listings1601, listings1602, listings1604, listings1605, listings1606)
View(how_many_entire_left)

####################################################
#plotting only multi-listings, % that were gone from airbnb
ggplot() +
  geom_point(aes(scrape_date, percent_that_left, group=1), colour="blue", data=how_many_entire_left) + 
  geom_line(aes(scrape_date, percent_that_left, group=1), colour="blue", data=how_many_entire_left) + 
  scale_x_date(breaks=date_breaks("months"), labels=date_format("%b")) +
  scale_y_continuous(limits=c(0,20)) + 
  xlab("Month") + 
  ylab("% of Multi-listings Gone") 

#plotting total people that left vs total (entire, multi-) listings that left
ggplot() +
  geom_point(aes(scrape_date, percent_that_left, group=1), colour="blue", data=how_many_entire_left) + 
  geom_line(aes(scrape_date, percent_that_left, group=1), colour="blue", data=how_many_entire_left) + 
  geom_point(aes(scrape_date, percent_that_left, group=1), colour="red", data=how_many_left) +
  geom_line(aes(scrape_date, percent_that_left, group=1), colour="red", data=how_many_left) +
  scale_x_date(breaks=date_breaks("months"), labels=date_format("%b")) +
  scale_y_continuous(limits=c(0,20)) + 
  xlab("Month") + 
  ylab("% Multi-/Listings Gone") 

how_many_left$percent_that_left
how_many_entire_left$percent_that_left

#red = gone listings of all 3 categories
#blue = gone listings that are entire homes && multi-listings
# huge revelation! The 'purge' seen in feb-march 2016 is NOT  a purge. The blue line 
# fall in sync with the red line. Meaning: multi-entire are leaving at the same rate 
# as all the othe categories. The apparent 'purge' is just a reflection of general
# listings leaving Airbnb!
# However, graph also solidifies the fact that Airbnb purged in Nov 17 2015. There
# is an abnormal amount of listings that leave that aren't in sync with the rest of Airbnb
# listings leaving. BOOM.
