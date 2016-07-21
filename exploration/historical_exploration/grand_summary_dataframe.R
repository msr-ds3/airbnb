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

#create a function that outputs summary statistics for one scrape
how_many_listings <- function(listings){
  nrow(listings)
}

how_many_entire <- function(listings){
  entire <- listings %>% filter(room_type == "Entire home/apt") %>% group_by(host_id) %>% mutate(host_count = n())
  nrow(entire)
}

how_many_multilistings <- function(listings){
  mult <- listings %>% filter(room_type == "Entire home/apt") %>% group_by(host_id) %>% mutate(host_count = n()) %>% filter(host_count > 1) 
  nrow(mult)
}

what_percent_multilistings <- function(listings){
  mult <- listings %>% filter(room_type == "Entire home/apt") %>% group_by(host_id) %>% mutate(host_count = n()) %>% filter(host_count > 1) 
  entire_apt <- listings %>% filter(room_type == "Entire home/apt") %>% group_by(host_id) %>% mutate(host_count = n()) 
  nrow(mult) / nrow(entire_apt) * 100
}

how_many_listings(listings1509)
how_many_multilistings(listings1509)
what_percent_multilistings(listings1509)

#create a grand function that creates a database of summary statistics of all scrapes
scrape_date <- c()
num_listings <- c()
entire_homes <- c()
num_multi <- c()
percent_multi <- c()

create_summary_dataframe <- function(l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, l15){
  scrape_date <- as.Date(c("2015-01-01", "2015-03-01", "2015-04-01", "2015-05-01", "2015-06-01", 
                           "2015-08-01", "2015-09-01","2015-10-01","2015-11-01","2015-11-20", 
                           "2015-12-02", "2016-01-01","2016-02-02","2016-04-03","2016-05-02"))
  
  entire_homes <- c(how_many_entire(l1), how_many_entire(l2), how_many_entire(l3), how_many_entire(l4), how_many_entire(l5),
                    how_many_entire(l6), how_many_entire(l7), how_many_entire(l8), how_many_entire(l9), how_many_entire(l10),
                    how_many_entire(l11), how_many_entire(l12), how_many_entire(l13), how_many_entire(l14), how_many_entire(l15))
                    
  num_listings <- c(how_many_listings(l1), how_many_listings(l2), how_many_listings(l3), how_many_listings(l4), how_many_listings(l5),
                    how_many_listings(l6), how_many_listings(l7), how_many_listings(l8), how_many_listings(l9), how_many_listings(l10),
                    how_many_listings(l11), how_many_listings(l12), how_many_listings(l13), how_many_listings(l14), how_many_listings(l15))
  
  num_multi <- c(how_many_multilistings(l1), how_many_multilistings(l2), how_many_multilistings(l3), how_many_multilistings(l4), how_many_multilistings(l5),
                 how_many_multilistings(l6), how_many_multilistings(l7), how_many_multilistings(l8), how_many_multilistings(l9), how_many_multilistings(l10),
                 how_many_multilistings(l11), how_many_multilistings(l12), how_many_multilistings(l13), how_many_multilistings(l14), how_many_multilistings(l15))

  percent_multi <- c(what_percent_multilistings(l1), what_percent_multilistings(l2), what_percent_multilistings(l3), what_percent_multilistings(l4), what_percent_multilistings(l5),
                     what_percent_multilistings(l6), what_percent_multilistings(l7), what_percent_multilistings(l8), what_percent_multilistings(l9), what_percent_multilistings(l10),
                     what_percent_multilistings(l11), what_percent_multilistings(l12), what_percent_multilistings(l13), what_percent_multilistings(l14), what_percent_multilistings(l15))
  
  grand_summary <- data.frame(scrape_date, num_listings, entire_homes, num_multi, percent_multi)
}

grand_summary <- create_summary_dataframe(listings1501, listings1503, listings1504, listings1505, listings1506, 
                                          listings1509, listings1510, listings1511, listings151120, listings1512, 
                                          listings1601, listings1602, listings1604, listings1605, listings1606)

View(grand_summary)
  