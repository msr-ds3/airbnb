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

create_summary_dataframe <- function(l1, l2, l3, l4, l5, l6, l7, l8, l9, l10){
  scrape_date <- c(scrape_date, "2015-09")
  scrape_date <- c(scrape_date, "2015-10")
  scrape_date <- c(scrape_date, "2015-11")
  scrape_date <- c(scrape_date, "2015-11-20")
  scrape_date <- c(scrape_date, "2015-12")
  scrape_date <- c(scrape_date, "2016-01")
  scrape_date <- c(scrape_date, "2016-02")
  scrape_date <- c(scrape_date, "2016-04")
  scrape_date <- c(scrape_date, "2016-05")
  scrape_date <- c(scrape_date, "2016-06")
  
  entire_homes <- c(entire_homes, how_many_entire(l1))
  entire_homes <- c(entire_homes, how_many_entire(l2))
  entire_homes <- c(entire_homes, how_many_entire(l3))
  entire_homes <- c(entire_homes, how_many_entire(l4))
  entire_homes <- c(entire_homes, how_many_entire(l5))
  entire_homes <- c(entire_homes, how_many_entire(l6))
  entire_homes <- c(entire_homes, how_many_entire(l7))
  entire_homes <- c(entire_homes, how_many_entire(l8))
  entire_homes <- c(entire_homes, how_many_entire(l9))
  entire_homes <- c(entire_homes, how_many_entire(l10))
  
  num_listings <- c(num_listings, how_many_listings(l1))
  num_listings <- c(num_listings, how_many_listings(l2))
  num_listings <- c(num_listings, how_many_listings(l3))
  num_listings <- c(num_listings, how_many_listings(l4))
  num_listings <- c(num_listings, how_many_listings(l5))
  num_listings <- c(num_listings, how_many_listings(l6))
  num_listings <- c(num_listings, how_many_listings(l7))
  num_listings <- c(num_listings, how_many_listings(l8))
  num_listings <- c(num_listings, how_many_listings(l9))
  num_listings <- c(num_listings, how_many_listings(l10))
  
  num_multi <- c(num_multi, how_many_multilistings(l1))
  num_multi <- c(num_multi, how_many_multilistings(l2))
  num_multi <- c(num_multi, how_many_multilistings(l3))
  num_multi <- c(num_multi, how_many_multilistings(l4))
  num_multi <- c(num_multi, how_many_multilistings(l5))
  num_multi <- c(num_multi, how_many_multilistings(l6))
  num_multi <- c(num_multi, how_many_multilistings(l7))
  num_multi <- c(num_multi, how_many_multilistings(l8))
  num_multi <- c(num_multi, how_many_multilistings(l9))
  num_multi <- c(num_multi, how_many_multilistings(l10))

  percent_multi <- c(percent_multi, what_percent_multilistings(l1))
  percent_multi <- c(percent_multi, what_percent_multilistings(l2))
  percent_multi <- c(percent_multi, what_percent_multilistings(l3))
  percent_multi <- c(percent_multi, what_percent_multilistings(l4))
  percent_multi <- c(percent_multi, what_percent_multilistings(l5))
  percent_multi <- c(percent_multi, what_percent_multilistings(l6))
  percent_multi <- c(percent_multi, what_percent_multilistings(l7))
  percent_multi <- c(percent_multi, what_percent_multilistings(l8))
  percent_multi <- c(percent_multi, what_percent_multilistings(l9))
  percent_multi <- c(percent_multi, what_percent_multilistings(l10))
  
  grand_summary <- data.frame(scrape_date, num_listings, entire_homes, num_multi, percent_multi)
}

grand_summary <- create_summary_dataframe(listings1509, listings1510, listings1511, listings151120, listings1512, 
                                          listings1601, listings1602, listings1604, listings1605, listings1606)

View(grand_summary)
  