# This file contains the function that returns how many listings left airbnb, per scrape date

library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(scales)
library(tidyr)

# >> insert loading CSVs <<

#============================== functions ========================

# Returns how many <typeof/all> listings are in this scrape date, but not in the next scrape date
how_many_listings_gone <- function(this_month, next_month, type, multi=F){
  if(type == "all"){ 
    gone_listings <- anti_join(this_month, next_month, by = 'id')
    nrow(gone_listings)
  } else if (multi == TRUE){
    this_month <- this_month %>% filter(room_type == type) %>% group_by(host_id) %>% mutate(host_count = n()) %>% filter(host_count > 1) 
    next_month <- next_month %>% filter(room_type == type) %>% group_by(host_id) %>% mutate(host_count = n()) %>% filter(host_count > 1)
    gone_listings <- anti_join(this_month, next_month, by = 'id')
    nrow(gone_listings)
  } else if (type == "Entire home/apt" || type == "Private room" || type == "Shared room"){
    gone_listings <- anti_join(filter(this_month, room_type==type), filter(next_month, room_type==type) , by = 'id')
    nrow(gone_listings)
  } else {
    print("¯\\_(ツ)_/¯")
  }
}

# Returns how many <typeof/all> listings are in this scrape date, but not in the next scrape date
how_many_listings_added <- function(this_month, next_month, type, multi=F){
  if(type == "all"){ 
    gone_listings <- anti_join(next_month, this_month, by = 'id')
    nrow(gone_listings)
  } else if (multi == TRUE){
    this_month <- this_month %>% filter(room_type == type) %>% group_by(host_id) %>% mutate(host_count = n()) %>% filter(host_count > 1) 
    next_month <- next_month %>% filter(room_type == type) %>% group_by(host_id) %>% mutate(host_count = n()) %>% filter(host_count > 1)
    gone_listings <- anti_join(next_month, this_month, by = 'id')
    nrow(gone_listings)
  } else if (type == "Entire home/apt" || type == "Private room" || type == "Shared room"){
    gone_listings <- anti_join(filter(next_month, room_type==type), filter(this_month, room_type==type) , by = 'id')
    nrow(gone_listings)
  } else {
    print('¯\\_(ツ)_/¯')
  }
}

how_many_listings_added(listings1509, listings1510, "all")

# Returns how many <typeof/all> homes are listed at the time of the scrape date
how_many_listings <- function(listings, type, multi=F){
  if(type == "all"){ 
    nrow(listings)
  } else if (multi == TRUE){
    entire_homes <- listings %>% filter(room_type == type) %>% group_by(host_id) %>% mutate(host_count = n()) %>% filter(host_count > 1)
    nrow(entire_homes)
  } else {
    entire_homes <- listings %>% filter(room_type == type)
    nrow(entire_homes)
  }
}

how_many_listings(listings1509, "Entire home/apt") #17026 (check)
how_many_listings_gone(listings1509, listings1510, "Entire home/apt", TRUE) #408

#========================= database creation ======================
#create_left_database: create database of how many airbnbs are gone for that scrape date
listings_gone <- c()
scrape_date <- c()
difference <- c()
listings_gone_percent <- c()
create_database <- function(l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, l15, type, multi=F){
  scrape_date <- as.Date(c("2015-01-01", "2015-03-01", "2015-04-01", "2015-05-01", "2015-06-01", 
                           "2015-08-01", "2015-09-01","2015-10-01","2015-11-01","2015-11-20", 
                           "2015-12-02", "2016-01-01","2016-02-02","2016-04-03","2016-05-02"))
  
  num_listings <- c(how_many_listings(l1, type, multi),
                    how_many_listings(l2, type, multi),
                    how_many_listings(l3, type, multi),
                    how_many_listings(l4, type, multi),
                    how_many_listings(l5, type, multi),
                    how_many_listings(l6, type, multi),
                    how_many_listings(l7, type, multi),
                    how_many_listings(l8, type, multi),
                    how_many_listings(l9, type, multi),
                    how_many_listings(l10, type, multi),
                    how_many_listings(l11, type, multi),
                    how_many_listings(l12, type, multi),
                    how_many_listings(l13, type, multi),
                    how_many_listings(l14, type, multi),
                    how_many_listings(l15, type, multi))
             
  num_listings_gone <- c(how_many_listings_gone(l1, l2, type, multi), 
                     how_many_listings_gone(l2, l3, type, multi), 
                     how_many_listings_gone(l3, l4, type, multi), 
                     how_many_listings_gone(l4, l5, type, multi), 
                     how_many_listings_gone(l5, l6, type, multi), 
                     how_many_listings_gone(l6, l7, type, multi), 
                     how_many_listings_gone(l7, l8, type, multi), 
                     how_many_listings_gone(l8, l9, type, multi), 
                     how_many_listings_gone(l9, l10, type, multi),
                     how_many_listings_gone(l10, l11, type, multi),
                     how_many_listings_gone(l11, l12, type, multi), 
                     how_many_listings_gone(l12, l13, type, multi), 
                     how_many_listings_gone(l13, l14, type, multi), 
                     how_many_listings_gone(l14, l15, type, multi),
                     0)
  
  num_listings_added <- c(how_many_listings_added(l1, l2, type, multi), 
                          how_many_listings_added(l2, l3, type, multi), 
                          how_many_listings_added(l3, l4, type, multi), 
                          how_many_listings_added(l4, l5, type, multi), 
                          how_many_listings_added(l5, l6, type, multi), 
                          how_many_listings_added(l6, l7, type, multi), 
                          how_many_listings_added(l7, l8, type, multi), 
                          how_many_listings_added(l8, l9, type, multi), 
                          how_many_listings_added(l9, l10, type, multi),
                          how_many_listings_added(l10, l11, type, multi),
                          how_many_listings_added(l11, l12, type, multi), 
                          how_many_listings_added(l12, l13, type, multi), 
                          how_many_listings_added(l13, l14, type, multi), 
                          how_many_listings_added(l14, l15, type, multi), 
                          0)
  
  for (i in 1:14){
    listings_gone_percent <- c(listings_gone_percent, ((num_listings_gone[i] / num_listings[i]) * 100))
    difference <- c(difference, (num_listings_added[i] - num_listings_gone[i])) 
  }
 
  listings_gone_percent <- c(listings_gone_percent, 0) #needed to make rows = 10
  difference <- c(difference, 0)
  
  final_database <- data.frame(scrape_date, num_listings, num_listings_added,
                               num_listings_gone, difference, listings_gone_percent)
}


######################### [creating databases]
#all
df_all <- create_database(listings1501, listings1503, listings1504, listings1505, listings1506, 
                          listings1509, listings1510, listings1511, listings151120, listings1512, 
                          listings1601, listings1602, listings1604, listings1605, listings1606,
                          "all")

# Entire home/apt
df_entire <- create_database(listings1501, listings1503, listings1504, listings1505, listings1506, 
                             listings1509, listings1510, listings1511, listings151120, listings1512, 
                             listings1601, listings1602, listings1604, listings1605, listings1606,
                                                 "Entire home/apt")
# Private room
df_private <- create_database(listings1501, listings1503, listings1504, listings1505, listings1506, 
                              listings1509, listings1510, listings1511, listings151120, listings1512, 
                              listings1601, listings1602, listings1604, listings1605, listings1606, 
                                                 "Private room")
# Shared Room
df_shared <- create_database(listings1501, listings1503, listings1504, listings1505, listings1506, 
                             listings1509, listings1510, listings1511, listings151120, listings1512, 
                             listings1601, listings1602, listings1604, listings1605, listings1606, 
                                                 "Shared room")
##################### MULTI-LISTINGS

# Entire home/apt, multilistings = TRUE
df_entire_multi <- create_database(listings1501, listings1503, listings1504, listings1505, listings1506, 
                                                 listings1509, listings1510, listings1511, listings151120, listings1512, 
                                                 listings1601, listings1602, listings1604, listings1605, listings1606,
                                                 "Entire home/apt", TRUE)

(nrow(listings1511 %>% group_by(host_id) %>% mutate(host_count = n()) %>% filter(host_count > 1) )/nrow(listings1511)) * 100
(nrow(listings1605 %>% group_by(host_id) %>% mutate(host_count = n()) %>% filter(host_count > 1) )/nrow(listings1605)) * 100
View(listings_gone_df_entire_multi)

# Private room, multilistings = TRUE
df_private_multi <- create_database(listings1501, listings1503, listings1504, listings1505, listings1506, 
                                    listings1509, listings1510, listings1511, listings151120, listings1512, 
                                    listings1601, listings1602, listings1604, listings1605, listings1606, 
                                                 "Private room", TRUE)
# Shared room, multilistings = TRUE
df_shared_multi <- create_database(listings1501, listings1503, listings1504, listings1505, listings1506, 
                                    listings1509, listings1510, listings1511, listings151120, listings1512, 
                                    listings1601, listings1602, listings1604, listings1605, listings1606, 
                                    "Shared room", TRUE)


######################### [graphing]
#### plots number of <typeof> listing present on Airbnb
  # creating dataframe
scrape_date <- as.Date(c("2015-01-01", "2015-03-01", "2015-04-01", "2015-05-01", "2015-06-01", 
                         "2015-08-01", "2015-09-01","2015-10-01","2015-11-01","2015-11-20", 
                         "2015-12-02", "2016-01-01","2016-02-02","2016-04-03","2016-05-02"))

number_of_listings_df <- data.frame(scrape_date, 
                                    entire_home=df_entire$num_listings, 
                                    private_room=df_private$num_listings, 
                                    shared_room=df_shared$num_listings)

number_of_listings_df_long <- gather(number_of_listings_df, listing_type, number_of_listings, -scrape_date)


  # creating plot
ggplot(aes(scrape_date, number_of_listings, colour=listing_type), data=number_of_listings_df_long) +
  geom_line() + geom_point() +
  scale_x_date(breaks=date_breaks("months"), labels=date_format("%b")) + 
  xlab("Scrape Date") + ylab("Number of Listings") + ggtitle("Number of Listings on Airbnb")


################################################## [2 TRASH]
# plots number of 'all' and 'entire home && multi-listing' listings present on Airbnb
# creating dataframe
number_of_listings_df2 <- data.frame(scrape_date, 
                                    all_listings=df_all$num_listings, 
                                    entire_home_and_multilisting=df_entire_multi$num_listings)

number_of_listings_df_long2 <- gather(number_of_listings_df2, listing_type, number_of_listings, -scrape_date)

# creating plot
ggplot(aes(scrape_date, number_of_listings, colour=listing_type), data=number_of_listings_df_long2) +
  geom_line() + geom_point() +
  scale_x_date(breaks=date_breaks("months"), labels=date_format("%b")) + 
  xlab("Scrape Date") + ylab("Number of Listings") + 
  ggtitle("Number of Listings on Airbnb\nAll Listings vs Entire Home/Apt Multilistings")

################################################## [3]
# plots number of multilistings present on Airbnb
# creating dataframe
number_of_listings_df3 <- data.frame(scrape_date, 
                                     entire_home_and_multilisting=df_entire_multi$num_listings,
                                     private_room_and_multilisting=df_private_multi$num_listings, 
                                     shared_room_and_multilisting=df_shared_multi$num_listings
                                     )

number_of_listings_df_long3 <- gather(number_of_listings_df3, listing_type, number_of_listings, -scrape_date)

# creating plot
ggplot(aes(scrape_date, number_of_listings, colour=listing_type), data=number_of_listings_df_long3) +
  geom_line() + geom_point() + 
  scale_x_date(breaks=date_breaks("months"), labels=date_format("%b")) + 
  xlab("Scrape Date") + ylab("Number of Listings") + 
  ggtitle("Number of Multi-listings on Airbnb\nBroken Down By Type") 
  
################################################## [4]
# plots number of multilistings present on Airbnb
# creating dataframe
number_of_listings_df4 <- data.frame(scrape_date, 
                                     entire_home_and_multilisting=df_entire_multi$num_listings)
                                   

number_of_listings_df_long4 <- gather(number_of_listings_df4, listing_type, number_of_listings, -scrape_date)
View(number_of_listings_)
# creating plot
ggplot(aes(scrape_date, number_of_listings, colour=listing_type), data=number_of_listings_df_long4) +
  geom_line() + geom_point() + 
  scale_x_date(breaks=date_breaks("months"), labels=date_format("%b")) + 
  xlab("Scrape Date") + ylab("Number of Listings") + 
  ggtitle("Number of Multi-listings on Airbnb\nEntire Home/Apt") 









######################################## [rubbish]

# plots percent of <typeof> listings that have left Airbnb on that scrape date
ggplot() +
  geom_line(aes(scrape_date, df_entire$listings_gone), group=1, colour = "black") + 
  geom_point(aes(scrape_date, df_entire$listings_gone_percent), colour = "black") +
  geom_line(aes(scrape_date, df_private$listings_gone_percent), group=1, colour = "red") + 
  geom_point(aes(scrape_date, df_private$listings_gone_percent), colour = "red") +
  geom_line(aes(scrape_date, df_shared$listings_gone_percent), group=1, colour = "green") + 
  geom_point(aes(scrape_date, df_shared$listings_gone_percent), colour = "green") +
  xlab("Scrape Date") + ylab("% Listings Gone from Airbnb")

# plots number of <typeof> listing that left Airbnb
ggplot() +
  geom_line(aes(scrape_date, df_entire$num_listings_gone), group=1, colour = "black") + 
  geom_point(aes(scrape_date, df_entire$num_listings_gone), colour = "black") +
  geom_line(aes(scrape_date, df_private$num_listings_gone), group=1, colour = "red") + 
  geom_point(aes(scrape_date, df_private$num_listings_gone), colour = "red") +
  geom_line(aes(scrape_date, df_shared$num_listings_gone), group=1, colour = "green") + 
  geom_point(aes(scrape_date, df_shared$num_listings_gone), colour = "green") +
  xlab("Scrape Date") + ylab("Number of Listings") + ggtitle("Number of Listings that Left Airbnb,\n Broken Down By Type")

# plots number of <typeof> listing that left Airbnb
ggplot() +
  geom_line(aes(scrape_date, df_entire$num_listings_gone), group=1, colour = "Entire_home") + 
  geom_point(aes(scrape_date, df_entire$num_listings_gone), colour = "Entire_home") +
  geom_line(aes(scrape_date, df_private$num_listings_gone), group=1, colour = "Private_room") + 
  geom_point(aes(scrape_date, df_private$num_listings_gone), colour = "Private_room") +
  geom_line(aes(scrape_date, df_shared$num_listings_gone), group=1, colour = "Shared_room") + 
  geom_point(aes(scrape_date, df_shared$num_listings_gone), colour = "Shared_room") +
  xlab("Scrape Date") + ylab("Number of Listings") + ggtitle("Number of Listings that Left Airbnb,\n Broken Down By Type") +
  scale_color_manual(name="Line Color", 
                     values=c(Entire_home="red", Private_room="blue", Shared_room="green"))
  
#proves that the second 'purge' is not a purge
  

# create a df that has the
