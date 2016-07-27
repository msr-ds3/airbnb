library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(scales)
library(tidyr)

listings1511 <- read_csv("../../raw_data/2015-11-01-listings.csv")
listings151120 <- read_csv("../../raw_data/2015-11-20-listings.csv")
listings1512 <- read_csv("../../raw_data/2015-12-02-listings.csv")
listings1601 <- read_csv("../../raw_data/2016-01-01-listings.csv")
listings1602 <- read_csv("../../raw_data/2016-02-02-listings.csv")
# No data for March 2016 :(
listings1604 <- read_csv("../../raw_data/2016-04-03-listings.csv") #APRIL! This is not March. March has been skipped.
listings1605 <- read_csv("../../raw_data/2016-05-02-listings.csv")
listings1606 <- read_csv("../../raw_data/2016-06-02-listings.csv")
listings1607 <- read_csv("../../raw_data/2016-07-02-listings.csv")

######################################### [ crude True/False was_purged_in_nov column]

# definition of purged: multilisting > 1 && never seen again

# obtain entire home/apt multilistings for Nov 2015
multilistings1511 <- listings1511 %>% filter(room_type == "Entire home/apt") %>% group_by(host_id) %>% mutate(host_listings_count = n()) %>% filter(host_listings_count > 1)
View(multilistings1511)

# smush together all listings post-purge (i.e. post 17 Nov 2015)
column_names <- colnames(listings1607) #get all column names and find names of missing columns

missing <- setdiff(column_names, names(listings151120))
listings151120[missing] <- NA
missing <- setdiff(column_names, names(listings1512)) 
listings1512[missing] <- NA   
missing <- setdiff(column_names, names(listings1601))  
listings1601[missing] <- NA   
missing <- setdiff(column_names, names(listings1602))  
listings1602[missing] <- NA   
missing <- setdiff(column_names, names(listings1604)) 
listings1604[missing] <- NA   
missing <- setdiff(column_names, names(listings1605)) 
listings1605[missing] <- NA   
missing <- setdiff(column_names, names(listings1606)) 
listings1606[missing] <- NA   
missing <- setdiff(column_names, names(listings1607)) 
listings1607[missing] <- NA   

listings_postpurge <- rbind(listings151120, listings1512, listings1601, listings1602, 
                            listings1604, listings1605, listings1606, listings1607) 

# listings that are multilistings and never seen again after the purge (i.e. 17 Nov 2015)
multilistings_gone_forever <- anti_join(multilistings1511, listings_postpurge, by = 'id')
nrow(multilistings_gone_forever) #1324
View(multilistings_gone_forever)
skinny <- select(multilistings_gone_forever, id, host_id, room_type, host_listings_count, price, reviews_per_month,
            minimum_nights, review_scores_rating, requires_license, 
            host_identity_verified, neighbourhood_cleansed, property_type)
skinny <- select(multilistings_gone_forever, id)
View(skinny)

######################################### [ exploration, in hopes of refinement]
# obtain entire home/apt multilistings
multi <- listings1511 %>% filter(room_type == "Entire home/apt") %>% group_by(host_id) %>% mutate(host_listings_count = n()) %>% filter(host_listings_count > 1)
View(multi)

# check that the ones purged are indeed multilistings that are > 3
gone_listings <- anti_join(listings1511, listings151120, by = 'id')
nrow(gone_listings) # 3700
View(gone_listings)

# how many gone_listings are entire home/apt multilistings?
nrow(gone_listings %>% 
       filter(room_type == "Entire home/apt") %>% 
       group_by(host_id) %>% mutate(host_listings_count = n()) %>% 
       filter(host_listings_count > 1)) # 1164 = 31%

# how many gone_listings are entire home/apt multilistings > 2? > 3?
nrow(gone_listings %>% 
       filter(room_type == "Entire home/apt") %>% 
       group_by(host_id) %>% mutate(host_listings_count = n()) %>% 
       filter(host_listings_count > 2)) # 760 = 21%

nrow(gone_listings %>% 
       filter(room_type == "Entire home/apt") %>% 
       group_by(host_id) %>% mutate(host_listings_count = n()) %>% 
       filter(host_listings_count > 3)) # 493 = 13%

# show entire home/apt multilistings in 1511 and gone_listings
View(select(multi, id, host_id, name, room_type, host_listings_count)) #skinny
View(select(multi, id, host_id, name, room_type, host_listings_count)) #skinny

##############################################


