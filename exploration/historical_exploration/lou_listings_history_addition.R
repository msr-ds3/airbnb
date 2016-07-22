library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(scales)
 
# >> loaded CSVs 2015-16, accounted for missing columns, smushed all dfs together
# >> from Kaciny's code: listings_history.R, line 8 - 81

######## Adding host_listings_count and multilistings T/F Column
# host_listings_count = number of listings the host has 
listings_history <- read_csv("../../raw_data/listing_history.csv")
listings_history <- listings_history %>% group_by(host_id) %>% mutate(host_listings_count = n()) %>% mutate(is_multilisting = (host_listings_count > 1))

######### Adding T/F amenities column
# 1. create giant skinny database of all 2015 scrapes
