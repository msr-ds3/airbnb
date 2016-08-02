library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(scales)

# >> ran line 8 - 94 in listings_history.R to generate all_listings_2015.R <<

# grab most recent listing observation
listings_with_host_effort <- all_listings_2015 %>% group_by(id) %>% arrange(id) %>% filter(row_number() == n())

# relevant columns
listings_with_host_effort <- select(listings_with_host_effort, 
                                    id, last_scraped, 
                                    host_response_rate, host_acceptance_rate, host_has_profile_pic, host_identity_verified)

# convert percentages into integers
listings_with_host_effort$host_response_rate = gsub("%", "", listings_with_host_effort$host_response_rate)
listings_with_host_effort$host_acceptance_rate = gsub("%", "", listings_with_host_effort$host_acceptance_rate)

# convert strings into booleans
listings_with_host_effort$host_has_profile_pic = ifelse(listings_with_host_effort$host_has_profile_pic=="t",TRUE,FALSE)
listings_with_host_effort$host_identity_verified = ifelse(listings_with_host_effort$host_identity_verified=="t",TRUE,FALSE)

# convert fake 'N/A' to real NA
#listings_with_host_effort$host_response_rate = ifelse(listings_with_host_effort$host_response_rate=="NA",NA,)


##########################################3 [ exploring / summaries ]
# profile_pic / identity_verified begins in August only. Before that all (real) N/A
# some scraped on 12/02 have N/As for all rows (only 10 of them)

# response / acceptance beings in March, but is some filled with string N/As. Strange.

# host_response_rate 
summary(listings_with_host_effort$host_response_rate)
  # N/A for Jan 2015 scrape
  # what do physical string 'N/A's mean? Is it that it's only listed once, then they leave?

# host_acceptance_rate
summary(listings_with_host_effort$host_acceptance_rate)

# host_has_profile_pic
summary(listings_with_host_effort$host_has_profile_pic)
  # Mode   FALSE    TRUE    NA's 
  # logical     144   48444   16328 

# host_identity_verified
summary(listings_with_host_effort$host_identity_verified)
  # Mode   FALSE    TRUE    NA's 
  # logical   16656   31932   16328 

View(listings_with_host_effort)
write_csv(listings_with_host_effort, "../../raw_data/host_effort.csv")
