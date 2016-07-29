#create skimmed listings history file
#this has only one listing per host randomized

library(readr)
library(dplyr)

#load the listings history database

listings_history <- read_csv("../raw_data/listing_history.csv")

skimmed_listings_history <- listings_history %>% group_by(host_id.x) %>% 
  filter(row_number() == sample(1:row_number(), 1))

save(skimmed_listings_history, file = "skimmed_listings_history.RData")
