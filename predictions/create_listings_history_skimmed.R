#create skimmed listings history file
#this has only one listing per host randomized

library(readr)
library(dplyr)

#load the listings history database

joined_models <- read_csv("../raw_data/joined_models.csv")

skimmed_listings_history <- joined_models %>% filter(purged == FALSE)


skimmed_listings_history <- skimmed_listings_history %>% group_by(host_id.x) %>% 
  filter(row_number() == sample(1:row_number(), 1))


View(skimmed_listings_history)

save(skimmed_listings_history, file = "skimmed_listings_history.RData")
