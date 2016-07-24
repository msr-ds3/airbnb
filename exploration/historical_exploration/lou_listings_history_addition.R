library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(scales)
 
# >> loaded CSVs 2015-16, accounted for missing columns, smushed all dfs together. Results in all_listings
# >> from Kaciny's code: listings_history.R, line 8 - 81

######## Adding host_listings_count and multilistings T/F Column
# host_listings_count = number of listings the host has 
listings_history <- read_csv("../../raw_data/listing_history.csv")
listings_history <- listings_history %>% group_by(host_id) %>% mutate(host_listings_count = n()) %>% mutate(is_multilisting = (host_listings_count > 1))

######### Adding T/F amenities column
# 1. create giant skinny database of all 2015-2016 scrapes
skinny_listings <- select(all_listings, id, amenities) %>% filter(is.na(amenities) == FALSE) #filter for NAs
View(skinny_listings)
write.table(skinny_listings, "../amenities_and_id_grand.tsv", sep='\t')
sum(is.na(skinny_listings$amenities)) #0. There should be 0 NAs
nrow(skinny_listings) # 331,727

# 2. create a list of all unique amenities and export it to tsv
total_amenities = c()

for(i in 1:nrow(skinny_listings)){
  print(sprintf("============ %f", i))
  one_listings_amenities <-strsplit(gsub("[{}\"]", "", skinny_listings[i, "amenities"]), "[,]")[[1]] #amenities for listing number #listnum
  
  if(length(one_listings_amenities) != 0){ #only continue if amenities are not empty for that listing
    for(n in 1:length(one_listings_amenities)){
      #print(n)
      #print(one_listings_amenities[[n]])
      amenity = one_listings_amenities[[n]]
      if(!amenity %in% total_amenities){
        total_amenities = c(total_amenities, amenity)
      }
    }
  }
}

total_amenities
write.table(total_amenities, '../unique_amenities_grand.tsv', sep='\t') #list of unique amenities for all 2015-16

# 3. 
amenities_table_grand = read.csv('table_amenities_grand.csv')
View(amenities_table_grand)
