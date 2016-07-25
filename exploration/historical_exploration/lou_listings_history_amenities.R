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
View(listings_history)

######### Adding T/F amenities column
# 1. create giant skinny database of all 2015-2016 scrapes
skinny_listings <- select(all_listings, id, amenities, last_scraped) %>% filter(is.na(amenities) == FALSE) #filter for NAs. 
View(skinny_listings) 
write.table(skinny_listings, "../amenities_and_id_grand.tsv", sep='\t')
sum(is.na(skinny_listings$amenities)) #0. There should be 0 NAs
nrow(skinny_listings) # 331,727 (check)

# 2. create a list of all unique amenities and export it to tsv
total_amenities = c()

  # returns a list of unique amenities, bound by scrape date. #todo: Granularity can be adjusted to be month or day.
find_unique_amenities <- function(skinny_df, lower_date="2015-01-01", upper_date="2016-07-01"){ #default boundaries include the whole dataset
  lower_date = as.Date(lower_date,format="%Y-%m-%d")
  upper_date = as.Date(upper_date,format="%Y-%m-%d")
  total_amenities <- c()
  if(lower_date > upper_date){ #checker
    print("Lower date must be smaller than upper date")
  } else {
    skinny_df <- mutate(skinny_df, last_scraped = as.Date(last_scraped,format="%Y-%m-%d"))# modify to ensure date comparison will work
    sliced_df <- subset(skinny_df, (last_scraped >= lower_date) & (last_scraped <= upper_date), select=c(id, amenities, last_scraped)) #subsetting
    
    #begin to obtain unique amenities for the specified subset of data
    for(i in 1:nrow(sliced_df)){
      #print(sprintf("============ %f", i)) # this was a checker
      one_listings_amenities <-strsplit(gsub("[{}\"]", "", sliced_df[i, "amenities"]), "[,]")[[1]] #amenities for listing number #listnum
      
      if(length(one_listings_amenities) != 0){ #only continue if amenities are not empty for that listing
        for(n in 1:length(one_listings_amenities)){
          amenity = one_listings_amenities[[n]]
          if(!amenity %in% total_amenities){
            total_amenities = c(total_amenities, amenity)
          }
        }
      }
    }
    
  }
  total_amenities 
}
total_amenities = find_unique_amenities(skinny_listings) #remember, can include lower and upper boundaries 
write.table(total_amenities, '../unique_amenities_grand.tsv', sep='\t') #list of unique amenities for all 2015-16
  
# 3. After running through python script, returns T/F table of amenities
# python lou_amenities_python.py > table_amenities_grand.csv
amenities_table_grand = read.csv('../table_amenities_grand.csv')
View(amenities_table_grand)
