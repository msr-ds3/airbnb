library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(scales)

# >> loaded CSVs 2015-16, accounted for missing columns, smushed all dfs together. Results in all_listings
# >> from Kaciny's code: listings_history.R, line 8 - 81

######### Adding T/F verifications column
skinny_listings_v <- select(all_listings, id, host_verifications, last_scraped) %>% filter(is.na(host_verifications) == FALSE) #filter for NAs. 
View(skinny_listings_v) 
write.table(skinny_listings_v, "../verifications_and_id_grand.tsv", sep='\t')
sum(is.na(skinny_listings_v$verifications)) #0. There should be 0 NAs
nrow(skinny_listings_v) # 331,727 (check)

total_verifications = c()
# returns a list of unique verification types, bound by scrape date. #todo: Granularity can be adjusted to be month or day.
find_unique_verifications <- function(skinny_df, lower_date="2015-01-01", upper_date="2016-07-01"){ #default boundaries include the whole dataset
  lower_date = as.Date(lower_date,format="%Y-%m-%d")
  upper_date = as.Date(upper_date,format="%Y-%m-%d")
  total_verifications <- c()
  if(lower_date > upper_date){ #checker
    print("Lower date must be smaller than upper date")
  } else {
    skinny_df <- mutate(skinny_df, last_scraped = as.Date(last_scraped,format="%Y-%m-%d"))# modify to ensure date comparison will work
    sliced_df <- subset(skinny_df, (last_scraped >= lower_date) & (last_scraped <= upper_date), select=c(id, host_verifications, last_scraped)) #subsetting
    
    #begin to obtain unique verifications for the specified subset of data
    for(i in 1:nrow(sliced_df)){
      #print(sprintf("============ %f", i)) # this was a checker
      one_listings_verifications <- strsplit(gsub("\\[|\\]|\'|\\s", "", sliced_df[i, "host_verifications"]), "[,]")[[1]] 
      #replaces [ ] ' with nothing >> \\[ OR \\] OR \\' OR \\s
      #verifications for listing number #listnum
      
      if(length(one_listings_verifications) != 0){ #only continue if verifications are not empty for that listing
        for(n in 1:length(one_listings_verifications)){
          verification = one_listings_verifications[[n]]
          if(!verification %in% total_verifications){
            total_verifications = c(total_verifications, verification)
          }
        }
      }
    }
    
  }
  total_verifications
}
total_verifications = find_unique_verifications(skinny_listings_v) #remember, can include lower and upper date boundaries 
View(total_verifications) #15 unique ways of verification!
write.table(total_verifications, '../unique_verifications_grand.tsv', sep='\t') #list of unique verifications for all 2015-16

# 3. After running through python script, returns T/F table of verifications
# (in exploration folder) $python lou_verifications_python.py > table_amenities_grand.csv
verifications_table_grand = read.csv('../table_verifications_grand.csv')
View(verifications_table_grand)

verifications_table_grand <- verifications_table_grand %>% group_by(id) %>% arrange(id) %>% filter(row_number() == n())
View(verifications_table_grand) #take the last instance of verifications
write.table(verifications_table_grand, '../table_verifications_grand_final.csv')
