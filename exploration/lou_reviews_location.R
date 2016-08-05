reviews <- read_csv("../../raw_data/us_rev_data.csv")
colnames(reviews)
xView(reviews_skinny_test)

reviews_skinny_test <- reviews_skinny[1:1000,] #test on 1,000 rows


# 1. create giant skinny database of all 2015-2016 scrapes

reviews_skinny <- reviews[,c(1,14)] %>% filter(is.na(locations_in_2015) == FALSE) #select only reviewer_id and locations, filter for NAs
sum(is.na(reviews_skinny$locations_in_2015)) #0. There should be 0 NAs
write.table(reviews_skinny_test, "locations_and_reviewer_grand.tsv", sep='\t')


# 2. create a list of all unique locations and export it to tsv

# returns a list of unique locations, bound by scrape date. #todo: Granularity can be adjusted to be month or day.
find_unique_locations <- function(skinny_df){ 
  total_locations <- c()
  #begin to obtain unique locations for the specified subset of data
    for(i in 1:nrow(skinny_df)){
      #print(sprintf("============ %f", i)) # this was a checker
      one_reviewers_locations <- strsplit(as.character(skinny_df[i,"locations_in_2015"]),"[|]")[[1]] #split by delimiter (|)
    if(length(one_reviewers_locations) != 0){ #only continue if locations are not empty for that listing
        for(n in 1:length(one_reviewers_locations)){
          location = one_reviewers_locations[[n]]
          if(!location %in% total_locations){
            total_locations = c(total_locations, location)
        }
      }
    }
    
  }
  total_locations
}

total_locations = sort(find_unique_locations(reviews_skinny)) # unique location of all reviewers, sorted alphabetically

write.table(total_locations, 'unique_locations_grand.tsv', sep='\t') #list of unique locations for all 2015-16

cleaned_total_location <- total_locations %>% tolower() %>% unique()
length(total_locations) #10651
length(cleaned_total_location) #10351 (-300)

# 3. After running through python script, returns T/F table of locations
# python lou_reviewer_locations_python.py > table_locations_grand.csv
locations_table_grand = read.csv('table_locations_grand.csv')

locations_table_grand <- locations_table_grand 
View(locations_table_grand) #takes the last instance of the listing locations

colnames(all_listings)

summary(all_listings$number_of_reviews)
View(all_listings)

summary(listings_history$num_reviews_in_2015)
summary(listings_history$num_reviews_in_2016)
