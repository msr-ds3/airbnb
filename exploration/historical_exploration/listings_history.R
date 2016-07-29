library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(scales)


listings1501 <- read_csv("../../raw_data/2015-01-01-listings.csv")
#no date for february
listings1503 <- read_csv("../../raw_data/2015-03-01-listings.csv")
listings1504 <- read_csv("../../raw_data/2015-04-01-listings.csv")
listings1505 <- read_csv("../../raw_data/2015-05-01-listings.csv")
listings1506 <- read_csv("../../raw_data/2015-06-01-listings.csv")
#no date for july
listings1508 <- read_csv("../../raw_data/2015-08-01-listings.csv")
listings1509 <- read_csv("../../raw_data/2015-09-01-listings.csv")
listings1510 <- read_csv("../../raw_data/2015-10-01-listings.csv")
listings1511 <- read_csv("../../raw_data/2015-11-01-listings.csv")
listings151120 <- read_csv("../../raw_data/2015-11-20-listings.csv") #watch out for this one! This is still November
listings1512 <- read_csv("../../raw_data/2015-12-02-listings.csv")


listings1601 <- read_csv("../../raw_data/2016-01-01-listings.csv")
listings1602 <- read_csv("../../raw_data/2016-02-02-listings.csv")
# No data for March 2016 :(
listings1604 <- read_csv("../../raw_data/2016-04-03-listings.csv") #APRIL! This is not March. March has been skipped.
listings1605 <- read_csv("../../raw_data/2016-05-02-listings.csv")
listings1606 <- read_csv("../../raw_data/2016-06-02-listings.csv")
listings1607 <- read_csv("../../raw_data/2016-07-02-listings.csv")



###rbind data frames####

#get all column names
column_names <- colnames(listings1606)
missing <- setdiff(column_names, names(listings1501))  # Find names of missing columns
listings1501[missing] <- NA   


column_names <- colnames(listings1501)
missing <- setdiff(column_names, names(listings1503))  # Find names of missing columns
listings1503[missing] <- NA   

missing <- setdiff(column_names, names(listings1504))  # Find names of missing columns
listings1504[missing] <- NA   

missing <- setdiff(column_names, names(listings1505))  # Find names of missing columns
listings1505[missing] <- NA   

missing <- setdiff(column_names, names(listings1506))  # Find names of missing columns
listings1506[missing] <- NA   

missing <- setdiff(column_names, names(listings1508))  # Find names of missing columns
listings1508[missing] <- NA   

missing <- setdiff(column_names, names(listings1509))  # Find names of missing columns
listings1509[missing] <- NA   

missing <- setdiff(column_names, names(listings1510))  # Find names of missing columns
listings1510[missing] <- NA   

missing <- setdiff(column_names, names(listings1511))  # Find names of missing columns
listings1511[missing] <- NA

missing <- setdiff(column_names, names(listings1512))  # Find names of missing columns
listings1512[missing] <- NA   

missing <- setdiff(column_names, names(listings1601))  # Find names of missing columns
listings1601[missing] <- NA   

missing <- setdiff(column_names, names(listings1602))  # Find names of missing columns
listings1602[missing] <- NA   

missing <- setdiff(column_names, names(listings1604))  # Find names of missing columns
listings1604[missing] <- NA   

missing <- setdiff(column_names, names(listings1605))  # Find names of missing columns
listings1605[missing] <- NA   

missing <- setdiff(column_names, names(listings1606))  # Find names of missing columns
listings1606[missing] <- NA   

missing <- setdiff(column_names, names(listings1607))  # Find names of missing columns
listings1607[missing] <- NA   

rm(column_names, missing)

all_listings <- rbind(listings1501, listings1503, listings1504, listings1505, listings1506, 
                      listings1508, listings1509, listings1510, listings1511, listings1512,
                      listings1601, listings1602, listings1604, listings1605, listings1606, listings1607 ) 
#run this for 2015#

#run this for 2016#
all_listings_2016 <- rbind(listings1601, listings1602, listings1604, listings1605, listings1606, listings1607) 

rm(listings1501, listings1503, listings1504, listings1505, listings1506, 
   listings1508, listings1509, listings1510, listings1511, listings151120, listings1512,
   listings1601, listings1602, listings1604, listings1605, listings1606, listings1607)

load("listings_history.RData")

# change price fields to numeric
all_listings <- mutate(all_listings, price = as.numeric(gsub("[$,]", "", price)), 
                   weekly_price = as.numeric(gsub("[$,]", "", weekly_price)),
                   monthly_price = as.numeric(gsub("[$,]", "", monthly_price)),
                   security_deposit = as.numeric(gsub("[$,]", "", security_deposit)),
                   cleaning_fee = as.numeric(gsub("[$,]", "", cleaning_fee)),
                   extra_people = as.numeric(gsub("[$,]", "", extra_people)))

all_listings_2015 <- mutate(all_listings_2015, price = as.numeric(gsub("[$,]", "", price)), 
                       weekly_price = as.numeric(gsub("[$,]", "", weekly_price)),
                       monthly_price = as.numeric(gsub("[$,]", "", monthly_price)),
                       security_deposit = as.numeric(gsub("[$,]", "", security_deposit)),
                       cleaning_fee = as.numeric(gsub("[$,]", "", cleaning_fee)),
                       extra_people = as.numeric(gsub("[$,]", "", extra_people)))

all_listings_2016 <- mutate(all_listings_2016, price = as.numeric(gsub("[$,]", "", price)), 
                       weekly_price = as.numeric(gsub("[$,]", "", weekly_price)),
                       monthly_price = as.numeric(gsub("[$,]", "", monthly_price)),
                       security_deposit = as.numeric(gsub("[$,]", "", security_deposit)),
                       cleaning_fee = as.numeric(gsub("[$,]", "", cleaning_fee)),
                       extra_people = as.numeric(gsub("[$,]", "", extra_people)))

names(all_listings)[names(all_listings) == 'id'] <- 'listing_id'
names(all_listings_2015)[names(all_listings_2015) == 'id'] <- 'listing_id'
names(all_listings_2016)[names(all_listings_2016) == 'id'] <- 'listing_id'

save(all_listings, all_listings_2015, all_listings_2016, file = "listings_history.RData")


last_seen <- aggregate(last_scraped ~ listing_id, all_listings_2015, max)
first_seen <- aggregate(last_scraped ~ listing_id, all_listings_2015, min)

last_seen_review <- all_listings %>% filter(!is.na(last_review)) %>% 
  group_by(listing_id) %>% arrange(last_scraped) %>% 
  filter(last_review < "2016-01-01") %>% 
  summarize(last_review= first(last_review)) 

first_seen_review <- all_listings %>% filter(!is.na(first_review)) %>% 
  group_by(listing_id) %>% arrange(last_scraped) %>% 
  filter(first_review < "2016-01-01") %>% 
  summarize(first_review= first(first_review)) 


#last_seen_review <- aggregate(last_review ~ listing_id, all_listings_2015, max)
#first_seen_review <- aggregate(first_review ~ listing_id, all_listings_2015, min)

mean_price <- aggregate(price ~ listing_id, all_listings_2015, mean)
min_price <- aggregate(price ~ listing_id, all_listings_2015, min)
max_price <- aggregate(price ~ listing_id, all_listings_2015, max)
total_occ_2015 <- all_listings_2015 %>% count(listing_id)

names(last_seen)[names(last_seen) == 'last_scraped'] <- 'last_seen'
names(first_seen)[names(first_seen) == 'last_scraped'] <- 'first_seen'
names(mean_price)[names(mean_price) == 'price'] <- 'mean_price'
names(min_price)[names(min_price) == 'price'] <- 'min_price'
names(max_price)[names(max_price) == 'price'] <- 'max_price'
names(total_occ_2015)[names(total_occ_2015) == 'n'] <- 'total_occ_2015'



listings_history <- full_join(first_seen, last_seen, by="listing_id")
listings_history <- mutate(listings_history, first_seen_month = month(first_seen), 
                           last_seen_month = month(last_seen))

listings_history <- left_join(listings_history, total_occ_2015, by="listing_id")
listings_history <- left_join(listings_history, first_seen_review, by="listing_id")
listings_history <- left_join(listings_history, last_seen_review, by="listing_id")
listings_history <- left_join(listings_history, mean_price, by="listing_id")
listings_history <- left_join(listings_history, min_price, by="listing_id")
listings_history <- left_join(listings_history, max_price, by="listing_id")

rm(first_seen, last_seen, total_occ_2015, first_seen_review, last_seen_review, mean_price, min_price, max_price)

all_listings_2015_temp <- full_join(all_listings_2015, listings_history, by= "listing_id")

last_record <- filter(all_listings_2015_temp, last_scraped == last_seen)
last_record <- mutate(last_record, has_reviews_2015 = (number_of_reviews > 0))


last_rating <- distinct(select(last_record, host_id, listing_id, host_since, review_scores_rating, 
                               number_of_reviews, has_reviews_2015, host_is_superhost, property_type, room_type))
names(last_rating)[names(last_rating) == 'number_of_reviews'] <- 'number_of_reviews_last'
names(last_rating)[names(last_rating) == 'review_scores_rating'] <- 'last_rating'
names(last_rating)[names(last_rating) == 'host_is_superhost'] <- 'is_superhost_2015'

first_record <- filter(all_listings_2015_temp, last_scraped == first_seen)
first_rating <- distinct(select(first_record, listing_id, number_of_reviews))
names(first_rating)[names(first_rating) == 'number_of_reviews'] <- 'number_of_reviews_first'

last_rating <- full_join(first_rating, last_rating, by= "listing_id") %>% 
  mutate(num_reviews_2015 = (number_of_reviews_last - number_of_reviews_first))

listings_history <- full_join(listings_history, last_rating, by="listing_id")

rm(last_rating, last_record, all_listings_2015_temp, first_rating, first_record)


names(listings_history)[names(listings_history) == 'number_of_reviews_last'] <- 'reviews_total_2015'
listings_history <- listings_history[,c("host_id", "listing_id", "property_type", "room_type","host_since", "first_seen", "last_seen", 
                                        "first_seen_month", "last_seen_month", "reviews_total_2015", "num_reviews_2015","has_reviews_2015", 
                                        "first_review", "last_review","last_rating", 
                                        "total_occ_2015", "mean_price", "min_price", "max_price", "is_superhost_2015")]

####2016 listings history####

last_seen_2016 <- aggregate(last_scraped ~ listing_id, all_listings_2016, max)
first_seen_2016 <- aggregate(last_scraped ~ listing_id, all_listings_2016, min)
names(last_seen_2016)[names(last_seen_2016) == 'last_scraped'] <- 'last_seen_2016'
names(first_seen_2016)[names(first_seen_2016) == 'last_scraped'] <- 'first_seen_2016'

all_listings_2016_temp <- full_join(all_listings_2016, last_seen_2016, by= "listing_id")
all_listings_2016_temp <- full_join(all_listings_2016_temp, first_seen_2016, by= "listing_id")

last_record_2016 <- filter(all_listings_2016_temp, last_scraped == last_seen_2016)
last_rating_2016 <- distinct(select(last_record_2016, listing_id, number_of_reviews, host_is_superhost))
last_rating_2016 <- mutate(last_record_2016, has_reviews_2016 = number_of_reviews > 0)
total_occ_2016 <- all_listings_2016 %>% count(listing_id)

names(last_rating_2016)[names(last_rating_2016) == 'number_of_reviews'] <- 'reviews_total_2016'
names(last_rating_2016)[names(last_rating_2016) == 'host_is_superhost'] <- 'is_superhost_2016'
names(total_occ_2016)[names(total_occ_2016) == 'n'] <- 'total_occ_2016'


first_record_2016 <- filter(all_listings_2016_temp, last_scraped == first_seen_2016)
first_rating_2016 <- distinct(select(first_record_2016, listing_id, number_of_reviews))
names(first_rating_2016)[names(first_rating_2016) == 'number_of_reviews'] <- 'number_of_reviews_first'



last_rating_2016 <- full_join(first_rating_2016, last_rating_2016, by= "listing_id") %>% 
  mutate(num_reviews_2016 = (reviews_total_2016 - number_of_reviews_first))


listings_history_2016 <- full_join(total_occ_2016, last_rating_2016, by="listing_id")
listings_history_2016 <- listings_history_2016[,c("listing_id","reviews_total_2016",
                                                  "num_reviews_2016","has_reviews_2016","total_occ_2016", "is_superhost_2016")]

listings_history <- left_join(listings_history, listings_history_2016, by="listing_id")

rm(last_seen_2016, all_listings_2016_temp, last_record_2016, last_rating_2016, 
   total_occ_2016, first_record_2016, first_rating_2016, first_seen_2016)

##########


listings_history <- mutate(listings_history, listing_recency_2015_weeks = difftime("2016-01-01", last_seen, unit = "weeks"),
                           scrap_duration = difftime(last_seen, first_seen, unit = "weeks"),
                           host_duration = difftime(last_seen, host_since, unit = "weeks"),
                           #first_review_month_2016 = month(first_review_2016),
                           #last_review_month_2016 = month(last_review_2016), 
                           first_review_year = year(first_review),
                           last_review_year = year(last_review),
                           first_seen_month = month(first_seen), 
                           last_seen_month = month(last_seen)
                           )


#host222016<- all_listings_2015 %>% select(host_id,month(last_scraped), host_is_superhost) %>% mutate(last_scraped = month(last_scraped))%>% distinct() %>%filter(host_is_superhost=="t") %>% group_by(host_id) %>% count(host_id)


superhost2015 <- all_listings_2015 %>% 
  select(host_id, month(last_scraped), host_is_superhost) %>% 
  mutate(last_scraped = month(last_scraped))%>% 
  distinct() %>%
  group_by(host_id) %>%
  tally(host_is_superhost=="t")
names(superhost2015)[names(superhost2015) == 'n'] <- 'is_superhost_count_2015'


superhost2016 <- all_listings_2016 %>% 
  select(host_id, month(last_scraped), host_is_superhost) %>% 
  mutate(last_scraped = month(last_scraped))%>% 
  distinct() %>%
  group_by(host_id) %>%
  tally(host_is_superhost=="t")
names(superhost2016)[names(superhost2016) == 'n'] <- 'is_superhost_count_2016'

listings_history <- left_join(listings_history , superhost2015, by="host_id")
listings_history <- left_join(listings_history , superhost2016, by="host_id")

listings_history <- listings_history %>% group_by(host_id) %>% mutate(host_listings_count = n()) %>% 
  mutate(is_multilisting = (host_listings_count > 1))



load("../ny_reviews.RData")



listings_history <- left_join(listings_history , ny_reviews, by="listing_id")
listings_history <- mutate(listings_history, review_recency_2015_weeks = difftime("2016-01-01", last_review_2015, unit = "weeks"), 
                           first_review_month_2015 = month(first_review_2015),
                           last_review_month_2015 = month(last_review_2015)
                           )




rm(superhost2016, superhost2015)

#load csv file into data frame
amenities <- read_csv("../table_amenities_grand_final.csv")
verification <- read_csv("../table_verifications_grand_final.csv")
names(amenities)[names(amenities) == 'id'] <- 'listing_id'
names(verification)[names(verification) == 'id'] <- 'listing_id'

listings_history <- left_join(listings_history , amenities, by="listing_id")
listings_history <- left_join(listings_history , verification, by="listing_id")


listings_history <- listings_history[,c("host_id", "listing_id", "property_type", "room_type", "mean_price", "min_price", 
                                        "max_price", "is_multilisting", "host_since", "host_listings_count", "host_duration", "first_seen", 
                                        "last_seen", "first_seen_month", "last_seen_month",
                                        "listing_recency_2015_weeks", "scrap_duration", "total_occ_2015", "first_review", 
                                        "first_review_year", "last_review", "last_review_year", "reviews_total_2015", "num_reviews_2015",           
                                        "has_reviews_2015", "first_review_2015", "first_review_month_2015", "last_review_2015", 
                                        "last_review_month_2015", "review_recency_2015_weeks", "text_2015",
                                        "last_rating", "is_superhost_2015", "is_superhost_count_2015", "TV", "Internet", "Wireless.Internet", 
                                        "Air.Conditioning", "Kitchen", "Heating", "Family.Kid.Friendly", "Smoke.Detector", 
                                        "Carbon.Monoxide.Detector", "Essentials", "Shampoo", "Cable.TV", "Free.Parking.on.Premises", 
                                        "Breakfast", "Pets.live.on.this.property", "Dog.s.", "First.Aid.Kit", "Buzzer.Wireless.Intercom", 
                                        "Washer", "Dryer", "Pets.Allowed", "Gym", "Safety.Card", "Fire.Extinguisher", "Wheelchair.Accessible", 
                                        "Cat.s.", "Indoor.Fireplace", "Suitable.for.Events", "Doorman", "Hot.Tub", "Elevator.in.Building", "Pool", 
                                        "Smoking.Allowed", "Other.pet.s.", "Washer...Dryer", "Lock.on.Bedroom.Door", "X24.Hour.Check.in", "Hangers", 
                                        "Hair.Dryer", "Iron", "Laptop.Friendly.Workspace", "translation.missing..en.hosting_amenity_49", 
                                        "translation.missing..en.hosting_amenity_50", "email", "phone", "facebook", "reviews", 
                                        "kba", "google", "jumio", "sent_id", "linkedin", "manual_offline", "manual_online", "weibo", "photographer", 
                                        "None", "amex", "verifications_count", "total_occ_2016", "has_reviews_2016", 
                                        "first_review_2016", "last_review_2016", 
                                        "reviews_total_2016", "num_reviews_2016", "text_2016", "is_superhost_2016", "is_superhost_count_2016" )]  
listings_history1 <- mutate(listings_history, exist_in_2016 = (is.na(total_occ_2016)|is.na(has_reviews_2016)|is.na(num_reviews_2016)))

#set NAs in total_occ_2016 to zero
#this means the listing no longer existed in 2016
listings_history[c("total_occ_2016")][is.na(listings_history[c("total_occ_2016")])] <- 0

listings_history[c("text_2016", "text_2015")][is.na(listings_history[c("text_2016", "text_2015")])] <- ""

#create a column that checks if listing still exist in 2016
listings_history1 <- mutate(listings_history, exist_in_2016 = (is.na(total_occ_2016)|is.na(has_reviews_2016)|is.na(num_reviews_2016)))

write_csv(listings_history, "listing_history.csv")

x <- filter(all_listings, host_id == 2787) %>% select(listing_id, last_scraped, first_review, last_review, review_scores_rating, number_of_reviews)
all_nas <- listings_history[is.na(listings_history$first_review),]


######crime#####


####purged listings####
purged_listings <- read_csv("../../raw_data/purged_listings.csv")

purged_listings <- mutate(purged_listings, purged = TRUE) #add purged T/F column

purged_listings_skinny <- select(purged_listings, id, purged) # selecting only id and T/F column

colnames(purged_listings_skinny)[1] <- "listing_id" #change "id" to "listings_id"

listings_history <- left_join(listings_history, purged_listings_skinny, by="listing_id") #join

listings_history$purged[is.na(listings_history$purged)] <- FALSE #changing NAs to FALSE (becuase when it's NA, it means it was NOT a purge)

summary(listings_history$purged) #checking for no NAs. Should have 1,324 TRUE

write_csv(listings_history, "listing_history.csv") #writing file

colnames(listings_history)[104]

###### words #####
word_features <- purged_listings <- read_csv("../../raw_data/word_features.csv")

colnames(listings_history) #check that this is the most recent version: should have "purged"

listings_history <- left_join(listings_history, word_features, by="listing_id") #join. Should have 205 columns! Ahh!

write_csv(listings_history, "../../raw_data/listing_history.csv") #writing file

###### skim #####

#selects one listings per host at random
skimmed_listings_history <- listings_history %>% group_by(host_id.x) %>% filter(row_number() == sample(1:row_number(), 1))

###### location #####
#add location information for each listing in listings_history
recent_listing <- all_listings %>% group_by(id) %>% arrange(last_scraped) %>% filter(row_number() == n()) #get the most recent listing

recent_listing_location <- recent_listing[,c(1, 11:12, 81:82, 13:20, 75)] # select only location-related columns, ordered for better comparison

colnames(recent_listing_location)[1] <- "listing_id" #change "id" to "listings_id"
View(recent_listing_location)

#nrow(listings_history) #64916 (check! Both must be equal)
#nrow(all_listings_2015%>% group_by(id) %>% arrange(last_scraped) %>% filter(row_number() == n()) ) #64916 

listings_history <- left_join(listings_history, recent_listing_location, by="listing_id")

ncol(listings_history) # 218 OMG

View(listings_history[,c(2,206:218)]) # visual check to ensure proper join

write_csv(listings_history, "../../raw_data/listing_history.csv") #writing file

######  #####