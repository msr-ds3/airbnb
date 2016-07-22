# Assemble New York review data into dataframe with fields for listing id,
# text for 2015, first review 2015, last review 2015, first review 2016, and
# last review 2016

library(readr)
library(dplyr)
library(lubridate)

# read in all New York review files
csvs <- Sys.glob('../raw_data/*-ny-reviews.csv')
reviews <- data.frame()
for (csv in csvs) {
  print(csv)
  tmp <- read_csv(csv, na='\\N')
  reviews <- rbind(reviews, tmp)
}

# arrange by listing_id and date, then remove duplicates by review id
reviews <- reviews %>%
  arrange(listing_id, date) %>%
  distinct(id)

# determine 2015 data columns
tmp_1 <- group_by(reviews, listing_id) %>%
  filter(date >= as.Date("2015-01-01", "%Y-%m-%d") & 
           date <= as.Date("2015-12-31", "%Y-%m-%d")) %>%
  summarize(text_2015 = paste(comments, collapse = "|"),
            first_review_2015 = first(date),
            last_review_2015 = last(date))

# determine 2016 data columns
tmp_2 <- group_by(reviews, listing_id) %>%
  filter(date >= as.Date("2016-01-01", "%Y-%m-%d") & 
           date <= as.Date("2016-12-31", "%Y-%m-%d")) %>%
  summarize(text_2016 = paste(comments, collapse = "|"),
            first_review_2016 = first(date),
            last_review_2016 = last(date))

# join columns together (NAs appear where a listing received reviews in one year
# and not the other)
ny_reviews <- full_join(tmp_1, tmp_2, by = "listing_id")

# save to RData
save(ny_reviews, file = "ny_reviews.RData")
