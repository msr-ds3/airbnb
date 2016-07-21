# Gather all reviewers and create dataframe with relevant fields

library(readr)
library(dplyr)
library(lubridate)

csvs <- Sys.glob('../raw_data/*-reviews.csv')
reviews <- data.frame()
for (csv in csvs) {
  print(csv)
  tmp <- read_csv(csv, na='\\N')
  reviews <- rbind(reviews, tmp)
}

save(reviews, file = "ten_cities_reviews.RData")

#######

load("ten_cities_reviews.RData")

get_reviewer_data <- function(df, start_month = FALSE, end_month = "2015-12-31", end_in_2016 = "2016-12-31") {
  if (start_month == FALSE) {
    start_month <- min(df$date)
  }
  tmp_1 <- group_by(df, reviewer_id) %>%
    arrange(date) %>%
    summarize(first_month = first(date),
              last_month = last(date),
              num_within_time_period = sum(date >= as.Date(start_month, "%Y-%m-%d") 
                                           & date <= as.Date(end_month, "%Y-%m-%d")),
              num_before_2016 = sum(date < as.Date("2016-01-01", "%Y-%m-%d")),
              num_in_2016 = sum(date >= as.Date("2016-01-01", "%Y-%m-%d") 
                                & date <= as.Date(end_in_2016, "%Y-%m-%d")),
              all_time_reviews = n(),
              name = first(reviewer_name))
  
  tmp_2 <- group_by(df, reviewer_id) %>%
    filter(date <= as.Date(end_in_2016, "%Y-%m-%d")) %>%
    summarize(text = paste(comments, collapse = "|"))
  
  inner_join(tmp_1, tmp_2, by = "reviewer_id")
}

system.time(reviewer_data <- get_reviewer_data(reviews, "2016-06-01", "2016-07-01", ""))