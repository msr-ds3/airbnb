# Gather all reviewers and create dataframe with relevant fields

library(readr)
library(dplyr)
library(lubridate)

reviews <- read_csv("../raw_data/tmp_small.csv")

month_and_year <- function(date) {
  
}

get_reviewer_data <- function(df, start_month, end_month) {
  group_by(df, reviewer_id) %>%
    arrange(date) %>%
    summarize(first_month = as.Date(first(date), "%Y-%m"),
              last_month = format(last(date), "%Y-%m"),
              num_within_time_period = sum(date >= as.Date(start_month, "%Y-%m-%d") 
                                           & date <= as.Date(end_month, "%Y-%m-%d")),
              num_before_2016 = sum(date < as.Date("2016-01-01", "%Y-%m-%d")),
              num_in_2016 = sum(date >= as.Date("2016-01-01", "%Y-%m-%d")),
              text = paste(comments, collapse = "|"),
              name = first(reviewer_name))
}

reviewer_data <- get_reviewer_data(reviews, "2016-06-01", "2016-07-01")

write_csv(reviewer_data, path = "test.csv")

reviews %>% filter(date < as.Date("2016-01-01", "%Y-%m-%d"))