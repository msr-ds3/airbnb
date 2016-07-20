# Gather all reviewers and create dataframe with relevant fields

library(readr)
library(dplyr)
library(lubridate)

reviews <- read_csv("../raw_data/tmp_small.csv")

get_reviewer_data <- function(df, start_month, end_month) {
  group_by(df, reviewer_id) %>%
    arrange(date) %>%
    summarize(first_month = first(date),
              last_month = last(date),
              num_within_time_period = sum(date >= as.Date(start_month, "%Y-%m-%d") 
                                           & date <= as.Date(end_month, "%Y-%m-%d")),
              num_before_2016 = sum(date < as.Date("2016-01-01", "%Y-%m-%d")),
              num_in_2016 = sum(date >= as.Date("2016-01-01", "%Y-%m-%d")),
              text = paste(comments, collapse = "|"),
              name = first(reviewer_name))
}

View(get_reviewer_data(reviews, "2016-06-01", "2016-07-01"))

reviews %>% filter(date < as.Date("2016-01-01", "%Y-%m-%d"))
