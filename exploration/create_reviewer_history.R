# Gather all reviewers and create dataframe with relevant fields

library(readr)
library(dplyr)
library(lubridate)


# read review csvs for ten cities (run pull_other_reviews.sh to pull files)
csvs <- Sys.glob('../raw_data/*cities-reviews.csv')
reviews <- data.frame()
for (csv in csvs) {
  print(csv)
  tmp <- read_csv(csv, na='\\N')
  reviews <- rbind(reviews, tmp)
}

# save to RData
save(reviews, file = "ten_cities_reviews.RData")

#######
# load RData file
load("ten_cities_reviews.RData")

# function to generate dataframe with each reviewer's first month, last month,
# number of reviews before 2016, number of reviews from all time, name, 
# and the first, last, total, and review text in a specified time period
# params: df - data frame
#         start_month - starting month for num_within_time_period column
#         end_month - ending month for num_within_time_period column
#         end_in_2016 - ending month for how many months to look at in 2016
#         (all month parameters must be formatted "YYYY-MM-DD")
get_reviewer_data <- function(df, start_month = FALSE, end_month = "2015-12-31",
                              end_in_2016 = "2016-12-31") {
  if (start_month == FALSE) {
    start_month <- min(df$date)
  }
  
  tmp_1 <- group_by(df, reviewer_id) %>%
    arrange(date) %>%
    summarize(first_month = first(date),
              first_month_num = month(first(date)),
              last_month = last(date),
              last_month_num = month(last(date)),
              num_in_2016 = sum(date >= as.Date("2016-01-01", "%Y-%m-%d") 
                                & date <= as.Date(end_in_2016, "%Y-%m-%d")),
              all_time_reviews = n(),
              name = first(reviewer_name))
  
  tmp_2 <- group_by(df, reviewer_id) %>%
    filter(date >= as.Date(start_month, "%Y-%m-%d") 
           & date <= as.Date(end_month, "%Y-%m-%d")) %>%
    summarize(text_within_time_period = paste(comments, collapse = "|"),
              num_within_time_period = n(),
              first_within_time_period = first(date),
              last_within_time_period = last(date))
  
  inner_join(tmp_1, tmp_2, by = "reviewer_id")
}

# call looking at review data using default parameters, running to the 
# end of the current data set
# reviewer_data <- get_reviewer_data(reviews)

# starting with January 2015
reviewer_data <- get_reviewer_data(reviews, "2015-01-01", "2015-12-31")

# write to csv
write_csv(reviewer_data, path = "reviewer_data.csv")

# saving reviews and resulting data
save(reviews, reviewer_data, file = "reviewer_data.RData")
load("reviewer_data.RData")
