library(ggplot2)
load("airbnb.RData")

# number of listings per neighbourhood colored by neighbourhood group
ggplot(listings, aes(x = neighbourhood_cleansed, fill = neighbourhood_group_cleansed)) +
  geom_bar()

# number of listings per zipcode
ggplot(listings, aes(x = zipcode)) +
  geom_bar()

# number of listings per neighbourhood group
# based on this graph, it can be determined that the most listings as of 6/2/2016 are in Manhattan
ggplot(listings, aes(x = neighbourhood_group_cleansed)) +
  geom_bar()

# histogram of dates and different bars showing number of listings available
# because this is a look at the future availability from 6/2/2016, the soonest times tend to be 
# booked more than not, and also upticks can be noticed around holiday times (Thanksgiving and Christmas)
ggplot(calendar, aes(x = date, fill = available)) + 
  geom_bar()

# dot plot of review ratings compared to the number of reviews
# listings with more reviews appear to have a higher ratings overall, my guess being that ones with good
# initial ratings are more likely to be booked and reviewed again compared to ones with a small # of bad reviews
ggplot(listings, aes(x = review_scores_rating, y = number_of_reviews)) +
  geom_point()

# count by room type, colored by neighbourhood group
# mostly entire home/apt in all 5 groups, shared room only apparent in Manhattan, Queens, and Brooklyn
ggplot(listings, aes(x = room_type, fill = neighbourhood_group_cleansed)) +
  geom_bar()

# count of listings by rating and colored by instant bookable
# based on this graph, instant bookable listings make up a minority of the total listings regardless of rating
ggplot(listings, aes(x = review_scores_rating, fill = instant_bookable)) + 
  geom_bar()

# count by bed type
# most listings have a "Real Bed"
ggplot(listings, aes(x = bed_type)) + 
  geom_bar()

