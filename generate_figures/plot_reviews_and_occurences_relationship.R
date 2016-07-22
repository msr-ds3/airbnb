#to plot the relationship between number of reviews and total occurences in 2015

library(ggplot2)
library(readr)

listings_history <- read_csv("../raw_data/listing_history.csv", na='\\N')

#first find the correlation
cor(listings_history$num_reviews_2015, listings_history$total_occ_2015)
  #0.52

#to plot the relationship

ggplot(data = listings_history, aes(x = total_occ_2015, y = num_reviews_2015)) + 
  geom_point() + 
  xlab("Total Occurences of Each Listing in 2015") + 
  ylab("Number of Reviews in 2015") + 
  ggtitle("Relationship Between Total Occurences and Number of Reviews in 2015") + 
  ggsave(file = "../figures/reviews_and_occurences_relationship.pdf")
