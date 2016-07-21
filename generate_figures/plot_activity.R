#to plot the activity of listings and reviews

library(ggplot2)


#load the derived data sets
load("listings_history.RData")
load("reviewer_history.RData")

#create histogram of number of occurances of each listing in 2015

ggplot(data = listings_history, aes(x = total_occ_2015)) + 
  geom_histogram() +
  xlab("Total Occurances in 2015") +
  ggtitle("Frequency of Occurances by Listing for 2015")




#create histogram of number of reviews of each reviewer in 2015
ggplot(data = reviewer_history.RData, aes(x= )) + 
  geom_histogram() + 
  xlab("") +
  ggtitle("")

