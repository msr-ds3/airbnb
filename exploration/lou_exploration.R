
library(ggplot2)

reviews = read.csv('reviews.csv')
listings = read.csv('listings.csv')
calendar = read.csv('calendar.csv')
neigh = read.csv('neighbourhoods.csv')

View(d)
View(calendar)
ggplot(aes(x=price), data=d) + geom_density() + ylim(0,1000) #shows that there is very little correlation btwn price and listing quantity

View(reviews)
View(listings)
View(neigh)
i = 1
vec = c()
while(i < ncol(listings)){
  vec <- c(vec, colnames(listings)[i])
  i = i + 1
}
vec

#average price per neighbourhood
listings <- mutate(listings, price = as.numeric(gsub("[$,]", "", price)))
basic <- select(listings, id, price, neighbourhood_cleansed, zipcode) %>% mutate(price = as.numeric(gsub("[$,]", "", price)))

View(basic)

by_neigh <- basic %>% group_by(neighbourhood_cleansed) %>% summarize(total_listings = n(),total_price = sum(as.numeric(price)))
by_neigh <- mutate(by_neigh, avg_price = total_price/total_listings)
View(by_neigh)

#average price per zip code
by_zip <- basic %>% group_by(zipcode) %>% summarize(total_listings = n(),total_price = sum(as.numeric(price)))
by_zip <- mutate(by_zip, avg_price = total_price/total_listings)
View(by_zip)

#frequency by zipcode
freq_by_zip <- basic %>% group_by(zipcode) %>% summarize(total_listings = n())
View(freq_by_zip)
ggplot(aes(x=zipcode, y=total_listings), data=freq_by_zip) + geom_point() + ylim(0,1000) #certain zipcodes blow up

#affect of amenities on price / review score

#price based on host name (i.e. gender)

#price ~ host response rate

#price or review ~ host verified?

#price ~ property type (home/apt)

#price ~ #bathrooms || #rooms