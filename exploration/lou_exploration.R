library(ggplot2)

load("../airbnb.RData")

View(d)
View(calendar)
ggplot(aes(x=price), data=d) + geom_density() + ylim(0,1000) #shows that there is very little correlation btwn price and listing quantity

View(reviews)
View(listings)
View(neigh)

  names(listings)

#average price per neighbourhood
listings <- mutate(listings, price = as.numeric(gsub("[$,]", "", price)))
basic <- select(listings, id, price, neighbourhood_cleansed, zipcode) %>% mutate(price = as.numeric(gsub("[$,]", "", price)))

View(basic)

by_neigh <- basic %>% group_by(neighbourhood_cleansed) %>% summarize(total_listings = n(),total_price = sum(price))
by_neigh <- by_neigh %>% mutate( avg_price = total_price/total_listings) %>% arrange(avg_price)
View(by_neigh)
summary(by_neigh)
ggplot(aes(x=neighbourhood_cleansed, y=avg_price), data=by_neigh) + geom_point()

#log price per neigh
hist(log(by_neigh$avg_price))

#price per neighbourhood where listings > 5
by_neigh_5 <- filter(by_neigh, total_listings > 5)
View(by_neigh_5)

#average price per zip code
by_zip <- basic %>% group_by(zipcode) %>% summarize(total_listings = n(),total_price = sum(price))
by_zip <- mutate(by_zip, avg_price = total_price/total_listings)
View(by_zip)
summary(by_zip)

#frequency by zipcode
freq_by_zip <- basic %>% group_by(zipcode) %>% summarize(total_listings = n())
View(freq_by_zip)
ggplot(aes(x=zipcode, y=total_listings), data=freq_by_zip) + geom_point() + ylim(0,1000) 
# >> certain zipcodes blow up

#what amenity is the most used?
first <-strsplit(gsub("[{}\"]", "", as.character(listings[,59][1])), "[,\"]") #splits the amenity string into individual words
first[1]

#affect of amenities on price / review score

#price based on host name (i.e. gender)

#price ~ host response rate

#price or review ~ host verified?

#price ~ property type (home/apt)

#price ~ #bathrooms || #rooms

#price ~ #reviews

#