library(ggplot2)
library(dplyr)

load("../airbnb.RData")

View(d)
View(calendar)
ggplot(aes(x=price), data=d) + geom_density() + ylim(0,1000) #shows that there is very little correlation btwn price and listing quantity

View(reviews)
View(listings)
View(neighbourhoods)

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
total_amenities = c()

for(i in 1:nrow(listings)){
  print(sprintf("============ %f", i))
  one_listings_amenities <-strsplit(gsub("[{}\"]", "", listings[i, "amenities"]), "[,]")[[1]] #amenities for listing number #listnum
  
  if(length(one_listings_amenities) != 0){ #only continue if amenities are not empty for that listing
    for(n in 1:length(one_listings_amenities)){
      #print(n)
      #print(one_listings_amenities[[n]])
      amenity = one_listings_amenities[[n]]
      if(!amenity %in% total_amenities){
        total_amenities = c(total_amenities, amenity)
      }
    }
  }
}

write.table(total_amenities, 'unique_amenities.tsv', sep='\t')

#exporting ID and amenities to CSV file
amen_bare <- select(listings, id, amenities)
View(amen_bare)
write.table(amen_bare, "amenities_and_id.tsv", sep='\t')

true_false_amen = read.csv('table_amenities.csv')
View(true_false_amen)

#affect of amenities on price / review score
true_false_amen<-mutate(true_false_amen, price = basic$price)
true_false_amen <- true_false_amen[, c(1, 2, 45, 3, 4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44)]
View(true_false_amen)

lm.fit1 <- lm(price ~ ., data = true_false_amen)
summary(lm.fit1)

lm.fit2 <- lm(price ~ TV + Wireless.Internet + Air.Conditioning + Free.Parking.on.Premises + Family.Kid.Friendly + Smoke.Detector + Essentials + Cable.TV + Fire.Extinguisher + Indoor.Fireplace + Breakfast + Lock.on.Bedroom.Door + Wheelchair.Accessible + Suitable.for.Events + Smoking.Allowed + Doorman, data=true_false_amen)
summary(lm.fit2)

#which amenity is the most/least prevalent?
amenity_count <- c()
for(i in 2:44)
  amenity_count <- c(amenity_count, length(which(true_false_amen[,i], TRUE)))

amenity_summary_df <- data.frame(amenity_type=total_amenities, frequency=amenity_count) %>% arrange(desc(amenity_count))
View(amenity_summary_df)

amenity_summary_df$amenity_type <- factor(amenity_summary_df$amenity_type, levels = amenity_summary_df$amenity_type[order(amenity_summary_df$frequency)])

ggplot(aes(amenity_type, frequency), data=amenity_summary_df) + 
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) # angle x axis

#price based on host name (i.e. gender)

#price ~ host response rate

#price or review ~ host verified?

#price ~ property type (home/apt)

#price ~ #bathrooms || #rooms

#price ~ #reviews

#