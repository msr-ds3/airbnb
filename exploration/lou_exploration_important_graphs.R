###### Murray Cox graph replicated!!
ggplot(aes(dates, percent_of_multilistings), data=df_multilistings) + 
  geom_point(color="blue") + geom_line(color="blue") + 
  scale_x_date(breaks=date_breaks("months"), labels=date_format("%b")) +
  scale_y_continuous(limits=c(0,20)) + 
  xlab("Month") + 
  ylab("% of Multi-listings on Airbnb") 

#plotting only multi-listings, % that were gone from airbnb
ggplot() +
  geom_point(aes(scrape_date, percent_that_left, group=1), colour="blue", data=how_many_entire_left) + 
  geom_line(aes(scrape_date, percent_that_left, group=1), colour="blue", data=how_many_entire_left) + 
  scale_x_date(breaks=date_breaks("months"), labels=date_format("%b")) +
  scale_y_continuous(limits=c(0,20)) + 
  xlab("Month") + 
  ylab("% of Multi-listings That Left Airbnb") 

#plotting total people that left vs total (entire, multi-) listings that left
ggplot() +
  geom_point(aes(scrape_date, percent_that_left, group=1), colour="blue", data=how_many_entire_left) + 
  geom_line(aes(scrape_date, percent_that_left, group=1), colour="blue", data=how_many_entire_left) + 
  geom_point(aes(scrape_date, percent_that_left, group=1), colour="red", data=how_many_left) +
  geom_line(aes(scrape_date, percent_that_left, group=1), colour="red", data=how_many_left) +
  scale_x_date(breaks=date_breaks("months"), labels=date_format("%b")) +
  scale_y_continuous(limits=c(0,20)) + 
  xlab("Month") + 
  ylab("% Multi-Listings & Listings That Left Airbnb") 

View(listings1509)


