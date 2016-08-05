library(dplyr)
library(readr)
library(ggplot2)
library(reshape)
library(scales)
library(tidyr)
library(lubridate)

library(tigris)
library(leaflet)
library(sp)
library(ggmap)
library(maptools)
library(broom)
library(httr)
library(rgdal)


install.packages("wordcloud")
install.packages("tm")
Needed <- c("SnowballCC", "RColorBrewer", "biclust", "cluster", "igraph", "fpc")
install.packages(Needed, dependencies = TRUE)

install.packages("Rcampdf", repos = "http://datacube.wu.ac.at/", type = "source")




library(wordcloud)
library(tm)


review_history <- read_csv("../exploration/historical_exploration/us_rev_data.csv")

#review_history <- mutate(review_history, ny_keyword = grepl('NY', name, ignore.case = TRUE))


review_history <- review_history %>% distinct(text_in_2015, .keep_all = TRUE) %>% sample_frac(.001)
review_history<- review_history[complete.cases(review_history),]


  
str(review_history)

review_text <- paste(review_history$text_in_2015, collapse=" ")
review_source <- VectorSource(review_text)
corpus <- Corpus(review_source)
corpus <- tm_map(corpus, content_transformer(tolower))

corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
dtm <- DocumentTermMatrix(corpus)
dtm2 <- as.matrix(dtm)
frequency <- colSums(as.matrix(dtm2))
length(frequency) 
ord <- order(frequency)  

wf <- data.frame(word=names(frequency), freq=frequency)   

p <- ggplot(subset(wf, freq>275), aes(word, freq))    
p <- p + geom_bar(stat="identity")   
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1)) 
p


frequency <- sort(frequency, decreasing=TRUE)

words <- names(frequency)




wordcloud(review_history$text_in_2015)





#read listings history csv into a dataframe
reviews <- read_csv("../raw_data/2015-04-01-ny-reviews.csv", stringAsFactors=FALSE)


#read listings history csv into a dataframe
listings_history <- read_csv("../exploration/historical_exploration/listing_history.csv")



# num of reviews vs month with January cohort ??
listings_history %>% filter(first_seen_month == 1) %>% ggplot(aes(x = last_seen_month, y = num_reviews_in_2015)) + geom_point() + geom_smooth(method = "lm")

listings_history %>% filter(first_seen_month == 1) %>% ggplot(aes(x = as.factor(last_seen_month), y = num_reviews_in_2015)) + geom_boxplot()

cor(listings_history$num_reviews_in_2015, listings_history$)

# num of reviews vs month with January cohort ??
ggplot(listings_history, aes(x = as.factor(total_occ_2015), y = num_reviews_in_2015)) + geom_boxplot()

ggplot(listings_history, aes(x = as.factor(total_occ_2015), y = last_rating)) + geom_boxplot()

# num of listings vs month with January cohort ??

all_listings_2015 %>% filter(month(last_scraped) == 1) %>% group_by(scrape_id) %>% summarise(num_listings = n())
  ggplot(aes(x = last_scraped, y = num_reviews_in_2015)) + geom_point() + geom_smooth(method = "lm")
  
  
#listing recency vs num_reviews Jan Cohort
listings_history %>% filter(first_seen_month == 1) %>% ggplot(aes(x = as.factor(listing_recency_2015_weeks), y = num_reviews_in_2015)) + geom_boxplot()


listings_history %>% filter(first_seen_month == 1) %>% ggplot(aes(x = host_listings_count, y = num_reviews_in_2015)) + geom_point() + geom_smooth(method = "lm")



ggplot(data = listings_history, aes(x = room_type)) + 
  geom_bar()

ggplot(data = listings_history, aes(x = year(host_since))) + 
  geom_bar()


#listing recency
ggplot(data = listings_history, aes(x = listing_recency_2015_weeks)) + 
  geom_histogram() +
  xlab("Listing Recency in Weeks") +
  ylab("Frequency") +
  ggtitle("Listing Recency 2015") +
  ggsave(file = "../airbnb/figures/hist_occurances_by_listing_2015.pdf")

#review recency
ggplot(data = listings_history, aes(x = review_recency_2015_weeks)) + 
  geom_histogram() +
  xlab("Review Recency in Weeks") +
  ylab("Frequency") +
  ggtitle("Review Recency 2015") 

#num reviews a listing got in 2015
ggplot(data = listings_history, aes(x = num_reviews_in_2015)) + 
  geom_histogram() +
  xlab("num 2015 Reviews") +
  ylab("Frequency") +
  ggtitle("2015 Reviews") 

#num reviews a listing got in 2016
ggplot(data = listings_history, aes(x = num_reviews_in_2016)) + 
  geom_histogram() +
  xlab("num 2016 Reviews") +
  ylab("Frequency") +
  ggtitle("2016 Reviews") 

listings_history %>% filter(num_reviews_in_2016>0)%>%  ggplot(aes(x = num_reviews_in_2016)) + 
  geom_histogram() +
  xlab("num 2016 Reviews") +
  ylab("Frequency") +
  ggtitle("2016 Reviews") 


#times a host has been a superhost
ggplot(data = listings_history, aes(x = is_superhost_count_2015)) + 
  geom_histogram() +
  xlab("Superhost count 2015") +
  ylab("Frequency") +
  ggtitle("Times a Host has been a Superhost") 

#without those who were superhost atleast 1x
listings_history %>% filter(is_superhost_count_2015>0)%>% ggplot(aes(x = is_superhost_count_2015)) + 
  geom_histogram() +
  xlab("Superhost count 2015") +
  ylab("Frequency") +
  ggtitle("Times a Host has been a Superhost in 2015") 

#times a host has been a superhost 2016
ggplot(data = listings_history, aes(x = is_superhost_count_2016)) + 
  geom_histogram() +
  xlab("Superhost count 2016") +
  ylab("Frequency") +
  ggtitle("Times a Host has been a Superhost") 

#without those who were superhost atleast 1x 2016
listings_history %>% filter(is_superhost_count_2016>0)%>% ggplot(aes(x = is_superhost_count_2016)) + 
  geom_histogram() +
  xlab("Superhost count 2016") +
  ylab("Frequency") +
  ggtitle("Times a Host has been a Superhost in 2016") 

#last seen month
ggplot(listings_history, aes(x = last_seen_month)) + 
  geom_histogram() +
  xlab("Last Seen Month") +
  ylab("Frequency") +
  ggtitle("Month the Listing Was Last Seen in 2015") 

listings_history %>% filter(last_seen_month < 12)%>% ggplot(aes(x = last_seen_month)) + 
  geom_histogram() +
  xlab("Last Seen Month") +
  ylab("Frequency") +
  ggtitle("Month the Listing Was Last Seen in 2015") 

#first seen month
ggplot(listing_history, aes(x = first_seen_month)) + 
  geom_histogram() +
  xlab("first Seen Month") +
  ylab("Frequency") +
  ggtitle("Month the Listing Was First Seen in 2015") 

listings_history %>% filter(first_seen_month>1)%>% ggplot(aes(x = first_seen_month)) + 
  geom_histogram() +
  xlab("first Seen Month") +
  ylab("Frequency") +
  ggtitle("Month the Listing Was First Seen in 2015") 

#last review month
ggplot(listings_history, aes(x = month(last_review))) + 
  geom_histogram() +
  xlab("Month of Last Review") +
  ylab("Frequency") +
  ggtitle("Month the Last Review Was Given") 

listings_history %>% filter(last_seen_month < 12)%>% ggplot(aes(x = last_seen_month)) + 
  geom_histogram() +
  xlab("Last Seen Month") +
  ylab("Frequency") +
  ggtitle("Month the Listing Was Last Seen in 2015") 

#last rating
ggplot(listings_history, aes(x = last_rating)) + 
  geom_histogram() +
  xlab("Last Overall Rating") +
  ylab("Frequency") +
  ggtitle("Overall Listings Rating As of 2015") 

listings_history %>% filter(last_rating > 60)%>%  ggplot(aes(x = last_rating)) + 
  geom_histogram() +
  xlab("Last Overall Rating") +
  ylab("Frequency") +
  ggtitle("Overall Listings Rating As of 2015") 

listings_history %>% filter(last_rating < 50)%>%  ggplot(aes(x = last_rating)) + 
  geom_histogram() +
  xlab("Last Overall Rating") +
  ylab("Frequency") +
  ggtitle("Overall Listings Rating As of 2015") 

#last rating
ggplot(listings_history, aes(x = last_rating)) + 
  geom_histogram() +
  xlab("Last Overall Rating") +
  ylab("Frequency") +
  ggtitle("Overall Listings Rating As of 2015") 

#num_reviews
ggplot(data = listings_history, aes(x = num_as_of_2015)) + 
  geom_histogram() +
  xlab("num Reviews") +
  ylab("Frequency") +
  ggtitle("Number of Reviews Listings Have") 

listings_history %>% filter(num_as_of_2015 > 0)%>% ggplot( aes(x = num_as_of_2015)) + 
  geom_histogram() +
  xlab("num Reviews") +
  ylab("Frequency") +
  ggtitle("Number of Reviews Listings Have") 









# 2016(# reviews) vs 2015(# times superhost)
ggplot(listings_history, aes(x = as.factor(is_superhost_count_2015), y = num_reviews_in_2016)) + 
  geom_boxplot() + 
  geom_smooth(method = "lm")

# 2016(# reviews) vs 2015(superhost > 0)
ggplot(listings_history, aes(x = (is_superhost_count_2015 > 0), y = num_reviews_in_2016)) + 
  geom_boxplot() + 
  geom_smooth(method = "lm")

# 2016(# reviews) vs 2015(last_seen_month)
ggplot(listings_history, aes(x = as.factor(last_seen_month), y = num_reviews_in_2016)) + 
  geom_boxplot() + 
  geom_smooth(method = "lm")

# 2016(# reviews) vs 2015(last_seen_review)
ggplot(listings_history, aes(x = as.factor(month(last_review_2015)), y = num_reviews_in_2016)) + 
  geom_boxplot() + 
  geom_smooth(method = "lm")

ggplot(listings_history, aes(x = review_recency_2015_weeks, y = num_reviews_in_2016)) + 
  geom_count() + 
  geom_smooth(method = "lm")


# 2016(# reviews) vs 2015(last_rating)
ggplot(listings_history, aes(x = as.factor(last_rating), y = num_reviews_in_2016)) + 
  geom_boxplot() + 
  geom_smooth(method = "lm") +facet_wrap(.~)

# 2016(# reviews) vs 2015(# reviews)
ggplot(listings_history, aes(x = num_reviews_in_2015, y = num_reviews_in_2016)) + 
  geom_count() + 
  geom_smooth(method = "lm")

# 2016(# reviews) vs 2015(reviews > 0s)
ggplot(listings_history, aes(x = has_reviews_2015, y = num_reviews_in_2016)) + 
  geom_jitter() + 
  geom_smooth(method = "lm")

ggplot(listings_history, aes(x = as.factor(first_seen_month)), y = num_reviews_in_2016) + 
  geom_boxplot() 





# 2016(# reviews) vs 2015(total_occ)
ggplot(listings_history, aes(x = as.factor(num_reviews_in_2015), y = total_occ_2015)) + 
  geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("Number of Reviews in 2015") +
  ylab("Total Occurences in 2015") 



ggplot(listings_history, aes(x = as.factor(num_reviews_in_2015), y = total_occ_2015)) + 
  geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("Number of Reviews in 2015") +
  ylab("Total Occurences in 2016") 

ggplot(listings_history, aes(x = as.factor(total_occ_2016), y = num_reviews_in_2015)) + 
  geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("Total Occurences in 2016") +
  ylab("Number of Reviews in 2015") + coord_flip()

ggplot(listings_history, aes(x = as.factor(total_occ_2015), y = num_reviews_in_2015) + 
  geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("Total Occurences in 2015") +
  ylab("Number of Reviews in 2015") + coord_flip()








ggplot(listings_history, aes(x = as.factor(total_occ_2016), y = num_reviews_in_2015)) + 
  geom_boxplot() + xlab("Total Occurences in 2016") +
  ylab("Number of Reviews in 2015") 



+ 
  geom_smooth(method = "lm")

# 2016(# reviews) vs 2015(mean_price)
ggplot(listings_history, aes(x =mean_price, y = num_reviews_in_2016)) + 
  geom_count() + 
  geom_smooth(method = "lm")

# 2016(# reviews) vs 2015(min_price)
ggplot(listings_history, aes(x =min_price, y = num_reviews_in_2016)) + 
  geom_count() + 
  geom_smooth(method = "lm")

# 2016(# reviews) vs 2015(max_price)
ggplot(listings_history, aes(x =max_price, y = num_reviews_in_2016)) + 
  geom_count() + 
  geom_smooth(method = "lm")











# 2016(atleast 1 review) vs 2015(# times superhost)
ggplot(listings_history, aes(x =has_reviews_2016 , y = is_superhost_count_2015)) + 
  geom_count() + 
  geom_smooth(method = "lm")

# 2016(atleast 1 review) vs 2015(superhost > 0)
ggplot(listings_history, aes(x = (is_superhost_count_2015 > 0), y = has_reviews_2016)) + 
  geom_jitter() + 
  geom_smooth(method = "lm")

# 2016 (atleast 1 review) vs 2015(last_seen_month)
ggplot(listings_history, aes(x = has_reviews_2016, y = last_seen_month)) + 
  geom_count() + 
  geom_smooth(method = "lm")

# 2016(atleast 1 review) vs 2015(last_seen_review)
ggplot(listings_history, aes(x = last_review_2015, y = has_reviews_2016)) + 
  geom_count() + 
  geom_smooth(method = "lm")

ggplot(listings_history, aes(x = review_recency_2015_weeks, y = has_reviews_2016)) + 
  geom_count() + 
  geom_smooth(method = "lm")


# 2016(atleast 1 review) vs 2015(last_rating)
ggplot(listings_history, aes(x = last_rating, y = has_reviews_2016)) + 
  geom_count() + 
  geom_smooth(method = "lm")

# 2016(atleast 1 review) vs 2015(# reviews)
ggplot(listings_history, aes(x = num_reviews_in_2015, y = has_reviews_2016)) + 
  geom_count() + 
  geom_smooth(method = "lm")

# 2016(atleast 1 review) vs 2015(reviews > 0s)
ggplot(listings_history, aes(x = has_reviews_2015, y = has_reviews_2016)) + 
  geom_jitter() + 
  geom_smooth(method = "lm")


# 2016(atleast 1 review) vs 2015(total_occ)
ggplot(listings_history, aes(x =total_occ_2015, y = has_reviews_2016)) + 
  geom_count() + 
  geom_smooth(method = "lm")

# 2016(atleast 1 review) vs 2015(mean_price)
ggplot(listings_history, aes(x =mean_price, y = has_reviews_2016)) + 
  geom_count() + 
  geom_smooth(method = "lm")

# 2016(atleast 1 review) vs 2015(min_price)
ggplot(listings_history, aes(x =min_price, y = has_reviews_2016)) + 
  geom_count() + 
  geom_smooth(method = "lm")

# 2016(# reviews) vs 2015(max_price)
ggplot(listings_history, aes(x =max_price, y = has_reviews_2016)) + 
  geom_count() + 
  geom_smooth(method = "lm")




















# how many times a person appears in 2016 given the number of reviews they had in 2015  
ggplot(listings_history, aes(x = total_occ_2016, y = num_reviews_in_2015)) + geom_point() + geom_smooth(method = "lm")

# how many times a person appears in 2016 given the number of listings they had in 2015  
listings_history %>% filter(host_listings_count < 20) %>% ggplot(aes(x = total_occ_2016, y = host_listings_count)) + geom_count() + geom_smooth(method = "lm")

cor(listings_history$num_reviews_2015, listings_history$total_occ_2015)

geom_coun

ggplot(listings_history, aes(x = total_occ_2015, y = num_reviews_in_2015)) + geom_point() 


####FREQUENCY####

#frequency of occurances 2015
ggplot(data = listings_history, aes(x = total_occ_2015)) + 
  geom_histogram() +
  xlab("Total Occurences in 2015") +
  ylab("Frequency") +
  ggtitle("Frequency of Occurrences by Listing for 2015") +
  ggsave(file = "../airbnb/figures/new_hist_occ_by_listing_2015.pdf")

#frequency of occurances 2016
ggplot(data = listings_history, aes(x = total_occ_2016)) + 
  geom_histogram() +
  xlab("Total Occurences in 2016") +
  ylab("Frequency") +
  ggtitle("Frequency of Occurrences by Listing for 2016") +
  ggsave(file = "../airbnb/figures/hist_occurances_by_listing_2015.pdf")








#number of listings a month










#########
##Recency Frequency


#first_seen_month vs num reviews 2015
ggplot(listings_history, aes(x = as.factor(first_seen_month), y = num_reviews_in_2015)) + geom_boxplot()

#last_seen_month vs num reviews 2015
ggplot(listings_history, aes(x = as.factor(last_seen_month), y = num_reviews_in_2015)) + geom_boxplot()

#listing_recency_2015_weeks vs num reviews 2015
#ggplot(listings_history, aes(x = as.factor(listing_recency_2015_weeks), y = num_reviews_in_2015)) + geom_boxplot()

#total occ vs num_reviews 2015
ggplot(listings_history, aes(x = as.factor(total_occ_2015), y = num_reviews_in_2015)) + geom_boxplot()

#last_seen_month vs num reviews 2015
ggplot(listings_history, aes(x = as.factor(is_superhost_count_2015), y = num_reviews_in_2015)) + geom_boxplot()

#last_seen_month vs num reviews 2015
ggplot(listings_history, aes(x = as.factor(first_review_year), y = num_reviews_in_2015)) + geom_boxplot()

#last_seen_month vs num reviews 2015
ggplot(listings_history, aes(x = as.factor(last_review_year), y = num_reviews_in_2015)) + geom_boxplot()

#last_seen_month vs num reviews 2015
ggplot(listings_history, aes(x = as.factor(first_review_month_2015), y = num_reviews_in_2015)) + geom_boxplot()

#last_seen_month vs num reviews 2015
ggplot(listings_history, aes(x = as.factor(last_review_month_2015), y = num_reviews_in_2015)) + geom_boxplot()





########2016#######
#first_seen_month vs num reviews 2016
ggplot(listings_history, aes(x = as.factor(first_seen_month), y = num_reviews_in_2016)) + geom_boxplot()

#last_seen_month vs num reviews 2016
ggplot(listings_history, aes(x = as.factor(last_seen_month), y = num_reviews_in_2016)) + geom_boxplot()

#total occ vs num_reviews 2016
ggplot(listings_history, aes(x = as.factor(total_occ_2015), y = num_reviews_in_2016)) + geom_boxplot()

#last_seen_month vs num reviews 2016
ggplot(listings_history, aes(x = as.factor(is_superhost_count_2015), y = num_reviews_in_2016)) + geom_boxplot()

#last_seen_month vs num reviews 2016
ggplot(listings_history, aes(x = as.factor(first_review_year), y = num_reviews_in_2016)) + geom_boxplot()

#last_seen_month vs num reviews 2016
ggplot(listings_history, aes(x = as.factor(last_review_year), y = num_reviews_in_2016)) + geom_boxplot()

#last_seen_month vs num reviews 2016
ggplot(listings_history, aes(x = as.factor(first_review_month_2015), y = num_reviews_in_2016)) + geom_boxplot()

#last_seen_month vs num reviews 2016
ggplot(listings_history, aes(x = as.factor(last_review_month_2015), y = num_reviews_in_2016)) + geom_boxplot()





#total occ 2015 vs num_reviews 2015
ggplot(listings_history, aes(x = as.factor(total_occ_2016), y = num_reviews_in_2015)) + geom_boxplot()


#first_seen_month vs num reviews 2016
ggplot() + geom_bar(listings_history, x = as.factor(total_occ_2016), color = "total_occ_2016") + geom_bar(listings_history, x = as.factor(total_occ_2015), color = "total_occ_2015")

#last_seen_month vs num reviews 2016
ggplot(listings_history, aes(x = as.factor(last_seen_month), y = num_reviews_in_2016)) + geom_boxplot(stat = )

#total occ vs num_reviews 2016
ggplot(listings_history, aes(x = as.factor(total_occ_2015), y = num_reviews_in_2016)) + geom_boxplot()

#last_seen_month vs num reviews 2016
ggplot(listings_history, aes(x = as.factor(is_superhost_count_2015), y = num_reviews_in_2016)) + geom_boxplot()

#last_seen_month vs num reviews 2016
ggplot(listings_history, aes(x = as.factor(first_review_year), y = num_reviews_in_2016)) + geom_boxplot()

#last_seen_month vs num reviews 2016
ggplot(listings_history, aes(x = as.factor(last_review_year), y = num_reviews_in_2016)) + geom_boxplot()

#last_seen_month vs num reviews 2016
ggplot(listings_history, aes(x = as.factor(first_review_month_2015), y = num_reviews_in_2016)) + geom_boxplot()

#last_seen_month vs num reviews 2016
ggplot(listings_history, aes(x = as.factor(last_review_month_2015), y = num_reviews_in_2016)) + geom_boxplot()

#now make your lovely plot
listings_history %>% filter(first_seen_month == 1) %>% ggplot(aes(total_occ_2015, fill = as.factor(total_occ_2016))) + geom_density(alpha = 0.2)

#now make your lovely plot
ggplot(listings_history, aes(total_occ_2016, fill = as.factor(is_superhost_2015))) + geom_density(alpha = 0.2)

#now make your lovely plot
ggplot(listings_history, aes(total_occ_2016, fill = as.factor(has_reviews_2015))) + geom_density(alpha = 0.2)

#now make your lovely plot
ggplot(listings_history, aes(total_occ_2016, fill = as.factor(has_reviews_2015))) + geom_density(alpha = 0.2)

#last_seen_month vs num reviews 2016
listings_history %>% filter(num_reviews_in_2015 < 50) %>% ggplot(aes(x = num_reviews_in_2015)) + geom_histogram()


ggplot(listings_history, aes(total_occ_2016, fill = as.factor(total_occ_2015))) + geom_histogram(alpha = 0.2, aes(y = ..density..), position = 'identity')


ggplot(listings_history, aes(x = num_reviews_in_2015, y = num_reviews_in_2016)) + geom_point()


ggplot(listings_history, aes(x =  as.factor(is_superhost_count_2015), y =num_reviews_in_2015, color = is_superhost_2015)) + geom_boxplot()


ggplot(listings_history, aes(clarity, fill=cut)) + geom_bar(position="dodge")

ggplot(listings_history, aes(x = as.factor(total_occ_2015), y = as.factor(total_occ_2016))) + geom_bar(stat = "identity")



##Reviews
#first_seen_month vs last rating
ggplot(listings_history, aes(x = as.factor(first_seen_month), y = last_rating)) + geom_boxplot()

#last_seen_month vs last rating
ggplot(listings_history, aes(x = as.factor(last_seen_month), y = last_rating)) + geom_boxplot()

#listing_recency_2015_weeks vs last rating
ggplot(listings_history, aes(x = as.factor(listing_recency_2015_weeks), y = last_rating)) + geom_boxplot()

#total occ vs last rating
ggplot(listings_history, aes(x = as.factor(total_occ_2015), y = last_rating)) + geom_boxplot()

#last_seen_month vs last rating
ggplot(listings_history, aes(x = as.factor(is_superhost_count_2015), y = last_rating)) + geom_boxplot()

#last_seen_month vs last rating
ggplot(listings_history, aes(x = as.factor(first_review_year), y = last_rating)) + geom_boxplot()

#last_seen_month vs last rating
ggplot(listings_history, aes(x = as.factor(last_review_year), y = last_rating)) + geom_boxplot()

#last_seen_month vs last rating
ggplot(listings_history, aes(x = as.factor(first_review_month_2015), y = last_rating)) + geom_boxplot()

#last_seen_month vs last rating
ggplot(listings_history, aes(x = as.factor(last_review_month_2015), y = last_rating)) + geom_boxplot()



#place feature
ggplot(listings_history, aes(x = as.factor(room_type), y = num_reviews_in_2015)) + geom_boxplot()

ggplot(listings_history, aes(x = as.factor(is_multilisting), y = num_reviews_in_2015)) + geom_boxplot()








##Against Predictors

#total occ 2015 vs num_reviews 2016
ggplot(listings_history, aes(x = as.factor(total_occ_2015), y = num_reviews_in_2016)) + geom_boxplot()


#total occ 2015 vs total occ 2016
ggplot(listings_history, aes(x = as.factor(total_occ_2015), y = total_occ_2016)) + geom_boxplot()

#last_seen_month vs num reviews 2015
ggplot(listings_history, aes(x = as.factor(is_superhost_count_2015), y = num_reviews_in_2016)) + geom_boxplot()


#last_seen_month vs num reviews 2015
ggplot(listings_history, aes(x = as.factor(is_superhost_count_2015), y = is_superhost_count_2016)) + geom_boxplot()

#last_seen_month vs num reviews 2015
ggplot(listings_history, aes(x = as.factor(last_review_year), y = num_reviews_in_2016)) + geom_boxplot()


#last_seen_month vs num reviews 2015
ggplot(listings_history, aes(x = as.factor(last_review_month_2015), y = num_reviews_in_2016)) + geom_boxplot()





#total_occ_2016
#first_seen_month vs num reviews 2015
ggplot(listings_history, aes(x = as.factor(first_seen_month), y = total_occ_2016)) + geom_boxplot()


#total occ vs num_reviews 2015
ggplot(listings_history, aes(x = as.factor(total_occ_2015), y = num_reviews_in_2015)) + geom_boxplot()

#last_seen_month vs num reviews 2015
ggplot(listings_history, aes(x = as.factor(is_superhost_count_2015), y = is_superhost_count_2016)) + geom_boxplot()

#last_seen_month vs num reviews 2015
ggplot(listings_history, aes(x = as.factor(first_review_year), y = num_reviews_in_2015)) + geom_boxplot()

#last_seen_month vs num reviews 2015
ggplot(listings_history, aes(x = as.factor(last_review_year), y = num_reviews_in_2015)) + geom_boxplot()

#last_seen_month vs num reviews 2015
ggplot(listings_history, aes(x = as.factor(first_review_month_2015), y = num_reviews_in_2015)) + geom_boxplot()

#last_seen_month vs num reviews 2015
listings_history %>% filter(exist_in_2016 == TRUE) %>% ggplot(aes(x = as.factor(last_review_month_2015), y = exist_in_2016)) + geom_bar(stat = "identity")











########################MAPS####################


#get shapefile for nyc neighborhood tabulation areas
r <- GET('http://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services/nynta/FeatureServer/0/query?where=1=1&outFields=*&outSR=4326&f=geojson')
nyc_neighborhoods <- readOGR(content(r,'text'), 'OGRGeoJSON', verbose = F)
nyc_neighborhoods_df <- tidy(nyc_neighborhoods)
nyc_map <- get_map(location = c(lon = -74.00, lat = 40.71), maptype = "terrain", zoom = 10)


names(all_listings)[names(all_listings) == 'id'] <- 'listing_id'
locations <- all_listings %>% select(listing_id, longitude, latitude) %>% distinct(listing_id, .keep_all = TRUE)

listings_history <- left_join(listings_history , locations, by="listing_id")


####get Crime rate by NTA ###
#convert crime long and lat to coordinates and convert them to projection of ny shapefile
listings_locations <- data.frame(listings_history, listing_id = listings_history$listing_id, 
                                 latitude=listings_history$latitude, longitude=listings_history$longitude, 
                                 mean_price = listings_history$mean_price, min_price=listings_history$min_price, 
                                 max_price =listings_history$max_price, rating =listings_history$last_rating,
                                 num_reviews= listings_history$num_as_of_2015,
                                 property_type = listings_history$property_type,
                                 room_type = listings_history$room_type)
listings_locations<- listings_locations[complete.cases(listings_locations),]
listings_locations_spdf <- listings_locations 
coordinates(listings_locations_spdf) <- ~longitude + latitude 
proj4string(listings_locations_spdf) <- proj4string(nyc_neighborhoods) 

#map long and lat of crimes to neighborhoods
listings_matches <- over(listings_locations_spdf, nyc_neighborhoods)
listings_locations <- cbind(listings_locations, listings_matches)


#get sum of crimes per neighborhood and join dataframes
listings_by_neighborhood <- listings_locations %>%
  group_by(NTACode) %>%
  summarize(avg_num_reviews = mean(num_reviews), avg_max_price = mean(max_price), avg_min_price = mean(min_price), 
            avg_rating = mean(rating),
            num_listings = n())

#create noise plot data
plot_listing_data <- tidy(nyc_neighborhoods, region="NTACode") %>%
  left_join(., listings_by_neighborhood, by=c("id"="NTACode"))

#map of noise complaints by neighborhood
ggmap(nyc_map) + 
  geom_polygon(data=plot_listing_data, aes(x=long, y=lat, group=group, fill= num_listings)) + 
  scale_fill_gradient(low = "blue", high = "red")

ggmap(nyc_map) + 
  geom_polygon(data=plot_listing_data, aes(x=long, y=lat, group=group, fill= avg_num_reviews)) + 
  scale_fill_gradient(low = "blue", high = "red")

ggmap(nyc_map) + 
  geom_polygon(data=plot_listing_data, aes(x=long, y=lat, group=group, fill= avg_max_price)) + 
  scale_fill_gradient(low = "blue", high = "red")

ggmap(nyc_map) + 
  geom_polygon(data=plot_listing_data, aes(x=long, y=lat, group=group, fill= avg_min_price)) + 
  scale_fill_gradient(low = "blue", high = "red")

ggmap(nyc_map) + 
  geom_polygon(data=plot_listing_data, aes(x=long, y=lat, group=group, fill= avg_rating)) + 
  scale_fill_gradient(low = "blue", high = "red")












####find NTA associated with each listing ###
#convert listings long and lat to coordinates and convert them to projection of ny shapefile
listings_locations <- data.frame(listing_id= all_listings_2015$id, latitude=all_listings_2015$latitude, longitude=all_listings_2015$longitude)
listings_locations<- listings_locations[complete.cases(listings_locations),]
listings_locations_spdf <- listings_locations
coordinates(listings_locations_spdf) <- ~longitude + latitude 
proj4string(listings_locations_spdf) <- proj4string(nyc_neighborhoods) 


#map long and lat of crimes to neighborhoods
listing_matches <- over(listings_locations_spdf, nyc_neighborhoods)
listings_locations <- cbind(listings_locations, listing_matches)


#select relevant columns from crimes and listings
crime_NTA <- select(crimes_by_neighborhood, NTACode, num_crimes, NTAName, Population, crime_rate)
listings_NTA <-  select(listings_locations, listing_id, NTACode)

#join crimes onto listings
crime_rate_by_listings <- left_join(listings_NTA, crime_NTA, by="NTACode")

#select relevant columns
listings_NTA_crime_rate <- select(crime_rate_by_listings, listing_id, crime_rate)

#write previous date_frame to a csv file
write_csv(listings_NTA_crime_rate, "../../raw_data/crime_and_listings.csv")












