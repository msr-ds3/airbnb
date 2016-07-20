#This file looks at number of reviews in other cities and sees the overlap of 
#these reviews with NY

#find the number of reviews each reviewer makes
num_reviewers <- reviews %>% group_by(reviewer_id) %>% 
  summarize(num_reviews = n())
nrow(num_reviewers)
summary(num_reviewers$num_reviews)
#range from 1 to 73 reviews, mean 1.128 (median = 1)
#create a data frame where people reviewed more than once
mult_reviews <- num_reviewers %>% filter(num_reviews > 1)
summary(mult_reviews$num_reviews)
#ranged from 2 review to 73, mean 2.394 (median =2)
#see the distribution of counts
ggplot(data = num_reviewers, aes(x = num_reviews)) + geom_histogram()

#create a table to see the number of reviewers that make 1 review, 2, etc.
count_review <- num_reviewers %>% group_by(num_reviews) %>% 
  summarize(num_reviewers = n())
View(count_review)

count_mult_reviews <- mult_reviews %>% group_by(num_reviews) %>% 
  summarize(num_reviewers = n())
View(count_mult_reviews)
sum(count_mult_reviews$num_reviewers)
#39,390 reviewers are reviewing more than one time 
sum(count_mult_reviews$num_reviewers) / length(unique(reviews$reviewer_id))
#9.18% of all unique reviewers 
#number or unique reviewers is 428,521

ggplot(data = count_review, aes(x = num_reviews)) + geom_histogram()

#filter out the reviews for only 2015
reviews_2015 <-reviews %>% filter(date >= "2015-01-01" & date <= "2015-12-31")
#create a table of the number of reviews per reviewer in 2015
num_reviewers_2015 <- reviews_2015 %>% group_by(reviewer_id) %>% 
  summarize(num_reviews = n())
View(num_reviewers_2015)

#create a table to see the number of reviewers that make 1 review, 2, etc. 
#(in 2015)
count_review_2015 <- num_reviewers_2015 %>% group_by(num_reviews) %>% 
  summarize(num_reviewers = n())
View(count_review_2015)
#find the total number of reviewers in 2015
sum(count_review_2015$num_reviewers)
#189,342 reviewers in 2015
#find the number of reviewers that reviewed more than one time
count_mult_reviews_2015 <- count_review_2015 %>% filter(num_reviews > 1) %>% 
  group_by(num_reviews) %>% 
  summarize(num_reviewers = n())
View(count_mult_reviews_2015)
#find the number of reviewers who reviewed more than one time in 2015
sum(count_mult_reviews_2015$num_reviewers)
#16 reviewers are reviewing more than one time 
length(unique(reviews_2015$reviewer_id))


##filter out the reviews for only 2016
reviews_2016 <-reviews %>% filter(date >= "2016-01-01" & date <= "2016-12-31")
#create a table of the number of reviews per reviewer in 2015
num_reviewers_2016 <- reviews_2016 %>% group_by(reviewer_id) %>% 
  summarize(num_reviews = n())
View(num_reviewers_2016)

#create a table to see the number of reviewers that make 1 review, 2, etc. 
#(in 2016)
count_review_2016 <- num_reviewers_2016 %>% group_by(num_reviews) %>% 
  summarize(num_reviewers = n())
View(count_review_2016)
#find the total number of reviewers in 2016
sum(count_review_2016$num_reviewers)
#110698 reviewers in 2016
#find the number of reviewers that reviewed more than one time
count_mult_reviews_2016 <- count_review_2016 %>% filter(num_reviews > 1) %>% 
  group_by(num_reviews) %>% 
  summarize(num_reviewers = n())
View(count_mult_reviews_2016)
#find the number of reviewers who reviewed more than one time in 2015
sum(count_mult_reviews_2016$num_reviewers)
#16 reviewers are reviewing more than one time 


## load san francisco

sf_reviews <- read_csv("sf_reviews.csv", na='\\N')


#filter out the sf reviews for only 2015
sf_reviews_2015 <-sf_reviews %>% 
  filter(date >= "2015-01-01" & date <= "2015-12-31")
#create a table of the number of reviews per reviewer in 2015
sf_num_reviewers_2015 <- sf_reviews_2015 %>% group_by(reviewer_id) %>% 
  summarize(num_reviews = n())
View(sf_num_reviewers_2015)
#join sf with ny 2015
sf_ny_2015 <- inner_join(sf_num_reviewers_2015, num_reviewers_2015, 
                         by = "reviewer_id")
colnames(sf_ny_2015) <- c("reviewer_id", "num_reviews_sf", "num_reviews_ny")
nrow(sf_ny_2015)
#2459

## load LA
la_reviews <- read_csv("la_reviews.csv", na= '\\N')
#filter out the la reviews for only 2015
la_reviews_2015 <-la_reviews %>% 
  filter(date >= "2015-01-01" & date <= "2015-12-31")
#create a table of the number of reviews per reviewer in 2015
la_num_reviewers_2015 <- la_reviews_2015 %>% group_by(reviewer_id) %>% 
  summarize(num_reviews = n())
View(la_num_reviewers_2015)
#join la with ny 2015
la_ny_2015 <- inner_join(la_num_reviewers_2015, num_reviewers_2015, 
                         by = "reviewer_id")
colnames(la_ny_2015) <- c("reviewer_id", "num_reviews_la", "num_reviews_ny")
nrow(la_ny_2015)
#2459


#load Chicago

chi_reviews <- read_csv("chi_reviews.csv", na= '\\N')
#filter out the la reviews for only 2015
chi_reviews_2015 <-chi_reviews %>% 
  filter(date >= "2015-01-01" & date <= "2015-12-31")
#create a table of the number of reviews per reviewer in 2015
chi_num_reviewers_2015 <- chi_reviews_2015 %>% group_by(reviewer_id) %>% 
  summarize(num_reviews = n())
View(chi_num_reviewers_2015)
#join chi with ny 2015
chi_ny_2015 <- inner_join(chi_num_reviewers_2015, num_reviewers_2015, 
                          by = "reviewer_id")
colnames(chi_ny_2015) <- c("reviewer_id", "num_reviews_chi", "num_reviews_ny")
nrow(chi_ny_2015)
#1979

#load Austin
austin_reviews <- read_csv("austin_reviews.csv", na= '\\N')
#filter out the la reviews for only 2015
austin_reviews_2015 <-austin_reviews %>% 
  filter(date >= "2015-01-01" & date <= "2015-12-31")
#create a table of the number of reviews per reviewer in 2015
austin_num_reviewers_2015 <- austin_reviews_2015 %>% group_by(reviewer_id) %>% 
  summarize(num_reviews = n())
View(austin_num_reviewers_2015)
#join chi with ny 2015
austin_ny_2015 <- inner_join(austin_num_reviewers_2015, num_reviewers_2015, 
                             by = "reviewer_id")
colnames(austin_ny_2015) <- c("reviewer_id", "num_reviews_austin", 
                              "num_reviews_ny")
nrow(austin_ny_2015)
#985

#load NOLA
nola_reviews <- read_csv("nola_reviews.csv", na= '\\N')
#filter out the la reviews for only 2015
nola_reviews_2015 <-nola_reviews %>% 
  filter(date >= "2015-01-01" & date <= "2015-12-31")
#create a table of the number of reviews per reviewer in 2015
nola_num_reviewers_2015 <- nola_reviews_2015 %>% group_by(reviewer_id) %>% 
  summarize(num_reviews = n())
View(nola_num_reviewers_2015)
#join chi with ny 2015
nola_ny_2015 <- inner_join(nola_num_reviewers_2015, num_reviewers_2015, 
                           by = "reviewer_id")
colnames(nola_ny_2015) <- c("reviewer_id", "num_reviews_nola", "num_reviews_ny")
nrow(nola_ny_2015)
#1398


#table of all cities and # shared reviews with nyc
cities <- data.frame(c("San Francisco", "Los Angeles", "Chicago", "Austin", 
                       "New Orleans"), 
                     c(nrow(sf_ny_2015), nrow(la_ny_2015), nrow(chi_ny_2015), 
                       nrow(austin_ny_2015), nrow(nola_ny_2015)), 
                     c(length(unique(sf_reviews_2015$reviewer_id)), 
                       length(unique(la_reviews_2015$reviewer_id)), 
                       length(unique(chi_reviews_2015$reviewer_id)), 
                       length(unique(austin_reviews_2015$reviewer_id)), 
                       length(unique(nola_reviews_2015$reviewer_id))))
#rename the columns
colnames(cities) <- c("city", "num_reviewers_shared_with_nyc", 
                      "total_reviewers_in_city")
#add column to find percentage
cities$percentage <- (cities$num_reviewers_shared_with_nyc / 
                        cities$total_reviewers_in_city) * 100


#group the reviews by listing
reviews_2015_by_listing <- reviews_2015 %>% ungroup() %>% group_by(listing_id)%>% 
  summarize(total_reviews_2015 = n())
colnames(reviews_2015_by_listing) <- c("id", "total_reviews_2015")

View(listings1509)
listings_blocked_cal <- listings %>% mutate(blocked_days_30 = 
                                              30 - availability_30)
listings_blocked_cal2 <- listings_blocked_cal %>% 
  mutate(estimated_booked_year = 12 * blocked_days_30)

est_blocked_cal <- inner_join(reviews_2015_by_listing, listings_blocked_cal2, 
                              by = "id" )
View(est_blocked_cal)
est_blocked_cal_2 <- est_blocked_cal %>% 
  mutate(est_review_days = total_reviews_2015 * 5)


#to look at a boxplot of the data
#separate the data into factors
box_plot_est <- est_blocked_cal_2 %>% mutate(rounded_boxplot = 
                               as.factor(round(estimated_booked_year / 10) * 10))
ggplot(box_plot_est, aes(x = rounded_boxplot, y = est_review_days)) + 
  geom_boxplot()

#look at different types of listings
#Entire apartments
est_blocked_cal_entire <- est_blocked_cal_2 %>% 
  filter(room_type == "Entire home/apt")
ggplot(est_blocked_cal_entire, aes(x = estimated_booked_year, 
                                   y = est_review_days)) + geom_point()

cor(est_blocked_cal_entire$estimated_booked_year, 
    est_blocked_cal_entire$est_review_days)
#-0.014

#Private rooms
est_blocked_cal_room <- est_blocked_cal_2 %>% 
  filter(room_type == "Private room")
ggplot(est_blocked_cal_room, aes(x = estimated_booked_year, 
                                   y = est_review_days)) + geom_point()

cor(est_blocked_cal_room$estimated_booked_year, 
    est_blocked_cal_room$est_review_days)
#0.011187645

#Shared rooms
est_blocked_cal_shared <- est_blocked_cal_2 %>% 
  filter(room_type == "Shared room")
ggplot(est_blocked_cal_shared, aes(x = estimated_booked_year, 
                                 y = est_review_days)) + geom_point()

cor(est_blocked_cal_shared$estimated_booked_year, 
    est_blocked_cal_shared$est_review_days)
#0.06545039

##listings with 90 days
listings_blocked_cal <- listings %>% mutate(blocked_days_90 = 
                                              90 - availability_30)
listings_blocked_cal2 <- listings_blocked_cal %>% 
  mutate(estimated_booked_year_90 = 4 * blocked_days_90)

est_blocked_cal <- inner_join(reviews_2015_by_listing, listings_blocked_cal2, 
                              by = "id" )
View(est_blocked_cal)
est_blocked_cal_2 <- est_blocked_cal %>% 
  mutate(est_review_days = total_reviews_2015 * 5)
ggplot(est_blocked_cal_2, aes(x = estimated_booked_year_90, 
                                   y = est_review_days)) + geom_point()

cor(est_blocked_cal_2$estimated_booked_year_90, 
    est_blocked_cal_2$est_review_days)
#-0.006