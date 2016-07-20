#To find out if there is a correlation between number of reviews and 
#the availabilty per month. 

#group the reviews by listing
reviews_2015_by_listing <- reviews_2015 %>% ungroup() %>% 
  group_by(listing_id)%>% 
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
box_plot_est <- est_blocked_cal_2 %>% 
  mutate(rounded_boxplot = as.factor(round(estimated_booked_year / 10) * 10))
ggplot(box_plot_est, aes(x = rounded_boxplot, y = est_review_days)) + 
  geom_boxplot() + xlab("Number of Blocked Out Days per Year") +
  ylab("Estimated Booked Days by Review") + 
  ggtitle("Relationship Between Reviews and Availability")

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