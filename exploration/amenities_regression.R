# DS3, AirBnB Analysis Team
# 2016-07-27

# This file adds additional features to the listings_history file. The goal is 
# to use some of this features in our prediction of future renter / host 
# behavior.

library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(scales)


###############################################################################
## Step 1: Load in data
###############################################################################

listings_history <- read.csv("../raw_data/listing_history.csv")



###############################################################################
## Step ?: Messing around with the data.
###############################################################################

amenities <- listings_history %>% 
  select_("mean_price", "room_type",
          "TV", "Internet", "Wireless.Internet", 
          "Air.Conditioning", "Kitchen", "Heating", "Family.Kid.Friendly", 
          "Smoke.Detector", "Carbon.Monoxide.Detector", "Essentials", "Shampoo",
          "Cable.TV", "Free.Parking.on.Premises",  "Breakfast", 
          "Pets.live.on.this.property", "Dog.s.", "First.Aid.Kit", 
          "Buzzer.Wireless.Intercom",  "Washer", "Dryer", "Pets.Allowed", "Gym",
          "Safety.Card", "Fire.Extinguisher", "Wheelchair.Accessible",  
          "Cat.s.", "Indoor.Fireplace", "Suitable.for.Events", "Doorman", 
          "Hot.Tub", "Elevator.in.Building", "Pool",  "Smoking.Allowed", 
          "Other.pet.s.", "Washer...Dryer", "Lock.on.Bedroom.Door", 
          "X24.Hour.Check.in", "Hangers",  "Hair.Dryer", "Iron", 
          "Laptop.Friendly.Workspace")

amenities_fit <- amenities %>% 
  filter(room_type == "Entire home/apt") %>%
  filter(mean_price < 300) %>% 
  select(-room_type) %>%
  lm(mean_price ~ ., data=.)

summary(amenities_fit)
