library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(scales)
library(rpart)
library(rpart.plot)


############################################################# [ data prep ]
listings_history <- read_csv("../raw_data/listing_history.csv")
View(listings_history)

#listings_history[78] <- NULL #deleting mulltiple listing_id column in column 78. Don't need this if Kaciny fixes.

# create test and train set
set.seed(123)
indexes <- sample(1:nrow(listings_history), 
                  size=0.2*nrow(listings_history))
listings_history_test=listings_history[indexes, ]
listings_history_train=listings_history[-indexes, ]


############################################################# [ exists_in_2016 ~ recency frequency (RF) ]
# model
tree_rf <- rpart(exist_in_2016 ~ host_listings_count + host_duration + 
                   first_seen_month + last_seen_month + 
                   listing_recency_2015_weeks + scrap_duration + 
                   total_occ_2015 + review_recency_2015_weeks + 
                   is_superhost_2015 + is_superhost_count_2015, 
                 data = listings_history_train, 
                 control = rpart.control(maxdepth = 5))

summary(listings_history$exist_in_2016)

printcp(tree_rf) #summary
bestcp_rf <- tree_rf$cptable[which.min(tree_rf$cptable[,"xerror"]), "CP"]

# prune tree using best cp
tree_pruned_rf <- prune(tree_rf, cp = bestcp_rf)

# plots
plot(tree_pruned_rf, uniform = TRUE)
text(tree_pruned_rf, cex = 0.8, use.n = TRUE, xpd = TRUE)
prp(tree_pruned_rf, faclen = 0, cex = 0.8, extra = 1)
