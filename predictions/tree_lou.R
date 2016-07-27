library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(scales)
library(rpart)
library(rpart.plot)

listings_history <- read_csv("../raw_data/listing_history.csv")
View(listings_history)

#############################################################
# [ small subsetting ]
#listings_history[78] <- NULL #deleting mulltiple listing_id column in column 78. Don't need this if Kaciny fixes.

#select small subset
View(listings_history)
subset <- listings_history %>% select(review_recency_2015_weeks, last_review_month_2015, num_reviews_2015, total_occ_2015, total_occ_2016)

# add T/F if listings is listed for at least one month
subset <-  subset %>% mutate(listed_at_least_one_month = (total_occ_2016 > 0))
subset$listed_at_least_one_month[is.na(subset$listed_at_least_one_month)] <- FALSE # if there are NA occurances, it means the listings is gone, and has 0 ocurrances, thus total_occ is FALSE

# remove total_occ_2016
subset$total_occ_2016 <- NULL

View(subset)

#############################################################
# [ big subsetting ]

#select small subset
View(bigset)
bigset <- listings_history[3:34, c(94)]#stops at amenities

# add T/F if listings is listed for at least one month
bigset <-  listings_history %>% mutate(listed_at_least_one_month = (total_occ_2016 > 0))
bigset$listed_at_least_one_month[is.na(bigset$listed_at_least_one_month)] <- FALSE # if there are NA occurances, it means the listings is gone, and has 0 ocurrances, thus total_occ is FALSE

# remove total_occ_2016
bigset$total_occ_2016 <- NULL

View(bigset)

############################################################## [ creating tree ]

# 1. Begin with a small complexity parameter (0.0001)
set.seed(123)
tree <- rpart(listed_at_least_one_month ~ ., data = bigset,  control = rpart.control(maxdepth = 5))

# 2. Pick tree size that minimizes misclassification
printcp(tree)
bestcp <- tree$cptable[which.min(tree$cptable[,"xerror"]), "CP"]

# 4. Prune the tree using the best cp
tree.pruned <- prune(tree, cp = bestcp)
printcp(tree)

# confusion matrix (training data)
conf.matrix <- table(bigset$listed_at_least_one_month, predict(tree.pruned))
rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix), sep=":")
colnames(conf.matrix) <- paste("Pred", colnames(conf.matrix), sep=":")
print(conf.matrix)

# plotting
tot_count <- function(x, labs, digits, varlen){
  paste(labs, "\n\nn =", x$frame$n)
}

prp(tree.pruned, faclen = 0, cex = 0.8, node.fun=tot_count)