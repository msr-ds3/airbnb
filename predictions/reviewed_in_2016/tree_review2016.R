#to create and generate the tree for if there was a review or not in 2016
#for all listings and generate the ROC curve

library(rpart)
library(rpart.plot)
library(readr)
library(dplyr)
library(ROCR)

#load the test and train data
load("test_train.RData")

set.seed(123)

#using just recency frequency data
