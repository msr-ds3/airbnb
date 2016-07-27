library(readr)

# prepping the data
# load US review data from create_reviewer_history run
rev_data <- read_csv("../raw_data/us_reviewer_data.csv")

# rename columns (change from generic to specific for 2015 time period)
names(rev_data)[names(rev_data) == "first_within_time_period"] <- "first_in_2015"
names(rev_data)[names(rev_data) == "first_month_within_time_period"] <- "first_month_in_2015"
names(rev_data)[names(rev_data) == "last_within_time_period"] <- "last_in_2015"
names(rev_data)[names(rev_data) == "last_month_within_time_period"] <- "last_month_in_2015"
names(rev_data)[names(rev_data) == "num_within_time_period"] <- "num_in_2015"
names(rev_data)[names(rev_data) == "text_within_time_period"] <- "text_in_2015"
names(rev_data)[names(rev_data) == "locations_within_time_period"] <- "locations_in_2015"

# mutate to create logical column of whether there is a review in 2016 or not
rev_data <- mutate(rev_data, has_review_2016 = num_in_2016 > 0)

# filtering out NAs
rev_data_filtered <- filter(rev_data, !is.na(text_in_2015))

# write to csv
write_csv(rev_data, "../raw_data/us_rev_data.csv")


# modeling the data
# read the csv
rev_data <- read_csv("../raw_data/us_rev_data.csv")

# logistic regression model for whether there is a review or not in 2016 based
# on when the last review in 2015 was and the number of reviews in 2015
model1 <- glm(has_review_2016 ~ last_in_2015 + num_in_2015, data = rev_data,
              family = "binomial")

summary(model1)
# Call:
#   glm(formula = has_review_2016 ~ last_in_2015 + num_in_2015, family = "binomial", 
#       data = rev_data)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -5.5374  -0.5407  -0.5099  -0.4723   2.1784  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  -2.460e+01  4.298e-01  -57.24   <2e-16 ***
#   last_in_2015  1.335e-03  2.581e-05   51.74   <2e-16 ***
#   num_in_2015   3.779e-01  2.093e-03  180.55   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 1280259  on 1569094  degrees of freedom
# Residual deviance: 1241778  on 1569092  degrees of freedom
# (1432362 observations deleted due to missingness)
# AIC: 1241784
# 
# Number of Fisher Scoring iterations: 4

# !!! ^ roughly half were removed due to missingness

model2 <- lm(num_in_2016 ~ last_month_in_2015 + num_in_2015, data = rev_data)
# Call:
#   lm(formula = num_in_2016 ~ last_month_in_2015 + num_in_2015, 
#      data = rev_data)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -7.997 -0.177 -0.148 -0.109 37.419 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        -0.1001320  0.0015120  -66.22   <2e-16 ***
#   last_month_in_2015  0.0096282  0.0001684   57.17   <2e-16 ***
#   num_in_2015         0.1710568  0.0005331  320.88   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.6371 on 1569092 degrees of freedom
# (1432362 observations deleted due to missingness)
# Multiple R-squared:  0.06518,	Adjusted R-squared:  0.06518 
# F-statistic: 5.47e+04 on 2 and 1569092 DF,  p-value: < 2.2e-16

# !!! ^ roughly half were removed due to missingness

# decision tree(s)
library(rpart)
library(rpart.plot)

set.seed(123)
indexes <- sample(1:nrow(rev_data), size=0.2*nrow(rev_data))
rev_test=rev_data[indexes, ]
rev_train=rev_data[-indexes, ]

# modeling for num in 2016
fit1 <- rpart(num_in_2016 ~ last_month_in_2015 + num_in_2015,
             data = rev_train, method = "anova")

plot(fit1)
text(fit1)
rpart.plot(fit1)
printcp(fit1)
# Regression tree:
#   rpart(formula = num_in_2016 ~ last_month_in_2015 + num_in_2015, 
#         data = rev_train, method = "anova")
# 
# Variables actually used in tree construction:
#   [1] num_in_2015
# 
# Root node error: 547787/1255464 = 0.43632
# 
# n=1255464 (1145702 observations deleted due to missingness)
# 
# CP nsplit rel error  xerror      xstd
# 1 0.033877      0   1.00000 1.00000 0.0103437
# 2 0.013978      1   0.96612 0.96613 0.0099477
# 3 0.010000      2   0.95215 0.95281 0.0094415

bestcp1 <- fit1$cptable[which.min(fit1$cptable[,"xerror"]), "CP"]

#prune tree using best cp
tree_pruned1 <- prune(fit1, cp = bestcp)

plot(tree_pruned1, uniform = TRUE)
text(tree_pruned1, cex = 0.8, use.n = TRUE, xpd = TRUE)

prp(tree_pruned1, faclen = 0, cex = 0.8, extra = 1)

tot_count <- function(x, labs, digits, varlen)
{
  paste(labs, "\n\nn =", x$frame$n)
}

prp(tree_pruned1, faclen = 0, cex = 0.8, node.fun=tot_count)

#use predict to see the prediction
sample_predict1 <- predict(tree_pruned1, rev_train)

ROCR1 <- prediction(sample_predict1, rev_train$num_in_2015) # error
# comparing to itself for now, error rev_test is shorter than rev_train
roc.perf1 = performance(ROCR1, measure = "tpr", x.measure = "fpr")
plot(roc.perf1)

# with every feature, still only uses num_in_2015
fit1a <- rpart(num_in_2016 ~ first_month + first_diff_2015 + last_month + 
                 last_diff_2015 + first_in_2015 + first_month_in_2015 + 
                 last_in_2015 + last_month_in_2015 + num_in_2015,
              data = rev_train, method = "anova")

rpart.plot(fit1a)
plot(fit1a)
text(fit1a)

# modeling for has review 2016
fit2 <- rpart(has_review_2016 ~ last_month_in_2015 + num_in_2015 + last_diff_2015,
             data = rev_train, control = rpart.control(maxdepth = 5))

plot(fit2)
text(fit2)
rpart.plot(fit2)
printcp(fit2)
# Regression tree:
#   rpart(formula = has_review_2016 ~ last_month_in_2015 + num_in_2015 + 
#           last_diff_2015, data = rev_train, control = rpart.control(maxdepth = 5))
# 
# Variables actually used in tree construction:
#   [1] num_in_2015
# 
# Root node error: 190387/1815250 = 0.10488
# 
# n=1815250 (585916 observations deleted due to missingness)
# 
# CP nsplit rel error  xerror      xstd
# 1 0.026848      0   1.00000 1.00000 0.0017461
# 2 0.010000      1   0.97315 0.97315 0.0016852

bestcp2 <- fit2$cptable[which.min(fit2$cptable[,"xerror"]), "CP"]

#prune tree using best cp
tree_pruned2 <- prune(fit2, cp = bestcp)

plot(tree_pruned2, uniform = TRUE)
text(tree_pruned2, cex = 0.8, use.n = TRUE, xpd = TRUE)

prp(tree_pruned2, faclen = 0, cex = 0.8, extra = 1)

tot_count <- function(x, labs, digits, varlen) {
  paste(labs, "\n\nn =", x$frame$n)
}

prp(tree_pruned2, faclen = 0, cex = 0.8, node.fun=tot_count)

#use predict to see the prediction
sample_predict2 <- predict(tree_pruned2, rev_train)

ROCR2 <- prediction(sample_predict2, rev_test$has_review_2016) 
# comparing to itself for now, error rev_test is shorter than rev_train
roc.perf2 = performance(ROCR2, measure = "tpr", x.measure = "fpr")
performance(ROCR2, measure = "auc")
plot(roc.perf2)
