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

