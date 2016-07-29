library(readr)
library(rpart)
library(rpart.plot)
library(ROCR)
library(dplyr)

set.seed(123)

# expands nums from abbreviated to actual numbers
tot_count <- function(x, labs, digits, varlen)
{
  paste(labs, "\n\nn =", x$frame$n)
}

df <- read_csv("../raw_data/reviews_and_listings.csv")
test <- sample_n(df, size=0.2*nrow(df))
train <- filter(df, !df$review_id %in% test$review_id)

fit <- rpart(exist_in_2016 ~                                 
             # property_type + room_type +
             # mean_price + min_price +
             # max_price + is_multilisting +
             # host_since + host_listings_count +
             # host_duration + first_seen +
             # last_seen + first_seen_month +
             # last_seen_month + listing_recency_2015_weeks +
             # scrap_duration + 
             total_occ_2015 +
             first_review + first_review_year +
             last_review + last_review_year +
             num_as_of_2015 + num_reviews_in_2015 +
             has_reviews_2015 + first_review_2015,
             # first_review_month_2015 + last_review_2015 +
             # last_review_month_2015 + review_recency_2015_weeks +
             # last_rating + is_superhost_2015 +
             # is_superhost_count_2015 +
             # TV +                                        
             # Internet + Wireless.Internet +                         
             # Air.Conditioning + Kitchen +                                   
             # Heating + Family.Kid.Friendly +                       
             # Smoke.Detector + Carbon.Monoxide.Detector +                  
             # Essentials + Shampoo +                                   
             # Cable.TV + Free.Parking.on.Premises +                  
             # Breakfast + Pets.live.on.this.property +                
             # Dog.s. + First.Aid.Kit +                             
             # Buzzer.Wireless.Intercom + Washer +                                    
             # Dryer + Pets.Allowed +                              
             # Gym + Safety.Card +                               
             # Fire.Extinguisher + Wheelchair.Accessible +                     
             # Cat.s. + Indoor.Fireplace +                          
             # Suitable.for.Events + Doorman +                                   
             # Hot.Tub + Elevator.in.Building +                      
             # Pool + Smoking.Allowed +                           
             # Other.pet.s. + Washer...Dryer +                            
             # Lock.on.Bedroom.Door + X24.Hour.Check.in +                         
             # Hangers + Hair.Dryer +                                
             # Iron + Laptop.Friendly.Workspace +                 
             # translation.missing..en.hosting_amenity_49 + translation.missing..en.hosting_amenity_50,
             # email + phone +                                     
             # facebook + reviews +                                   
             # kba + google +                                    
             # jumio +                                   
             # linkedin + manual_offline +                            
             # manual_online + weibo +                                     
             # photographer + None +                                      
             # amex + verifications_count,
              data = train, control = rpart.control(cp = 0.01))

plot(fit)
text(fit)
rpart.plot(fit)
printcp(fit)

bestcp <- fit$cptable[which.min(fit$cptable[,"xerror"]), "CP"]

#prune tree using best cp
tree_pruned <- prune(fit, cp = bestcp)

plot(tree_pruned, uniform = TRUE)
text(tree_pruned, cex = 0.8, use.n = TRUE, xpd = TRUE)

prp(tree_pruned, faclen = 0, cex = 0.8, extra = 1)
prp(tree_pruned, faclen = 0, cex = 0.8, node.fun=tot_count)

#use predict to see the prediction
sample_predict <- predict(tree_pruned, rev_test)

ROCR <- prediction(sample_predict, rev_test$has_review_2016) 
# comparing to itself for now, error rev_test is shorter than rev_train
roc.perf = performance(ROCR, measure = "tpr", x.measure = "fpr")
performance(ROCR, measure = "auc")
plot(roc.perf)
############################ comes back with nothing every time. NAs? :/