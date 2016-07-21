#To plot number of MultiListings per Host separated by Borough

library(ggplot2)

#To find number of listings for each host and then isolate just the multilistings
multilistings <- listings %>% filter(room_type == "Entire home/apt") %>% 
  group_by(host_id) %>% mutate(host_count = n()) %>% filter(host_count > 1) %>% 
  arrange(host_id)

#to plot and save as a pdf

ggplot(data = multilistings, aes(x = host_count)) + geom_histogram() +
  facet_wrap(~neighbourhood_group_cleansed) + 
  ggtitle("Number of Multi-Listings per Host") + 
  xlab("Number of Multi-Listings per Host") +
  ggsave(file = "../airbnb/figures/num_multilistings_per_host_by_boro.pdf")

