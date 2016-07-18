#!/bin/bash
# 
# description:
#   fetches airbnb listings data from insideairbnb.com from September 2015 to June 2016
#
# usage: ./pull_old_listings.sh


dates=("2015-01-01" "2015-03-01" "2015-04-01" "2015-05-01" "2015-06-01" "2015-08-01" "2015-09-01" "2015-10-01" "2015-11-01" "2015-11-20" "2015-12-02" "2016-01-01" "2016-02-02" "2016-04-03" "2016-05-02" "2016-06-02") 

for d in ${dates[@]}
do
  wget "http://data.insideairbnb.com/united-states/ny/new-york-city/"$d"/data/listings.csv.gz"
  gzip -d listings.csv.gz
  mv listings.csv $d-listings.csv
done
