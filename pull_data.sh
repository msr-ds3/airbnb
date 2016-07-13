#!/bin/bash
# 
# description:
#   fetches airbnb data from insideairbnb.com (currently pulls listings, calendar, and reviews
#   data from June 2016
#
# usage: ./pull_data.sh
#

wget http://data.insideairbnb.com/united-states/ny/new-york-city/2016-06-02/data/listings.csv.gz

wget http://data.insideairbnb.com/united-states/ny/new-york-city/2016-06-02/data/calendar.csv.gz

wget http://data.insideairbnb.com/united-states/ny/new-york-city/2016-06-02/data/reviews.csv.gz

wget http://data.insideairbnb.com/united-states/ny/new-york-city/2016-06-02/visualisations/neighbourhoods.csv

for name in *.gz
do
  gzip -d "$name"
done


