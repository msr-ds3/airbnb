# !/bin/bash
# description:
#   fetches airbnb reviews data from insideairbnb.com from 
#   Paris, New York, London, Barcelona, Berlin, Los Angeles,
#   Copenhagen, Sydney, San Francisco, and Chicago
#
# usage: ./pull_other_reviews.sh

params=("france/ile-de-france/paris/2016-07-03" "united-states/ny/new-york-city/2016-07-02" 
	"united-kingdom/england/london/2016-06-02" "spain/catalonia/barcelona/2016-01-03"
	"germany/be/berlin/2015-10-03" "united-states/ca/los-angeles/2016-05-02"
	"denmark/hovedstaden/copenhagen/2016-06-28" "australia/nsw/sydney/2016-01-03"
	"united-states/ca/san-francisco/2016-07-02" "united-states/il/chicago/2015-10-03")

for p in ${params[@]}
do
  IFS='/' read -r -a array <<< "$p";
  wget "http://data.insideairbnb.com/"$p"/data/reviews.csv.gz"
  gzip -d reviews.csv.gz
  mv reviews.csv ${array[2]}-cities-reviews.csv
done

