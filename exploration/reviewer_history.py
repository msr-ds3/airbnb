# -*- coding: utf-8 -*-
"""
Created on Wed Jul 20 10:47:12 2016

Reviewer History

@author: Erica Ram
"""
import csv
import os


abspath = os.path.abspath(__file__)
dname = os.path.dirname(abspath)
os.chdir(dname)

locations = ['barcelona', 'chicago', 'london', 'new-york-city',
             'san-francisco', 'berlin', 'copenhagen', 'los-angeles',
             'paris', 'sydney']
             
list_of_reviews = []

for loc in locations:
    path = "../raw_data/%s-reviews.csv" % loc
    with open(path, 'rb') as csvfile:
        reader = csv.reader(csvfile, delimiter=',')
        
        for row in reader:
            list_of_reviews.append(row)
        
        
print list_of_reviews
