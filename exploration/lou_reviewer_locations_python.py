
file_name = 'unique_locations_grand.tsv' #change these if you want to work on different files
skinny_table = 'locations_and_reviewer_grand.tsv'

grand_locations = open(file_name).readlines()

#creates list of unique locations
list_of_locations = []
for one_row in grand_locations[1:len(grand_locations)]:
    nrow, item = one_row.strip().replace('\"', '').replace(', ', '_').replace(" ", "_").split('\t') #can remove second replace
    list_of_locations.append(item)


with open(skinny_table) as f:
    reviews = f.readlines() #returns the rownum, reviewer_id, list of locations for each line

grand_result = []
locations_string = "id," + ",".join(list_of_locations) # header of date frame
temp_arr = locations_string.split(',')
locations_string = ",".join(temp_arr)
print(locations_string) # <<<<< need this for output



for review in reviews[1:len(reviews)]:

    rownum, reviewer_id, locations = review.split('\t')

    reviews_locations = locations.strip().replace(", ", "_").replace("\'", '').replace('\"' , '').split("|")
    # entering each verification item within a review
    true_false_row = [reviewer_id]  # start off with review ID at the front

    for i in range(0, len(list_of_locations)):  # run for the length of total list of locations i.e. 385
        location = list_of_locations[i]

        if location in reviews_locations:  # check to see if is contained
            #print(location)
            #print(reviews_locations)
            # print(i) #tells 'position' of the location
            true_false_row.append("TRUE")
        else:
            true_false_row.append("FALSE")

    row_string = ",".join(true_false_row)
    print(row_string) #<<< need this

#if "TRUE" in row_string:
#    print("\nin there")

# python lou_amenities_python.py > table_amenities_grand.csv