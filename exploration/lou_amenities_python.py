
#f = open('amenities_and_id.tsv', 'r+')

grand_amenities = open('unique_amenities.tsv').readlines()
list_of_amenities = []
for one_row in grand_amenities[1:len(grand_amenities)]:
    nrow, item = one_row.strip().replace('\"', '').split('\t')
    list_of_amenities.append(item)

#print(list_of_amenities) #prints out all unique amenities in airbnb
#print(len(list_of_amenities)) #43

with open('amenities_and_id.tsv') as f:
    listings = f.readlines() #returns the rownum, id, list of amenities for each line


grand_result = []
amenities_string = "id," + ",".join(list_of_amenities)
temp_arr = amenities_string.split(',')
amenities_string = ",".join(temp_arr)
print(amenities_string)


for listing in listings[1:20]:
    rownum, id, amenities = listing.split('\t')
    listings_amenities = amenities.replace('{', '').replace('}', '').replace('\\', '').replace('\"', '').strip().split(',') #strip \n and split it by comma and backslash
    #refactor replacements of {, }, \, "

    #entering each amenity item within a listing
    true_false_row = [id] #start off with listing ID at the front
    for i in range(0,len(list_of_amenities)): #run for the length of total list of amenities i.e. 43
        item = list_of_amenities[i]
        if item in listings_amenities: #check to see if is contained
            #print(item)
            #print("T")
            #print(i) #tells 'position' of the amenity
            true_false_row.append("TRUE")
        else:
            true_false_row.append("FALSE")
    #print(len(true_false_row))

    row_string = ",".join(true_false_row)
    print(row_string)

    #grand_result.append(true_false_row)
#print("========================== length of list_of_amenitites:")
#print(len(list_of_amenities))
#print(grand_result)


    #print(amenities)
    #print(len(amenities))


#f = open('amenities_and_id.tsv', 'r+')

#lines = f.read().split('\n')
#print len(lines)
#f.close()
#i = 0

#for line in lines:
 # if(i < 2):
 #   print(line)
 # i = i + 1

#['"36608"\t6614094\t"{\\"Wireless Internet\\",\\"Air Conditioning\\",\\"Pets live on this property\\",Dog(s),Heating}"\n'].split(')
#36609

#distinct rows with non-empty summaries filter(listings, summary != "") %>% distinct(summart) %>% nrow() #33436
#sapply(listing$summary[1:5], nchar)
