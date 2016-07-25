
#f = open('verifications_and_id.tsv', 'r+')

file_name = 'unique_verifications_grand.tsv' #change these if you want to work on different files
skinny_table = 'verifications_and_id_grand.tsv'


grand_verifications = open(file_name).readlines() #reads list of unique verifications

list_of_verifications = []
for one_row in grand_verifications[1:len(grand_verifications)]:
    nrow, item = one_row.strip().replace('\"', '').replace(' ', '').split('\t')
    list_of_verifications.append(item)

#print(list_of_verifications) #prints out all unique verifications in airbnb
#print(len(list_of_verifications))

with open(skinny_table) as f:
    listings = f.readlines() #returns the rownum, id, list of verifications for each line

grand_result = []
verifications_string = "id," + ",".join(list_of_verifications)
temp_arr = verifications_string.split(',')
verifications_string = ",".join(temp_arr)
print verifications_string # <<<<< need this for output

for listing in listings[1:len(listings)]:
    #print(listing)
    rownum, id, verifications, scrape_date = listing.split('\t')
    listings_verifications = verifications.replace('[', '').replace(']', '').replace('\\', '').replace('\"', '').strip().split(',') #strip \n and split it by comma and backslash
    #to do: refactor replacements of [, ], \, "

    #entering each verification item within a listing
    true_false_row = [id] #start off with listing ID at the front
    for i in range(0,len(list_of_verifications)): #run for the length of total list of verifications i.e. 43
        item = list_of_verifications[i]
        if item in listings_verifications: #check to see if is contained
            #print(item)
            #print("T")
            #print(i) #tells 'position' of the verification
            true_false_row.append("TRUE")
        else:
            true_false_row.append("FALSE")
    #print(len(true_false_row))

    row_string = ",".join(true_false_row)
    print(row_string)
