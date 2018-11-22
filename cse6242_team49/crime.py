# get a application token from https://data.cityofchicago.org/profile/app_tokens
import json
import sys
import csv
import time
import requests
from uszipcode import Zipcode
from uszipcode import SearchEngine

# sys argument
api_key = sys.argv[1]
# Make a get request to get the crime data
url="https://data.cityofchicago.org/resource/6zsd-86xi.json?$$app_token="+str(api_key)+"&$where=year >= 2009&$order=year DESC&$limit=7000000"
response = requests.get(url)

# convert to json string to python object
data = json.loads(response.content.decode("utf-8")) 

# extract useful info from returned data
def extractCrime():
    # extract id, iucr, primary_type, date, year, arrest, latitude, longitude
    crime_list = []
    total = 0
    crime_results = data
    for crime in crime_results:
        crime['id']=crime.get('id')  #if id is missing, return id=None
        crime['iucr']=crime.get('iucr')
        crime['primary_type']=crime.get('primary_type')
        crime['date']=crime.get('date')
        crime['arrest']=crime.get('arrest')
        crime['latitude']=crime.get('latitude')
        crime['longitude']=crime.get('longitude')
        crime_list.append([crime['id'],crime['iucr'],crime['primary_type'],crime['date'],crime['arrest'] ,crime['latitude'],crime['longitude']])
        total += 1
    return crime_list,total

print(extractCrime()[1])

# Search zipcode within 10 miles, ordered from closest to farthest - return nearest 1
# append the corresponding zipcode to the crime
search = SearchEngine(simple_zipcode=True)
res_list = []
for crime in extractCrime()[0]:
    if crime[5] is not None:
        res= search.by_coordinates(float(crime[5]), float(crime[6]), radius=10, returns=1)
        crime.append(res[0].to_dict()['zipcode'])
        res_list.append(crime)
    else:
        crime.append(None)
        res_list.append(crime)

# export
with open('res_list.csv', 'w', newline = '') as res_list_csv:
    wr = csv.writer(res_list_csv, dialect = csv.excel, quoting = csv.QUOTE_MINIMAL)
    wr.writerows(res_list)
