import csv
from uszipcode import Zipcode
from uszipcode import SearchEngine
from geopy.distance import geodesic

# read a data file with zipcode
zipcode = ['60644','60636','60653','60615','60621','60609','60628','60827','60620','60617',
'60608','60651','60649','60647','60619','60640','60637','60641','60616','60623',
'60624','60618','60638','60660','60632','60614','60639','60610','60625','60652',
'12311','60622','60629','60626','60642','60643','60659','60633','60646','60612',
'6761','60654','60634','60657','60630','60707','60631','60613','60645','60655',
'60656']

# find lat/lang for each zipcode
zipcode_latlng = []
search = SearchEngine(simple_zipcode=True) # set simple_zipcode=False to use rich info database
for code in zipcode:
    if code != '12311' and code != '6761':
        lat = search.by_zipcode(code).lat
        lng = search.by_zipcode(code).lng
        zipcode_latlng.append([code,lat,lng,zipcode.index(code)])
    elif code == '12311':
        lat = round((search.by_zipcode(60601).lat + search.by_zipcode(60602).lat + search.by_zipcode(60603).lat + search.by_zipcode(60604).lat + search.by_zipcode(60605).lat + search.by_zipcode(60611).lat)/6,2)
        lng = round((search.by_zipcode(60601).lng + search.by_zipcode(60602).lng + search.by_zipcode(60603).lng + search.by_zipcode(60604).lng + search.by_zipcode(60605).lng + search.by_zipcode(60611).lng)/6,2)
        zipcode_latlng.append([code,lat,lng,zipcode.index(code)])
    else:
        lat = round((search.by_zipcode(60606).lat + search.by_zipcode(60607).lat + search.by_zipcode(60661).lat)/3,2)
        lng = round((search.by_zipcode(60606).lng + search.by_zipcode(60607).lng + search.by_zipcode(60661).lng)/3,2)
        zipcode_latlng.append([code,lat,lng,zipcode.index(code)])

# calculnge distances between zipcodes
distance = []
for record1 in zipcode_latlng:
    for record2 in zipcode_latlng:
        if record1[0] == record2[0]:
            dist = 0
            distance.append([record1[0],record2[0],dist])
        else:
            dist = round(geodesic((record1[1],record1[2]),(record2[1],record2[2])).miles,3)
            distance.append([record1[0],record2[0],dist])
print(distance)
print(len(distance))

# write to csv
with open('distance.csv', 'w', newline = '') as distance_csv:
    wr = csv.writer(distance_csv, dialect = csv.excel, quoting = csv.QUOTE_MINIMAL)
    wr.writerow(["Origin", "Destination", "Distance_miles"])
    for dist in distance:
        wr.writerow(dist)