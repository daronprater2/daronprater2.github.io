library(sp)
library(maptools)
library(zipcode)

data(zipcode)

#Load data
test <- read.csv(file='C:\\Users\\daron\\Downloads\\Crimes_-_2001_to_present.csv')

#Remove na long/lat values
test <- test[!is.na(test$Longitude), ]
test <- test[!is.na(test$Latitude), ]
lon_lats <- test[c('Longitude', 'Latitude')]

# grab the zip code boundaries
url <- "http://www2.census.gov/geo/tiger/GENZ2014/shp/cb_2014_us_zcta510_500k.zip"
fil <- "ztca.zip"

# don't waste bandwidth
if (!file.exists(fil)) { download.file(url, fil) }
unzip(fil, exdir="ztca")

# read them in (this takes a bit)
ztca <- readShapePoly("ztca/cb_2014_us_zcta510_500k.shp", verbose=TRUE)

# extract Chicago Zips
chicago <- ztca[as.character(ztca$ZCTA5CE10) %in% as.character(zipcode[zipcode$state=="IL",]$zip),]

# make them all super spatial-like (must be in lon,lat format)
pts <- SpatialPoints(as.matrix(lon_lats[,1:2]))


# figure out where they are (this can take a bit)
dat <- pts %over% chicago

#Assign zip codes to columns
test$zipcode <- dat$ZCTA5CE10

format(as.Date(test$Date, format="%m/%d/%Y"))

#Get unique zip codes
zips = unique(test$zipcode)
zips = zips[!is.na(zips)]

#Create dataframe for predictions
future_years = data.frame("Year" = c(2019, 2020, 2021, 2022, 2023))

#predictions dataframe
crime_preds = data.frame("Zip" = c(), "Year" = c(), "Prediciton" = c())

# #Change aggregated zips to correct zipcode.
# zip.6761 = c(60606, 60607, 60661)
# zip.12311 = c(60601, 60602, 60603, 60604, 60605, 60611)
# 
# #For loop to change zipcodes in test dataframe.
# for (row in nrow(test)){
# 
#   zip = test[row, "zipcode"]
#   if (zip %in% zip.6761){
#     print(zip)
#     test[row, "zipcode"] = 6761
#   }else if (zip %in% zip.12311){
# #    print(zip)
# #    test[row, "zipcode"] = 12311
# #  }
# #}  

#For loop to create models for each zip code
for (zip in zips){
  
  #Filter dataframe by zipcode
  test_zip = subset(test, zipcode == zip)
  
  #Group by year
  crime_by_year = test_zip %>%
    group_by(Year) %>%
    count()
  
  #If current zip doesn't include the 2018 year, then skip.
  if (!(2018 %in% crime_by_year$Year)){
    next
  }
  #Create linear model
  model = lm(n~Year, crime_by_year)
  
  #Make predictions for future years
  predictions = predict(model, future_years)
  
  #Store predictions in new dataframe
  predictions_df = data.frame("Zip" = zip, "Year" = c(2019, 2020, 2021, 2022, 2023), 
                              "Prediction" = predictions)
  
  #Current year crime
  current_year = data.frame("Zip" = zip, "Year" = 2018,
                            "Prediction" = crime_by_year[crime_by_year$Year == 2018,]$n)
  
  #Add new dataframe to overall dataframe
  crime_preds = rbind(crime_preds, current_year)

  crime_preds = rbind(crime_preds, predictions_df)
}


#Convert Zip column to character
crime_preds[, "Zip"] = sapply(crime_preds[, "Zip"], as.character)

##----Aggregate data by new dummy zips----##
# Change aggregated zips to correct zipcode.
zip.6761 = c('60606', '60607', '60661')
zip.12311 = c('60601', '60602', '60603', '60604', '60605', '60611')

#For loop to loop through crime_preds and change zip codes accordingly.
for (row in 1:nrow(crime_preds)){
  
    zip = crime_preds[row, "Zip"]
    
    if (zip %in% zip.6761){
      
      crime_preds[row, "Zip"] = '6761'
      
    }else if (zip %in% zip.12311){
      
      crime_preds[row, "Zip"] = '12311'
      
    }
  
}

#Create new dataframe with new aggregated zips
crimePredsAgg = crime_preds %>%
  group_by(Zip, Year) %>%
  summarise(prediction = sum(Prediction))

#Turn to dataframe
crimePredsAgg = as.data.frame(crimePredsAgg)

#Add columns to crime_preds to match csv format
crimePredsAgg$Category = "Crime"
crimePredsAgg$NormalizedValue = 0
crimePredsAgg$Rank = 0
crimePredsAgg[,"prediction"] = log(crimePredsAgg[,"prediction"], 10)

#Order by year to get in same format
crimePredsAgg = crimePredsAgg[order(crimePredsAgg$Year), ]

#Filter by Only Chicago zips
chiTownZips = c("6761","12311","60608","60609","60610","60612","60613","60614","60615",
"60616","60617","60618","60619","60620","60621","60622","60623","60624","60625","60626",
"60628","60629","60630","60631","60632","60633","60634","60636","60637","60638","60639",
"60640","60641","60642","60643","60644","60645","60646","60647","60649","60651","60652","60653",
"60654","60655","60656","60657","60660", "60659", "60707","60827")

#Only select zips from chiTownZips
crimePredsAgg = subset(crimePredsAgg, Zip %in% chiTownZips)



#Compute standardized values for each year
for (year in 2018:2023){
  
  year_preds = crimePredsAgg[crimePredsAgg$Year == year, "prediction"]
  
  norms = 1-(year_preds - min(year_preds)) / (max(year_preds) - min(year_preds))
  
  ranks = rank(norms)
  
  crimePredsAgg[crimePredsAgg$Year == year, "NormalizedValue"] = norms
  
  crimePredsAgg[crimePredsAgg$Year == year, "Rank"] = ranks
  
}

#Reorder columns to match current format
crimePredsAgg = crimePredsAgg[,c(1, 4, 2, 3, 5, 6)]

#Add column for Metric
crimePredsAgg$Metric = "Total Number of Crimes"

#Change Year column to years_ahead
crimePredsAgg$Year = crimePredsAgg$Year - 2018

#Change column names to match current format
colnames(crimePredsAgg) = c("ZipCode", "Category", "YearsAhead", "AbsoluteValue",
                          "NormalizedValue", "Rank", "Metric")

#Write to csv
write.csv(crimePredsAgg, "crime_v4.csv", row.names = FALSE)
