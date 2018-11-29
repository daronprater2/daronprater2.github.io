#This is the R file used to process the chicago crime data and generate predictions for each chicago zip code.
#To use this file follow these steps

##1. Download the Chicago crime dataset "Crimes_-_2001_to_present.csv" from this link. https://data.cityofchicago.org/Public-Safety/Crimes-2001-to-present/ijzp-q8t2
##2. Ensure the file path on line 25 is changed so the code reads the data file from the correct directory.
##3. Run the code. The code will create total crime predictions for each zip code for 5 years into the future and generate a plot comparing the linear regression prediction R^2 values vs ARIMA R^2 values.
##.. The csv file written at the end is the ARIMA predictions which are used in the tool.




library(sp)
library(maptools)
library(zipcode)
library(lubridate)
library(dplyr)
library(forecast)
library(BBmisc)

data(zipcode)



import_crime <- function(){
  
  #Load data
  crime <- read.csv(file='C:\\Users\\daron\\Downloads\\Crimes_-_2001_to_present.csv')
  
  #Remove na long/lat values
  crime <- crime[!is.na(crime$Longitude), ]
  crime <- crime[!is.na(crime$Latitude), ]
  lon_lats <- crime[c('Longitude', 'Latitude')]
  
  # grab the zip code boundaries
  url <- "http://www2.census.gov/geo/tiger/GENZ2014/shp/cb_2014_us_zcta510_500k.zip"
  fil <- "ztca.zip"
  
  # don't waste bandwidth
  if (!file.exists(fil)) { download.file(url, fil) }
  unzip(fil, exdir="ztca")
  
  # read them in (this takes a bit)
  ztca <- readShapePoly("ztca/cb_2014_us_zcta510_500k.shp", verbose=TRUE)
  
  # extract Illinois Zips
  chicago <- ztca[as.character(ztca$ZCTA5CE10) %in% as.character(zipcode[zipcode$state=="IL",]$zip),]
  
  # make them all super spatial-like (must be in lon,lat format)
  pts <- SpatialPoints(as.matrix(lon_lats[,1:2]))
  
  # figure out where they are (this can take a bit)
  dat <- pts %over% chicago
  
  #Assign zip codes to columns
  crime$zipcode <- dat$ZCTA5CE10
  crime$zipcode <- as.character(crime$zipcode)
  
  #Convert zips to dummy zips
  crime$zipcode <- ifelse(crime$zipcode %in% c('60601', '60602', '60603', '60604', '60605', '60611'), '12311', 
                         ifelse(crime$zipcode %in% c('60606', '60607', '60661'), '6761', crime$zipcode))
  
  #Change Date column to char first, then date type, then extract month and put into month column
  crime$Date <- as.character(crime$Date)
  crime$Date <- as.Date(crime$Date, format = "%m/%d/%Y %H:%M:%S")
  crime$month <- month(crime$Date)
  
  #Change all days to 1
  day(crime$Date) <- 1
  
  #Filter by Only Chicago zips
  chiTownZips = c("6761","12311","60608","60609","60610","60612","60613","60614","60615",
                  "60616","60617","60618","60619","60620","60621","60622","60623","60624","60625","60626",
                  "60628","60629","60630","60631","60632","60633","60634","60636","60637","60638","60639",
                  "60640","60641","60642","60643","60644","60645","60646","60647","60649","60651","60652","60653",
                  "60654","60655","60656","60657","60660", "60659", "60707","60827")
  
  crime <- subset(crime, zipcode %in% chiTownZips)
  
  return(crime)
}

predict_five_years <- function(dataframe, method, arima_type, log_normal){
  
  #Create dataframe for predictions
  future_years = data.frame("Year" = c(2019, 2020, 2021, 2022, 2023))
  
  #predictions dataframe
  crime_preds = data.frame("Zip" = c(), "Year" = c(), "Prediciton" = c())
  
  #Model accuracy dataframe
  accuracy <- data.frame("Zip" = c(), "R^2" = c())
  
  #Loop through all zip codes
  for(zip in unique(dataframe$zipcode)){
    
    
    print(zip)
    
    #Filter dataframe by zipcode
    test_zip = subset(dataframe, zipcode == zip)
    
    #Group by year
    crime_by_year = test_zip %>%
      group_by(Year) %>%
      count()
    
    #Get violent crime rate for that zip code for that year
    #violent_crime_rate = test_zip 
    
    #If using linear regression
    if (method == 'lm'){
      
      
      #Create linear model
      model = lm(n~Year, crime_by_year)
      
      #Get current zip accuracy and put into dataframe
      curr_acc <- data.frame("Zip" = zip, "R^2" = summary(model)$r.squared)
      
      #Add accuracy to overall accuracy dataframe
      accuracy <- rbind(accuracy, curr_acc)
      
      
      
      #Make predictions for future years
      predictions = predict(model, future_years)
      
      #Store predictions in new dataframe
      predictions_df = data.frame("zip" = zip, "year" = c(2019, 2020, 2021, 2022, 2023), 
                                  "prediction" = predictions)
      
      #Current year crime
      current_year = data.frame("zip" = zip, "year" = 2018,
                                "prediction" = crime_by_year[crime_by_year$Year == 2018,]$n)
      
      #Add 2018 and future year predictions to overall dataframe
      crime_preds = rbind(crime_preds, current_year)
      crime_preds = rbind(crime_preds, predictions_df)
      
    }
    
    else if (method == 'arima'){
      
      b_ts <- ts(crime_by_year$n, start = 2001, end=2018, frequency = 1)
      
      if (arima_type == 'random_walk_drift') {
        fit <- Arima(b_ts, order = c(0,1,0), include.drift = TRUE)
      }
      
      else {
        fit <- auto.arima(b_ts)
      }
      
      
      arima_forecast <- forecast(fit, 5)
      if(zip == "12311"){
        print(arima_forecast)
        plot(arima_forecast)
      }
      
      
      sse = 0
      sst = 0
      a = mean(crime_by_year$n)
      
      for(i in 1:length(fit$residuals)){
        sse = sse + fit$residuals[i]^2
        cur = ((crime_by_year$n[i] + fit$residuals[i]) - a)^2
        sst = sst + cur
      }
      
      r2 = 1 - (sse/sst)
      curr_acc <- data.frame("Zip" = zip, "R^2" = r2)
      accuracy <- rbind(accuracy, curr_acc)
      
      #Store predictions in new dataframe
      predictions_df = data.frame("zip" = zip, "year" = c(2019, 2020, 2021, 2022, 2023), 
                                  "prediction" = arima_forecast$mean)
      
      #Current year crime
      current_year = data.frame("zip" = zip, "year" = 2018,
                                "prediction" = crime_by_year[crime_by_year$Year == 2018,]$n)
      
      #Add 2018 and future year predictions to overall dataframe
      crime_preds = rbind(crime_preds, current_year)
      crime_preds = rbind(crime_preds, predictions_df)
      
    }
      
      
    
  }
  
  #Add columns to crime_preds to match csv format
  crime_preds$Category = "Crime"
  crime_preds$NormalizedValue = 0
  crime_preds$Rank = 0
  
  for(year in 2018:2023){
    
    year_preds <- crime_preds[crime_preds$year == year, "prediction"]
    #Get log of predictions
    if(log_normal == TRUE){
      year_preds_log = log10(year_preds)
      year_norms <- normalize(year_preds_log, method='range', range=c(1,0))
      year_ranks <- rank(year_norms)
      if(year == 2018){
        print(year_norms)
        print(year_ranks)
      }
      crime_preds[crime_preds$year == year, "NormalizedValue"] = year_norms
      crime_preds[crime_preds$year == year, "Rank"] = year_ranks
    }
    
    else{
      year_norms <- normalize(year_preds, method='range', range=c(1,0))
      year_ranks <- rank(1-year_norms)
      crime_preds[crime_preds$year == year, "NormalizedValue"] = year_norms
      crime_preds[crime_preds$year == year, "Rank"] = year_ranks
    }
    
  }
  
  
  
  #Order by year to get in same format
  crime_preds = crime_preds[order(crime_preds$year), ]
  
  #Reorder columns to match current format
  crime_preds = crime_preds[,c(1, 4, 2, 3, 5, 6)]
  
  #Add column for Metric
  crime_preds$Metric = "Total Number of Crimes"
  
  #Change Year column to years_ahead
  crime_preds$year = crime_preds$year - 2018
  
  #Change column names to match current format
  colnames(crime_preds) = c("ZipCode", "Category", "YearsAhead", "AbsoluteValue",
                              "NormalizedValue", "Rank", "Metric")
  
  
  
  return (list(crime_preds, accuracy))
}

#Read in data function
crimedf <- import_crime()

#Get Linear Regression Results
lm_results <- predict_five_years(crimedf, 'lm', 'none', TRUE)

#Assign results
lm_prediction <- lm_results[[1]] 
lm_accuracy <- lm_results[[2]]

#Get Arima Results
arima_results <- predict_five_years(crimedf, 'arima', 'random_walk_drift', TRUE)

#Assign results
arima_prediction <- arima_results[[1]] 
arima_accuracy <- arima_results[[2]]  


#Combine results
crime_arima <- arima_prediction

#Compare accuracies of lm vs arima for all zip codes
#Get accuracies into dataframe
accuracy_compare = data.frame("zip" = arima_accuracy$Zip,
                              "lm" = lm_accuracy$R.2,
                              "arima" = arima_accuracy$R.2)
#Melt data
library(reshape2)
library(ggplot2)
accuracy_compare <- melt(accuracy_compare, id="zip")
colnames(accuracy_compare) = c("zip", "model", "R2_Value")
head(accuracy_compare)

#Plot accuracies
ggplot(data = accuracy_compare, aes(x=zip, y=R2_Value, color = model, group = 1)) + 
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0)) + 
  ggtitle("R^2 for Crime Dataset ARIMA vs Linear Model")



#Write to csv
write.csv(crime_arima, "crime.csv", row.names = FALSE)

