# Program used to aggregate and forecast on median home price
# and business license (used for shopping and entertainment ) data.

# Datasets are imported from 
# Median Home Prices
# https://www.realtor.com/research/data/ which links to below
# https://s3-us-west-2.amazonaws.com/econresearch/Reports/Core/RDC_InventoryCoreMetrics_Zip.csv

# Business Licenses
# https://data.cityofchicago.org/Community-Economic-Development/Business-Licenses/r5kz-chrr


# Import Libraries ####

library(BBmisc)
library(dplyr)
library(forecast)
library(ggplot2)

# Define Functions ####

import_land_area <- function() {
  
  # Get land areas for zip codes so we can adjust for it
  
  land_area_df <- read.csv(file = 'C:\\Users\\bzmcc\\Documents\\CSE6242\\Project\\Gaz_zcta_national.txt', sep = '\t')
  land_area_df <- land_area_df[c('GEOID', 'ALAND_SQMI')]
  names(land_area_df) <- c("ZipCode", "SquareMiles")
  ohare <- data.frame(ZipCode = 60666,
                      SquareMiles = 11.25)
  dum1_df <- data.frame(ZipCode = 6761,
                        SquareMiles = sum(land_area_df[land_area_df$ZipCode %in% c(60606, 60607, 60661), 'SquareMiles']))
  dum2_df <- data.frame(ZipCode = 12311,
                        SquareMiles = sum(land_area_df[land_area_df$ZipCode %in% c(60601, 60602, 60603,
                                                                                   60604, 60605, 60611), 'SquareMiles']))
  land_area_df <- rbind(land_area_df, ohare)
  land_area_df <- rbind(land_area_df, dum1_df)
  land_area_df <- rbind(land_area_df, dum2_df)
  
  return(land_area_df)
}
import_median_price <- function() {
  
  # https://www.realtor.com/research/data/
  median_listing_df <- read.csv(file='C:\\Users\\bzmcc\\Documents\\CSE6242\\Project\\Data\\RDC_InventoryCoreMetrics_Zip_Hist_Realtordotcom_10_22_18.csv',
                                stringsAsFactors = FALSE)
  median_listing_df <- median_listing_df[median_listing_df$ZipName == 'Chicago, IL' |
                                           median_listing_df$ZipCode %in% c(60707, 60827), ]
  median_listing_df$Month <- as.Date(median_listing_df$Month)
  
  
  # Group by dummy and real zips
  
  median_listing_df$ZipCode <- ifelse(median_listing_df$ZipCode %in% c(60601, 60602, 60603,
                                                                       60604, 60605, 60611), 12311,
                                      ifelse(median_listing_df$ZipCode %in% c(60606, 60607, 60661), 6761, median_listing_df$ZipCode))
  
  
  temp <- by(median_listing_df,list(median_listing_df$ZipCode, 
                                    median_listing_df$Month), function(z) {
                                      
                                      ZipCode <- z$ZipCode[1]
                                      Month <- z$Month[1]
                                      Median.Listing.Price <- sum(z$Median.Listing.Price) / length(z$Median.Listing.Price)
                                      
                                      data.frame(ZipCode,
                                                 Month,
                                                 Median.Listing.Price)
                                    })
  median_listing_df <- do.call(rbind,temp)
  
  return(median_listing_df)
  
}

import_biz <- function() {
  
  biz_df <- read.csv(file='C:\\Users\\bzmcc\\Documents\\CSE6242\\Project\\Data\\Business_Licenses.csv',
                     stringsAsFactors = FALSE)
  
  
  # Replace zips with dummys
  
  biz_df$ZIP.CODE <- ifelse(biz_df$ZIP.CODE %in% c(60601, 60602, 60603,
                                                   60604, 60605, 60611), 12311,
                            ifelse(biz_df$ZIP.CODE %in% c(60606, 60607, 60661), 6761, biz_df$ZIP.CODE))
  
  biz_df <- biz_df[biz_df$ZIP.CODE != '', ]
  # biz_df <- biz_df[biz_df$ZIP.CODE != 60827, ]
  # biz_df <- biz_df[!biz_df$ZIP.CODE %in% c('01701',
  #                                             '05478',
  #                                             '05676',
  #                                             '34620',
  #                                             '60635',
  #                                             '60650',
  #                                             'V6B1A',
  #                                             '60039',
  #                                             '60263',
  #                                             '60085',
  #                                             '66047',
  #                                             '60139'), ]
  
  biz_df$YearIssued <- as.numeric(format(as.Date(biz_df$DATE.ISSUED, format = '%m/%d/%Y'), '%Y'))
  biz_df$MonthIssued <- as.Date(format(as.Date(biz_df$DATE.ISSUED, format = '%m/%d/%Y'), '%Y/%m/01'))
  
  return(biz_df)
  
}
filter_and_agg_biz <- function(biz_df, filter_grep) {
  
  biz_df2 <- biz_df[grepl(filter_grep, biz_df$BUSINESS.ACTIVITY.ID), ]
  biz_df2 <- merge(biz_df2, land_area_df,
                   by.x = 'ZIP.CODE',
                   by.y = 'ZipCode',
                   all.x = TRUE)
  
  temp <- by(biz_df2,list(biz_df2$ZIP.CODE, 
                          biz_df2$MonthIssued), function(z) {
                            
                            ZipCode <- z$ZIP.CODE[1]
                            Month <- z$MonthIssued[1]
                            LicenseCount <- length(z$ZIP.CODE)
                            SquareMiles <- z$SquareMiles[1]
                            
                            data.frame(ZipCode,
                                       Month,
                                       LicenseCount,
                                       SquareMiles)
                          })
  agg_df <- do.call(rbind,temp)
  
  return(agg_df)
  
}

predict_five_years <- function(current_data, monthly_data, prediction_method,
                               datapoint, category, metric, met_range, start,
                               end, log_normal, arima_type) {
  
  one_year_df <- data.frame()
  two_year_df <- data.frame()
  three_year_df <- data.frame()
  four_year_df <- data.frame()
  five_year_df <- data.frame()
  
  accuracy <- data.frame("Zip" = c(), "R^2" = c())
  
  for (zip in unique(current_data$ZipCode)) {
    
    print(zip)
    
    b <- monthly_data[monthly_data$ZipCode == zip, c('ZipCode',
                                                     'Month',
                                                     'SquareMiles',
                                                     datapoint)]
    
    b <- b[order(b$Month, decreasing = FALSE), ]
    
    b$month_count <- 1:nrow(b) - 1
    
    form <- as.formula(paste0(datapoint, ' ~ month_count') )
    
    if (prediction_method == 'lm') {
    
      b_lm <- lm(form, data = b)
      
      r2 <- summary(b_lm)$r.squared
      # print(acc)
      
      one_year_pred <- (nrow(b) + 12) * b_lm$coefficients[[2]] + b_lm$coefficients[[1]]
      two_year_pred <- (nrow(b) + 24) * b_lm$coefficients[[2]] + b_lm$coefficients[[1]]
      three_year_pred <- (nrow(b) + 36) * b_lm$coefficients[[2]] + b_lm$coefficients[[1]]
      four_year_pred <- (nrow(b) + 48) * b_lm$coefficients[[2]] + b_lm$coefficients[[1]]
      five_year_pred <- (nrow(b) + 60) * b_lm$coefficients[[2]] + b_lm$coefficients[[1]]
      
    }
    
    if (prediction_method == 'arima') {
      
      b_ts <- ts(b[c(datapoint)], start = start, end = end, frequency = 12)
      
      if (arima_type == 'random_walk_drift') {
        fit <- Arima(b_ts, order = c(0,1,0), include.drift = TRUE)
      }
      
      else {
        fit <- auto.arima(b_ts)
      }
      
      arima_forecast <- forecast(fit, 60)
      plot(arima_forecast)
      
      one_year_pred <- arima_forecast[[4]][[12]]
      two_year_pred <- arima_forecast[[4]][[24]]
      three_year_pred <- arima_forecast[[4]][[36]]
      four_year_pred <- arima_forecast[[4]][[48]]
      five_year_pred <- arima_forecast[[4]][[60]]
      
      sse = 0
      sst = 0
      a = mean(b_ts)
      
      for(i in 1:length(fit$residuals)){
        
        sse = sse + fit$residuals[i]^2
        cur = ((b_ts[i] + fit$residuals[i]) - a)^2
        sst = sst + cur
        
      }
      
      r2 = 1 - (sse/sst)
      
    }
    
    prediction_df_1 <- data.frame(ZipCode = b$ZipCode[1],
                                  Category = category,
                                  YearsAhead = 1,
                                  AbsoluteValue = one_year_pred,
                                  NormalizedValue = NA,
                                  Rank = NA,
                                  Metric = metric,
                                  SquareMiles = b$SquareMiles[1])
    
    one_year_df <- rbind(one_year_df, prediction_df_1)
    
    prediction_df_2 <- data.frame(ZipCode = b$ZipCode[1],
                                  Category = category,
                                  YearsAhead = 2,
                                  AbsoluteValue = two_year_pred,
                                  NormalizedValue = NA,
                                  Rank = NA,
                                  Metric = metric,
                                  SquareMiles = b$SquareMiles[1])
    
    two_year_df <- rbind(two_year_df, prediction_df_2)
    
    prediction_df_3 <- data.frame(ZipCode = b$ZipCode[1],
                                  Category = category,
                                  YearsAhead = 3,
                                  AbsoluteValue = three_year_pred,
                                  NormalizedValue = NA,
                                  Rank = NA,
                                  Metric = metric,
                                  SquareMiles = b$SquareMiles[1])
    
    three_year_df <- rbind(three_year_df, prediction_df_3)
    
    prediction_df_4 <- data.frame(ZipCode = b$ZipCode[1],
                                  Category = category,
                                  YearsAhead = 4,
                                  AbsoluteValue = four_year_pred,
                                  NormalizedValue = NA,
                                  Rank = NA,
                                  Metric = metric,
                                  SquareMiles = b$SquareMiles[1])
    
    four_year_df <- rbind(four_year_df, prediction_df_4)
    
    prediction_df_5 <- data.frame(ZipCode = b$ZipCode[1],
                                  Category = category,
                                  YearsAhead = 5,
                                  AbsoluteValue = five_year_pred,
                                  NormalizedValue = NA,
                                  Rank = NA,
                                  Metric = metric,
                                  SquareMiles = b$SquareMiles[1])
    
    five_year_df <- rbind(five_year_df, prediction_df_5)
    
    curr_acc <- data.frame("Zip" = zip, "R^2" = r2)
    accuracy <- rbind(accuracy, curr_acc)
    
  }
  
  if (log_normal == TRUE) {
    
    one_year_df$ValuePerSqMile <- one_year_df$AbsoluteValue / one_year_df$SquareMiles
    one_year_df$log_ValuePerSqMile <- log10(one_year_df$ValuePerSqMile)
    one_year_df$NormalizedValue <- normalize(one_year_df$log_ValuePerSqMile, method = 'range', range = met_range)
    one_year_df$Rank <- rank(-one_year_df$NormalizedValue)
    
    two_year_df$ValuePerSqMile <- two_year_df$AbsoluteValue / two_year_df$SquareMiles
    two_year_df$log_ValuePerSqMile <- log10(two_year_df$ValuePerSqMile)
    two_year_df$NormalizedValue <- normalize(two_year_df$log_ValuePerSqMile, method = 'range', range = met_range)
    two_year_df$Rank <- rank(-two_year_df$NormalizedValue)
    
    three_year_df$ValuePerSqMile <- three_year_df$AbsoluteValue / three_year_df$SquareMiles
    three_year_df$log_ValuePerSqMile <- log10(three_year_df$ValuePerSqMile)
    three_year_df$NormalizedValue <- normalize(three_year_df$log_ValuePerSqMile, method = 'range', range = met_range)
    three_year_df$Rank <- rank(-three_year_df$NormalizedValue)
    
    four_year_df$ValuePerSqMile <- four_year_df$AbsoluteValue / four_year_df$SquareMiles
    four_year_df$log_ValuePerSqMile <- log10(four_year_df$ValuePerSqMile)
    four_year_df$NormalizedValue <- normalize(four_year_df$log_ValuePerSqMile, method = 'range', range = met_range)
    four_year_df$Rank <- rank(-four_year_df$NormalizedValue)
    
    five_year_df$ValuePerSqMile <- five_year_df$AbsoluteValue / five_year_df$SquareMiles
    five_year_df$log_ValuePerSqMile <- log10(five_year_df$ValuePerSqMile)
    five_year_df$NormalizedValue <- normalize(five_year_df$log_ValuePerSqMile, method = 'range', range = met_range)
    five_year_df$Rank <- rank(-five_year_df$NormalizedValue)
    
  }
  
  else {
    
    one_year_df$NormalizedValue <- normalize(one_year_df$AbsoluteValue, method = 'range', range = met_range)
    one_year_df$Rank <- rank(-one_year_df$NormalizedValue)
    
    two_year_df$NormalizedValue <- normalize(two_year_df$AbsoluteValue, method = 'range', range = met_range)
    two_year_df$Rank <- rank(-two_year_df$NormalizedValue)
    
    three_year_df$NormalizedValue <- normalize(three_year_df$AbsoluteValue, method = 'range', range = met_range)
    three_year_df$Rank <- rank(-three_year_df$NormalizedValue)
    
    four_year_df$NormalizedValue <- normalize(four_year_df$AbsoluteValue, method = 'range', range = met_range)
    four_year_df$Rank <- rank(-four_year_df$NormalizedValue)
    
    five_year_df$NormalizedValue <- normalize(five_year_df$AbsoluteValue, method = 'range', range = met_range)
    five_year_df$Rank <- rank(-five_year_df$NormalizedValue)
  }
  
  pred_dfs <- rbind(one_year_df,
                    two_year_df,
                    three_year_df,
                    four_year_df,
                    five_year_df)
  
  pred_dfs$ValuePerSqMile <- NULL
  pred_dfs$log_ValuePerSqMile <- NULL
  pred_dfs$SquareMiles <- NULL
  
  five_year_pred_df <- rbind(current_data,
                             pred_dfs)
  
  df_list <- list(five_year_pred_df, accuracy)
  
  return(df_list)
  
}

# Run Program ####

land_area_df <- import_land_area()


# Median home prices

median_listing_df <- import_median_price()

median_listing_df <- merge(median_listing_df, land_area_df,
                 by.x = 'ZipCode',
                 by.y = 'ZipCode',
                 all.x = TRUE)

current_period_median_prices <- median_listing_df[median_listing_df$Month == '2018-09-01 00:00:00', ]
current_period_median_prices$Category <- 'Median Home Price'
current_period_median_prices$YearsAhead <- 0
current_period_median_prices$AbsoluteValue <- current_period_median_prices$Median.Listing.Price
current_period_median_prices$NormalizedValue <- normalize(current_period_median_prices$Median.Listing.Price, method = 'range', range = c(1,0))
current_period_median_prices$Metric <- 'Median Home Price in Dollars'
current_period_median_prices$Rank <- rank(-current_period_median_prices$NormalizedValue)

current_period_median_prices <- current_period_median_prices[c('ZipCode',
                                                               'Category',
                                                               'YearsAhead',
                                                               'AbsoluteValue',
                                                               'NormalizedValue',
                                                               'Rank',
                                                               'Metric')]


home_price_list <- predict_five_years(current_period_median_prices, median_listing_df,
                                                prediction_method = 'arima', datapoint = 'Median.Listing.Price',
                                                category = 'Median Home Price',
                                                metric = 'Median Home Price in Dollars',
                                                met_range = c(1,0),
                                                start = c(2012, 5),
                                                end = c(2018, 9),
                                                log_normal = FALSE,
                                                arima_type = 'random_walk_drift')

home_price_list_lm <- predict_five_years(current_period_median_prices, median_listing_df,
                                      prediction_method = 'lm', datapoint = 'Median.Listing.Price',
                                      category = 'Median Home Price',
                                      metric = 'Median Home Price in Dollars',
                                      met_range = c(1,0),
                                      start = c(2012, 5),
                                      end = c(2018, 9),
                                      log_normal = FALSE,
                                      arima_type = 'random_walk_drift')

all_years_median_price_df <- home_price_list[[1]]

home_price_accuracy_arima <- home_price_list[[2]]
home_price_accuracy_arima$model_type <- 'ARIMA'

home_price_accuracy_lm <- home_price_list_lm[[2]]
home_price_accuracy_lm$model_type <- 'LM'

home_price_accuracy <- rbind(home_price_accuracy_arima, home_price_accuracy_lm)
home_price_accuracy$Zip <- as.factor(home_price_accuracy$Zip)






# Nightlife and entertainment

biz_df <- import_biz()
agg_nightlife_df <- filter_and_agg_biz(biz_df, filter_grep = '638|639|735|736|781|782|829|697|698|916|717|996')

current_period_nightlife <- agg_nightlife_df[agg_nightlife_df$Month == '2018-09-01 00:00:00', ]
current_period_nightlife$Category <- 'Nightlife/Social/Entertainment'
current_period_nightlife$YearsAhead <- 0
current_period_nightlife$AbsoluteValue <- current_period_nightlife$LicenseCount
current_period_nightlife$log_ValuePerSqMile <- log10(current_period_nightlife$AbsoluteValue / current_period_nightlife$SquareMiles)
current_period_nightlife$NormalizedValue <- normalize(current_period_nightlife$log_ValuePerSqMile, method = 'range', range = c(0,1))
current_period_nightlife$Metric <- 'Count of newly issued and renewed licenses for nightlife/social/entertainment businesses'
current_period_nightlife$Rank <- rank(-current_period_nightlife$log_ValuePerSqMile)

current_period_nightlife <- current_period_nightlife[c('ZipCode',
                                                               'Category',
                                                               'YearsAhead',
                                                               'AbsoluteValue',
                                                               'NormalizedValue',
                                                               'Rank',
                                                               'Metric')]


nightlife_list <- predict_five_years(current_period_nightlife, agg_nightlife_df,
                                                prediction_method = 'arima', datapoint = 'LicenseCount',
                                                category = 'Nightlife/Social/Entertainment',
                                                metric = 'Count of newly issued and renewed licenses for nightlife/social/entertainment businesses',
                                                met_range = c(0,1),
                                                start = c(2002, 1),
                                                end = c(2018, 10),
                                                log_normal = TRUE,
                                                arima_type = 'optimal')

nightlife_list_lm <- predict_five_years(current_period_nightlife, agg_nightlife_df,
                                     prediction_method = 'lm', datapoint = 'LicenseCount',
                                     category = 'Nightlife/Social/Entertainment',
                                     metric = 'Count of newly issued and renewed licenses for nightlife/social/entertainment businesses',
                                     met_range = c(0,1),
                                     start = c(2002, 1),
                                     end = c(2018, 10),
                                     log_normal = TRUE,
                                     arima_type = 'optimal')

all_years_nightlife_df <- nightlife_list[[1]]

nightlife_accuracy_arima <- nightlife_list[[2]]
nightlife_accuracy_arima$model_type <- 'ARIMA'

nightlife_accuracy_lm <- nightlife_list_lm[[2]]
nightlife_accuracy_lm$model_type <- 'LM'

nightlife_accuracy <- rbind(nightlife_accuracy_arima, nightlife_accuracy_lm)





# Everyday shopping

agg_shopping_df <- filter_and_agg_biz(biz_df, filter_grep = '927|864|827|797|783|900|931|759|760|954|911|921|766|904|767|768|775|729')

current_period_shopping <- agg_shopping_df[agg_shopping_df$Month == '2018-09-01 00:00:00', ]
current_period_shopping$Category <- 'Everyday Shopping'
current_period_shopping$YearsAhead <- 0
current_period_shopping$AbsoluteValue <- current_period_shopping$LicenseCount
current_period_shopping$log_ValuePerSqMile <- log10(current_period_shopping$AbsoluteValue / current_period_shopping$SquareMiles)
current_period_shopping$NormalizedValue <- normalize(current_period_shopping$log_ValuePerSqMile, method = 'range', range = c(0,1))
current_period_shopping$Metric <- 'Count of newly issued and renewed licenses for everyday shopping businesses'
current_period_shopping$Rank <- rank(-current_period_shopping$log_ValuePerSqMile)

current_period_shopping <- current_period_shopping[c('ZipCode',
                                                       'Category',
                                                       'YearsAhead',
                                                       'AbsoluteValue',
                                                       'NormalizedValue',
                                                       'Rank',
                                                       'Metric')]


shopping_list <- predict_five_years(current_period_shopping, agg_shopping_df,
                                             prediction_method = 'arima', datapoint = 'LicenseCount',
                                             category = 'Everyday Shopping',
                                             metric = 'Count of newly issued and renewed licenses for everyday shopping businesses',
                                             met_range = c(0,1),
                                             start = c(2002, 1),
                                             end = c(2018, 10),
                                             log_normal = TRUE,
                                             arima_type = 'optimal')

shopping_list_lm <- predict_five_years(current_period_shopping, agg_shopping_df,
                                    prediction_method = 'lm', datapoint = 'LicenseCount',
                                    category = 'Everyday Shopping',
                                    metric = 'Count of newly issued and renewed licenses for everyday shopping businesses',
                                    met_range = c(0,1),
                                    start = c(2002, 1),
                                    end = c(2018, 10),
                                    log_normal = TRUE,
                                    arima_type = 'optimal')

all_years_shopping_df <- shopping_list[[1]]

shopping_accuracy_arima <- shopping_list[[2]]
shopping_accuracy_arima$model_type <- 'ARIMA'

shopping_accuracy_lm <- shopping_list_lm[[2]]
shopping_accuracy_lm$model_type <- 'LM'

shopping_accuracy <- rbind(shopping_accuracy_arima, shopping_accuracy_lm)


all_data <- rbind(all_years_median_price_df,
                  all_years_shopping_df,
                  all_years_nightlife_df)


write.csv(all_data, file = 'C:\\Users\\bzmcc\\Documents\\CSE6242\\Project\\nightlife_shopping_homeprices.csv',
          row.names = FALSE)

shopping_r2_chart <- ggplot(data = shopping_accuracy, aes(Zip, R.2)) + geom_point(aes(color = factor(model_type))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle('R^2 for Shopping Dataset ARIMA vs Linear Model')

nightlife_r2_chart <- ggplot(data = nightlife_accuracy, aes(Zip, R.2)) + geom_point(aes(color = factor(model_type))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle('R^2 for Nightlife and Entertainment Dataset ARIMA vs Linear Model')

home_price_r2_chart <- ggplot(data = home_price_accuracy, aes(Zip, R.2)) + geom_point(aes(color = factor(model_type))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle('R^2 for Median Home Price Dataset ARIMA vs Linear Model')

