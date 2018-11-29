#This is the python file used to process the chicago education data and generate predictions for each chicago zip code.
#To use this file correctly follow the steps below.

#--1. Change the directory to the directory where you will be running this file.
#--2. Download the following files into this directory from https://cps.edu/SchoolData/Pages/SchoolData.aspx and rename them as follows:
##-- SY14_SQRP_Report_CPSEDU_FINAL_20151026.xlsx ---> edu_14.xlsx
##-- SY15_SQRP_Report_CPSEDU_FINAL_20151023.xlsx ---> edu_15.xlsx
##-- Accountability_SQRPratings2016-2017_SchoolLevel.xls ---> edu_16.xlsx
##-- Accountability_SQRPratings2017-2018_SchoolLevel.xls ---> edu_17.xlsx
##-- Accountability_SQRPratings2018-2019_SchoolLevel.xls ---> edu_18.xlsx

#--3. Download the following file from https://data.cityofchicago.org/Education/Chicago-Public-Schools-School-Progress-Reports-SY1/cp7s-7gxg into this directoryself.
##--Chicago_Public_Schools_-_School_Progress_Reports_SY1617.csv

#--4. Run this code and the csv file generated will contain SQRP score predictions for each zip code.



import pandas as pd
import numpy as np
import os

os.chdir('C:\\Users\\daron\\OneDrive\\CSE 6242 - Data and Visual Analytics\\Edu Data\\')

df = pd.DataFrame()

for i in range(14, 19):
    #For each sheet name
    for sname in ["Elem Schools (grds PreK-8 only)", "High Schools (grds 9-12 only)"]:
        #Load in dataset
        df_new = pd.read_excel("edu_{}.xlsx".format(i), sheetname = sname, header=None)

        #Only get first 4 columns
        df_new = df_new.iloc[:, :4]

        #Add column for year
        df_new['Year'] = i + 2000

        #Add column for high school or elementary
        if sname[0] == 'E':
            df_new['type'] = 'ES'
        elif sname[0] == 'H':
            df_new['type'] = 'HS'
        #Append to overall dataframe

        df = df.append(df_new)

#Read in progress report dataset to get zip codes for each school.
school_info = pd.read_csv("Chicago_Public_Schools_-_School_Progress_Reports_SY1617.csv")

#Formatting.
df.columns = ["School_ID", "School_Name", "Network", "SQRP", "Year", "Type"]

df2 = pd.merge(df, school_info[['School_ID', 'Address', 'City', 'State', 'Zip']], on='School_ID', how='left')
#df2 = pd.concat([df, school_info[['School_ID', 'Address', 'City', 'State', 'Zip']]], axis=1, join='inner')

#Use group by object to impute median SQRP values for schools
by_school_ID = df2.groupby('School_ID')

def impute_median(series):
    return series.fillna(series.median())

#Impute values using impute_median and transform function.
df2.SQRP = by_school_ID.SQRP.transform(impute_median)

#Filter out nulls
df2 = df2[df2["SQRP"].notnull()]
df2 = df2[df2["Zip"].notnull()]

#Aggregate data by zip and Year
eduByZip = df2.groupby(["Zip", "Year"]).mean()['SQRP'].reset_index()

#Turn zip code column to string
eduByZip["Zip"] = eduByZip["Zip"].apply(lambda x: str(x)[:5])

#Create list of years
years = [2019, 2020, 2021, 2022, 2023]

#Create new dataframe to hold predictions
eduPredictions = pd.DataFrame(columns=["Zip", "Year", "SQRP"])

#convert non-existant zips to dummy zips
zip_6761 = ['60606', '60607','60661']
zip_12311 = ['60601', '60602', '60603', '60604', '60605', '60611']

#For loop to do conversion
for i in range(len(eduByZip)):

    #Set x to current zip in dataframe
    x = eduByZip.loc[i, "Zip"]

    #Converto to 6761 if zip is in list
    if x in zip_6761:
        eduByZip.loc[i, "Zip"] = '6761'

    #Convert to 12311 if zip is in list
    elif x in zip_12311:
        eduByZip.loc[i, "Zip"] = '12311'

#Group by Zip and year to get average SQRP for newly created zip codes
eduByZip = eduByZip.groupby(["Zip", "Year"]).mean().reset_index()

#Get unique zip codes
zips = eduByZip.Zip.unique().tolist()

#For loop to generate predictions
from sklearn.linear_model import LinearRegression
import numpy as np

for zipc in zips:
    test_zip = eduByZip[eduByZip["Zip"] == zipc]

    #Create predictions for zip code
    #Reshaped input vectors due to scikit-learn requirement
    lm = LinearRegression()
    lm.fit(np.array(test_zip['Year']).reshape(-1,1), test_zip['SQRP'])
    predictions = lm.predict(np.array(years).reshape(-1,1))

    #Cap predictions at 5 because SQRP score has a max of 5
    predictions = [pred if pred < 5 else 5 for pred in predictions]

    #Create dataframe with new predictions
    current = test_zip[test_zip['Year'] == 2018]['SQRP'].tolist()[0]
    new_df = pd.DataFrame({'Zip':zipc, 'Year':years, 'SQRP':predictions})
    new_df.loc[-1] = [zipc, 2018, current]

    #Append to overall dataframe
    eduPredictions = eduPredictions.append(new_df)

##Formatting
eduPredictions['Year'] = eduPredictions['Year'].apply(lambda x: x-2018)

eduPredictions['Category'] = 'Education'

#For loop to compute normalized numbers
for year in range(6):

    #Get data for year in loop
    yearPreds = eduPredictions[eduPredictions['Year'] == year]['SQRP'].tolist()

    #Calculate normalized score values
    norms = [(x - min(yearPreds))/(max(yearPreds) - min(yearPreds)) for x in yearPreds]

    #Sort normalized values
    norms_sorted = sorted(norms)

    #Use sorted normalized values to obtain ranks
    ranks = [norms_sorted.index(norms[i]) for i in range(len(norms))]

    #Add ranks to eduPredictions Dataframe.
    eduPredictions.loc[eduPredictions['Year'] == year, 'Rank'] = ranks

    #Add add norms to eduPredictions Dataframe
    eduPredictions.loc[eduPredictions['Year'] == year, 'NormalizedValue'] = norms


##--Data formatting for csv export --##
eduPredictions['Metric'] = 'Average SQRP Score'

eduPredictions.columns = ['ZipCode', 'YearsAhead', 'AbsoluteValue', 'Category', 'Rank', 'NormalizedValue', 'Metric']

eduPredictions = eduPredictions[['ZipCode', 'Category', 'YearsAhead', 'AbsoluteValue', 'NormalizedValue', 'Rank', 'Metric']]

eduPredictions.sort_values(by='YearsAhead', inplace=True)

eduPredictions.to_csv("edu.csv", index=False)
