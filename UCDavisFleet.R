########### Install required packages if they are not already installed
if (any(grepl("RCurl",installed.packages()))==FALSE){
  install.packages("RCurl", repos="http://cran.rstudio.com/")
}
if (any(grepl("XML", installed.packages()))==FALSE){
  install.packages("XML", repos="http://cran.rstudio.com/")
}
if (any(grepl("stringr", installed.packages()))==FALSE){
  install.packages("stringr", repos="http://cran.rstudio.com/")
}
if (any(grepl("tidyr", installed.packages()))==FALSE){
  install.packages("tidyr", repos="http://cran.rstudio.com/")
}
if (any(grepl("sqldf", installed.packages()))==FALSE){
  install.packages("sqldf", repos="http://cran.rstudio.com/")
}

########### Import libraries
library(RCurl)
library(XML)
library(stringr)
library(tidyr)
library(sqldf)

########### Read in URL and table for rolling inventory
url <- "http://fleet.ucdavis.edu/Fleet/inService?assetType=ALL&category=roll"

# Read In-Service Units table from main web page
tables <- readHTMLTable(url)

# In-Service Units table
tableDF = data.frame(tables$`NULL`)

########### Read in URL links for each vehicle
html <- paste(readLines(url), collapse="\n")

# Create list which includes matched URL
matchList <- str_match_all(html, "<a href=\"(.*?)\"")

# Extract matched URLs from matched list
matchDF <- data.frame(matched[[1]][,2])

# Change data type from factor to character
matchDF$matched..1.....2. = as.character(matchDF$matched..1.....2.)

# Extract vehicle unit number URLs
Units = subset(matchDF, grepl("unit", matchDF$matched..1.....2.) & grepl("\\?", matchDF$matched..1.....2.))

# combine the unit number values with our In-Service Units table
UnitsLinked = cbind(tableDF, Units)

########### Extract data from URL links
# Read Unit Information tables from each unit number web page
UnitInformation = sapply(UnitsLinked$matched..1.....2., function(x){readHTMLTable(as.character(x))})

## Convert each from long to wide format such that you have one row per vehicle unit number
# Begin with the first entry, then bind the next ones one row at a time in a for loop
ExtractedDetail_wide = spread(data.frame(UnitInformation[[1]]), Information, Value)

for (i in 2:length(UnitInformation)){
  Extraction = spread(data.frame(UnitInformation[[i]]), Information, Value)
  ExtractedDetail_wide = rbind(ExtractedDetail_wide, Extraction)
}

# Bind the unit Information dataframe to the In-Service Units table with unit number URLs
Result = cbind(UnitsLinked, ExtractedDetail_wide)

########### Parse utilization column, bring into seperate rows for each of LTD, 12mo, 6mo, 3mo using regular expressions
Result$Utilization = gsub("Â", "", as.character(Result$Utilization))

newCol1<-strsplit(as.character(Result$Utilization),';',fixed=TRUE)

Result<-data.frame(Result,do.call(rbind, newCol1))

# LTD
Result$X1 = as.character(Result$X1)
Result$X1 = gsub("LTD:      |LTD: ", "", Result$X1)
Result$X1 = gsub(" mi/mo", "", Result$X1)
Result$X1 = gsub(",", "", Result$X1)
Result$X1 = as.numeric(Result$X1)

# 12mo
Result$X2 = as.character(Result$X2)
Result$X2 = gsub("12mo", "", Result$X2)
Result$X2 = gsub(":", "", Result$X2)
Result$X2 = gsub(" mi/mo", "", Result$X2)
Result$X2 = gsub("[[:space:]]", "", Result$X2)
Result$X2 = gsub(",", "", Result$X2)
Result$X2 = as.numeric(Result$X2)

# 6mo
Result$X3 = as.character(Result$X3)
Result$X3 = gsub("6mo", "", Result$X3)
Result$X3 = gsub(":", "", Result$X3)
Result$X3 = gsub(" mi/mo", "", Result$X3)
Result$X3 = gsub("[[:space:]]", "", Result$X3)
Result$X3 = gsub(",", "", Result$X3)
Result$X3 = as.numeric(Result$X3)

# 3mo
Result$X4 = as.character(Result$X4)
Result$X4 = gsub("3mo", "", Result$X4)
Result$X4 = gsub(":", "", Result$X4)
Result$X4 = gsub(" mi/mo", "", Result$X4)
Result$X4 = gsub("[[:space:]]", "", Result$X4)
Result$X4 = gsub(",", "", Result$X4)
Result$X4 = as.numeric(Result$X4)

# Change column names
colnames(Result) = c( "Unit", "Asset_Type", "Mileage","Year", "Make", "Model",
                      "Owning_Dept", "Using_Dept", "Matched_URL", "CAMS_Property_Number",
                      "Fuel", "In_Service", "License_Tag", "Make_1", "Mileage_1", "Model_1",
                      "Owning_Department", "Parking_Location", "Unit_Description", "Unit_Number",
                      "Using_Department", "Utilization", "Vehicle_Identifier", "Year_1", "LTD",
                      "12mo (mi/mo)", "6mo (mi/mo)", "3mo (mi/mo)" )

########### Parse mileage column, bring into seperate rows for each of LTD, 12mo, 6mo, 3mo using regular expressions
Result$Mileage_1 = gsub("Â", "", as.character(Result$Mileage_1))

newCol2<-strsplit(as.character(Result$Mileage_1),';',fixed=TRUE)
Result<-data.frame(Result,do.call(rbind, newCol2))

# LTD
Result$X1 = as.character(Result$X1)
Result$X1 = gsub("LTD:      |LTD: ", "", Result$X1)
Result$X1 = gsub(" mi|mi", "", Result$X1)
Result$X1 = gsub(",", "", Result$X1)
Result$X1 = as.numeric(Result$X1)

# 12mo
Result$X2 = as.character(Result$X2)
Result$X2 = gsub("12mo", "", Result$X2)
Result$X2 = gsub(":", "", Result$X2)
Result$X2 = gsub(" mi", "", Result$X2)
Result$X2 = gsub("[[:space:]]", "", Result$X2)
Result$X2 = gsub(",", "", Result$X2)
Result$X2 = gsub(":", "", Result$X2)
Result$X2 = as.numeric(Result$X2)

# 6mo
Result$X3 = as.character(Result$X3)
Result$X3 = gsub("6mo", "", Result$X3)
Result$X3 = gsub(":", "", Result$X3)
Result$X3 = gsub(" mi", "", Result$X3)
Result$X3 = gsub("[[:space:]]", "", Result$X3)
Result$X3 = gsub(",", "", Result$X3)
Result$X3 = gsub(":", "", Result$X3)
Result$X3 = as.numeric(Result$X3)

# 3mo
Result$X4 = as.character(Result$X4)
Result$X4 = gsub("3mo", "", Result$X4)
Result$X4 = gsub(":", "", Result$X4)
Result$X4 = gsub(" mi|mi", "", Result$X4)
Result$X4 = gsub("[[:space:]]", "", Result$X4)
Result$X4 = gsub(",", "", Result$X4)
Result$X4 = gsub(":", "", Result$X4)
Result$X4 = as.numeric(Result$X4)

# Change column names
colnames(Result) = c( "Unit", "Asset_Type", "Mileage","Year", "Make", "Model",
                      "Owning_Dept", "Using_Dept", "Matched_URL", "CAMS_Property_Number",
                      "Fuel", "In_Service", "License_Tag", "Make_1", "Mileage_1", "Model_1",
                      "Owning_Department", "Parking_Location", "Unit_Description", "Unit_Number",
                      "Using_Department", "Utilization", "Vehicle_Identifier", "Year_1", "LTD (mi/mo)",
                      "12mo (mi/mo)", "6mo (mi/mo)", "3mo (mi/mo)", "LTD (mi)", "12mo (mi)",
                      "6mo (mi)", "3mo (mi)" )

########### Run SQL Queries

# Full dataset
write.csv(Result, file = "UCDavis_FleetServices_FULL.csv", row.names = F)

# AVG and COUNT 1-year mileage by fuel and unit description
AVGmileage = sqldf("select Fuel, Unit_Description, ROUND(AVG(`12mo (mi)`),0) as Average_Annual_Mileage, COUNT(Unit) as Number_Vehicles from Result group by Fuel, Unit_Description;")
write.csv(AltFuels, file = "UCDavis_FleetServices_All_Fuels.csv", row.names = F)

# Filter out alternative fuel vehicles (excluding FFVs, as no indication these are using E85)
AltFuels = sqldf("select * from AVGmileage where Fuel != 'UNLEADED' and Fuel != 'DIESEL' and Fuel != 'UNLEADED/ETHANOL' and Average_Annual_Mileage > 0;")
write.csv(AltFuels, file = "UCDavis_FleetServices_AFVs16.csv", row.names = F)

########### For future Queries, read in CSV rather than go through steps again
########### NOTE: Last data run was March 4, 2017
DF = read.csv("UCDavis_FleetServices_FULL.csv")
colnames(DF) = c( "Unit", "Asset_Type", "Mileage","Year", "Make", "Model",
                  "Owning_Dept", "Using_Dept", "Matched_URL", "CAMS_Property_Number",
                  "Fuel", "In_Service", "License_Tag", "Make_1", "Mileage_1", "Model_1",
                  "Owning_Department", "Parking_Location", "Unit_Description", "Unit_Number",
                  "Using_Department", "Utilization", "Vehicle_Identifier", "Year_1", "LTD (mi/mo)",
                  "12mo (mi/mo)", "6mo (mi/mo)", "3mo (mi/mo)", "LTD (mi)", "12mo (mi)",
                  "6mo (mi)", "3mo (mi)" )

########### FUTURE QUERIES
# Enter any desired future queries on the final dataframe here
# EXAMPLE: we want to identify the MY 2017 Ford Fusions in the fleet, as well as their fuel type (seperating Fusion gasoline, Fusion Hybrid, Fusion Energi)
Fusions = sqldf("select Year, Make, Model, Fuel, Vehicle_Identifier as VIN from DF where Make = 'FORD' and Model = 'FUSION'")
write.csv(Fusions, file = "Fusions.csv", row.names = F)
