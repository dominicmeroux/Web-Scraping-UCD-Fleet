# UC Davis Public Fleet Inventory Detail

This R program scrapes data from the publicly-available UC Davis rolling fleet inventory to obtain insights including alternative fuel vehicle counts and annual mileage. 

The rolling inventory is available [here](http://fleet.ucdavis.edu/Fleet/inService?assetType=ALL&category=roll), and each vehicle has additional detail by clicking on the unit number. 

The R program takes the following steps:

1. Parse the HTML link to read in the In-Service Units table

2. Match each row to the respective unit number hyperlink, and read in each of those tables which have useful data like fuel type, mileage breakdown, and utilization rates

3. Change the unit tables from long to wide format, such that you have one row per vehicle unit number, and bind this table with the In-Service Units table

4. Parse the Utilization and Mileage rows to seperate LTD, 12mo, 6mo, and 3mo values into seperate columns

4. Run SQL queries on the final dataframe, and save the final dataframe as a CSV such that for any future analysis of the fleet data, you can simply read in this CSV file instead of scraping the page again. Of course, over time you may want to refresh your data and re-run the full program. NOTE: this program was last tested March 4, 2017, so if there are any errors, this may be due to some update of the structure of the web page. 

