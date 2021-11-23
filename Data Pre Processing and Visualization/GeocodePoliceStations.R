#Doing data wrangling on a CSV file containing details of police stations.

#Load libraries
library(base)
library(datasets)
library(graphics)
library(grDevices)
library(methods)
library(stats)
library(utils)
library(ggmap)
library(tmaptools)
library(dplyr)

# Read in the CSV data and store it in a variable 
origAddressPS <- read.csv("./police stations.csv", stringsAsFactors = FALSE)

origAddressPS <- origAddressPS %>% distinct()

#concat the different address columns
origAddressPS$Address <- paste(origAddressPS$Street.Address, ",", origAddressPS$Suburb, ",", origAddressPS$State,",", origAddressPS$Postcode,",",origAddressPS$Country)

# Geocoding a csv column of "addresses" of Police Stations in R

# geocoding the Police Stations
# this might take a while to run because each address in the dataframe needs to be geocoded
PS_tmaptools <- geocode_OSM(paste(origAddressPS$Address, sep = " "),
                             details = TRUE, as.data.frame = TRUE)

# extracting from the result only coordinates and address
PS_tmaptools <- PS_tmaptools[, c("lat", "lon", "display_name")]

#binding the result with original df
newAddressPS <- cbind(origAddressPS,PS_tmaptools)

# Write a CSV file containing the newly geocoded Dataframe to the working directory
write.csv(newAddressPS, "geocodedPS.csv", row.names=FALSE)

