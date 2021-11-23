#Doing data wrangling on a CSV file containing details of universities

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
origAddressUni <- read.csv("./Universities.csv", stringsAsFactors = FALSE)

# Geocoding a csv column of "Locations" of Universities in R

# geocoding the Police Stations
# this might take a while to run because each address in the dataframe needs to be geocoded
Uni_tmaptools <- geocode_OSM(paste(origAddressUni$Location, sep = " "),
                            details = TRUE, as.data.frame = TRUE)

# extracting from the result only coordinates and address
Uni_tmaptools <- Uni_tmaptools[, c("lat", "lon")]

#binding the result with original df
newAddressUni <- cbind(origAddressUni,Uni_tmaptools)

# Write a CSV file containing the newly geocoded Dataframe to the working directory
write.csv(newAddressUni, "geocodedUnis.csv", row.names=FALSE)

