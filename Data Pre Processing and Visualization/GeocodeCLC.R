# Geocoding a csv column of "addresses" of Community Legal Centres in R

#load required libraries
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
origAddressCLC <- read.csv("Community Legal Centres.csv", stringsAsFactors = FALSE)

# geocoding the Community Legal Centres
#this might take a while to finish executing since it has to geocode all the addresses
CLC_tmaptools <- geocode_OSM(paste(origAddressCLC$Street.Address, sep = " "),
                              details = TRUE, as.data.frame = TRUE)

# extracting from the result only coordinates and address
CLC_tmaptools <- CLC_tmaptools[, c("lat", "lon", "display_name")]

#binding the result with original df
newAddressCLC <- cbind(origAddressCLC,CLC_tmaptools)

# Write a CSV file containing the newly geocoded Dataframe to the working directory
write.csv(newAddressCLC, "geocodedCLC.csv", row.names=FALSE)

