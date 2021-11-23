#DATA WRANGLING FOR CRIME DATA AND RENT DATA

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
library(readxl)

#read the excel sheet for crime data by LGA and offense type
LGA_2 <- read_excel("./Data_Tables_LGA_Criminal_Incidents_Year_Ending_March_2021.xlsx", 
                    sheet = "Table 02")

#read the excel sheet for crime data by LGA and location type
LGA_4 <- read_excel("./Data_Tables_LGA_Criminal_Incidents_Year_Ending_March_2021.xlsx", 
                    sheet = "Table 04")

#looking at the different offence types
unique(LGA_2$`Offence Division`)
unique(LGA_2$`Offence Subdivision`)
unique(LGA_2$`Offence Subgroup`)

#looking at the different location types
unique(LGA_4$`Location Subdivision`)

#create a cut-off year for the data being used. 
#We are using only data from the last 3 years to develop the safety metric
flag= max(LGA_2$Year)-3

#filter the data for only the past 3 years
LGA_2<- LGA_2[LGA_2$Year >flag,]
LGA_4<- LGA_4[LGA_4$Year >flag,]

#Set danger ratings for each offense based on how much it applies to an international student
#some offenses are more dangerous than others in this context.
LGA_2$Danger[LGA_2$`Offence Subdivision`=="A10 Homicide and related offences"] <- 10
LGA_2$Danger[LGA_2$`Offence Subdivision`=="A20 Assault and related offences"] <- 9 
LGA_2$Danger[LGA_2$`Offence Subdivision`=="A30 Sexual offences"] <- 9 
LGA_2$Danger[LGA_2$`Offence Subdivision`=="A40 Abduction and related offences"] <-8 
LGA_2$Danger[LGA_2$`Offence Subdivision`=="A50 Robbery"] <- 7 
LGA_2$Danger[LGA_2$`Offence Subdivision`=="A60 Blackmail and extortion"] <- 8 
LGA_2$Danger[LGA_2$`Offence Subdivision`=="A70 Stalking, harassment and threatening behaviour"] <- 8 
LGA_2$Danger[LGA_2$`Offence Subdivision`=="A80 Dangerous and negligent acts endangering people"] <- 7 
LGA_2$Danger[LGA_2$`Offence Subdivision`=="B10 Arson"] <-7 
LGA_2$Danger[LGA_2$`Offence Subdivision`=="B20 Property damage"] <- 5 
LGA_2$Danger[LGA_2$`Offence Subdivision`=="B30 Burglary/Break and enter"] <-7 
LGA_2$Danger[LGA_2$`Offence Subdivision`=="B40 Theft"] <-6 
LGA_2$Danger[LGA_2$`Offence Subdivision`=="B50 Deception"] <-6 
LGA_2$Danger[LGA_2$`Offence Subdivision`=="C10 Drug dealing and trafficking"] <-3 
LGA_2$Danger[LGA_2$`Offence Subdivision`=="C20 Cultivate or manufacture drugs"] <-1 
LGA_2$Danger[LGA_2$`Offence Subdivision`=="C30 Drug use and possession"] <-2 
LGA_2$Danger[LGA_2$`Offence Subdivision`=="C90 Other drug offences"] <-1 
LGA_2$Danger[LGA_2$`Offence Subdivision`=="D10 Weapons and explosives offences"] <-6 
LGA_2$Danger[LGA_2$`Offence Subdivision`=="D20 Disorderly and offensive conduct"] <-6 
LGA_2$Danger[LGA_2$`Offence Subdivision`=="D30 Public nuisance offences"] <- 6
LGA_2$Danger[LGA_2$`Offence Subdivision`=="E10 Justice procedures"] <- 1
LGA_2$Danger[LGA_2$`Offence Subdivision`=="E20 Breaches of orders"] <- 1
LGA_2$Danger[LGA_2$`Offence Subdivision`=="F20 Transport regulation offences"] <- 1
LGA_2$Danger[LGA_2$`Offence Subdivision`=="F30 Other government regulatory offences"] <- 1
LGA_2$Danger[LGA_2$`Offence Subdivision`=="F90 Miscellaneous offences"] <- 2
LGA_2$Danger[LGA_2$`Offence Subdivision`=="D40 Public security offences"] <- 6
LGA_2$Danger[LGA_2$`Offence Subdivision`=="B60 Bribery"] <- 1
LGA_2$Danger[LGA_2$`Offence Subdivision`=="F10 Regulatory driving offences"] <- 4


#Calculate a metric using the danger rating and offenses per 100000 people
LGA_2$CalculatedDanger <- LGA_2$Danger*LGA_2$`LGA Rate per 100,000 population`

#change the column names for ease
names(LGA_2)[names(LGA_2) == "Offence Subdivision"] <- "Offence"
names(LGA_2)[names(LGA_2) == "Local Government Area"] <- "LGA"

#aggregate mean ratings over years, LGAs and offenses
LGA_2 <- aggregate(CalculatedDanger~Year+Offence+LGA,LGA_2,mean)

#aggregate mean ratings again over offense type and LGA
LGA_2 <- aggregate(CalculatedDanger~Offence+LGA, LGA_2, mean)

#aggregate mean ratings finally over only LGA
LGA_2 <- aggregate(CalculatedDanger~LGA, LGA_2, sum)

#Set danger ratings for each location based on how much it applies to an international student
#some locations are more relevant than others in this context.
#since location isn't as important as the crime itself, the ratings are downgraded to represent that
LGA_4$Danger[LGA_4$'Location Subdivision' == "11 Dwelling - private"] <- 5
LGA_4$Danger[LGA_4$'Location Subdivision' == "12 Dwelling  - non-private"] <- 4
LGA_4$Danger[LGA_4$'Location Subdivision' == "13 Grounds/surrounding land"] <- 2
LGA_4$Danger[LGA_4$'Location Subdivision' == "21 Education"] <- 5
LGA_4$Danger[LGA_4$'Location Subdivision' == "22 Health"] <- 4
LGA_4$Danger[LGA_4$'Location Subdivision' == "24 Public Transport"] <- 5
LGA_4$Danger[LGA_4$'Location Subdivision' == "25 Other Transport"] <- 3
LGA_4$Danger[LGA_4$'Location Subdivision' == "26 Justice"] <- 2
LGA_4$Danger[LGA_4$'Location Subdivision' == "27 Open Space"] <- 3
LGA_4$Danger[LGA_4$'Location Subdivision' == "28 Street/footpath"] <- 4
LGA_4$Danger[LGA_4$'Location Subdivision' == "29 Other community location"] <- 3
LGA_4$Danger[LGA_4$'Location Subdivision' == "31 Admin/professional"] <- 2
LGA_4$Danger[LGA_4$'Location Subdivision' == "33 Retail"] <- 2
LGA_4$Danger[LGA_4$'Location Subdivision' == "36 Manufacturing"] <- 1
LGA_4$Danger[LGA_4$'Location Subdivision' == "37 Agricultural"] <- 1
LGA_4$Danger[LGA_4$'Location Subdivision' == "38 Recreational"] <- 2
LGA_4$Danger[LGA_4$'Location Subdivision' == "39 Other location"] <- 1
LGA_4$Danger[LGA_4$'Location Subdivision' == "23 Religious"] <- 2
LGA_4$Danger[LGA_4$'Location Subdivision' == "32 Financial"] <- 1
LGA_4$Danger[LGA_4$'Location Subdivision' == "34 Wholesale"] <- 1
LGA_4$Danger[LGA_4$'Location Subdivision' == "35 Warehousing/storage"] <- 1

#Calculate a metric using the danger rating and offenses per 100000 people
LGA_4$CalculatedDanger <- LGA_4$Danger*LGA_4$`Incidents Recorded`/100

#change column names for ease of use
names(LGA_4)[names(LGA_4) == "Location Subdivision"] <- "Location"
names(LGA_4)[names(LGA_4) == "Local Government Area"] <- "LGA"

#aggregate mean ratings over years, LGAs and locations
LGA_4 <- aggregate(CalculatedDanger~Year+Location+LGA,LGA_4,mean)

#aggregate mean ratings again over location type and LGA
LGA_4 <- aggregate(CalculatedDanger~Location+LGA, LGA_4, mean)

#aggregate mean ratings finally over only LGA
LGA_4 <- aggregate(CalculatedDanger~LGA, LGA_4, sum)

#initialize a new dataframe
Safety <- as.data.frame(LGA_2$LGA)

#Add LGA names to this dataframe
names(Safety)[names(Safety) == "LGA_2$LGA"] <- "LGA"

#Add a cumulative danger rating skewd towards offense types
Safety$DangerRating <- LGA_2$CalculatedDanger*0.7/100+LGA_4$CalculatedDanger*0.3

#plot the ratings to see the distribution
plot(Safety$DangerRating)  

#Give a safety rating from 1-5 based on the distribution
Safety$SafetyMetric[Safety$'DangerRating' >= 120] <- 1 
Safety$SafetyMetric[Safety$'DangerRating' < 120 & Safety$'DangerRating' >= 90] <- 2
Safety$SafetyMetric[Safety$'DangerRating' < 90 & Safety$'DangerRating' >= 65] <- 3
Safety$SafetyMetric[Safety$'DangerRating' < 65 & Safety$'DangerRating' >= 40] <- 4
Safety$SafetyMetric[Safety$'DangerRating' < 40] <- 5

#read the spreadsheet containing median rents for 1 bedroom flat
Rent_1 <- read_excel("./Quarterly median rents by local government area - March Quarter 2021.xlsx", 
                    sheet = "1br flat")

#get the number of columns
cols <- ncol(Rent_1)

#Filter only the second (LGA) and last column (latest median rental), the rest are not applicable
Rent_1 <- Rent_1[c(2,cols)]

#see what LGA names dont match
setdiff(Safety$LGA, Rent_1$...2)

#change the LGA name so that it matches
Rent_1$...2[Rent_1$...2 == "Mornington Penin'a"] <- "Mornington Peninsula"

#do a left join to add the rental data to the safety dataframe
safety_rent <- left_join(Safety, Rent_1, by = c("LGA"="...2"))

#rename the column to make it more legible
names(safety_rent)[names(safety_rent) == "...178"] <- "br1_flat"



#read the spreadsheet containing median rents for 2 bedroom flat
Rent_2 <- read_excel("./Quarterly median rents by local government area - March Quarter 2021.xlsx", 
                     sheet = "2br Flat")

#get the number of columns
cols <- ncol(Rent_2)

#Filter only the second (LGA) and last column (latest median rental), the rest are not applicable
Rent_2 <- Rent_2[c(2,cols)]

#see what LGA names dont match
setdiff(Safety$LGA, Rent_2$...2)

#change the LGA name so that it matches
Rent_2$...2[Rent_2$...2 == "Mornington Penin'a"] <- "Mornington Peninsula"

#do a left join to add the rental data to the safety dataframe
safety_rent <- left_join(safety_rent, Rent_2, by = c("LGA"="...2"))

#rename the column to make it more legible
names(safety_rent)[names(safety_rent) == "...178"] <- "br2_flat"

#change LGA names to uppercase
safety_rent[,"LGA"] = toupper(safety_rent[,"LGA"])

#write the dataframe to a csv file
write.csv(safety_rent, "safetyrent.csv", row.names=FALSE)

  