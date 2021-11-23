# VICSAFE

## DATA SCRIPTS AND DATASETS

### INTRODUCTION:

The two main functionalities of VicSafe involve visualizing how safe different Local Government Areas (LGAs) in Victoria are and also 
displaying legal and safety services (Police Stations and Community Legal Centres) across Victoria to the users. For this purpose data 
was obtained from different sources: 

|                                                   Data Sources (Open Datasets)                                                  |                              |                             |                                       |                                                                          |                                                                                 |                           |                                                                                                       |
|:-------------------------------------------------------------------------------------------------------------------------------:|:----------------------------:|:---------------------------:|:-------------------------------------:|:------------------------------------------------------------------------:|:-------------------------------------------------------------------------------:|:-------------------------:|:-----------------------------------------------------------------------------------------------------:|
| Names                                                                                                                           | Physical access              | Frequency of source updates | Frequency of Iteration system updates | Granularity                                                              | Copyright / licensing details                                                   | Type of Data              | Attributes                                                                                            |
| Crime Incidents in Victoria  https://www.crimestatistics.vic.gov.au/crime-statistics/latest-crime-data-by-area                  | Available as csv             | 3 months                    | 4 months                              | Aggregate number of crime incidents in Victoria by LGA                   | Creative commons http://creativecommons.org/licen ses/by/3.0/                   | Tabular Data              | Local Government Area (string), Police Region (string), Criminal incidents in 2012, 2013…2021 (int)   |
| Open Streep Map      https://api.openstreetmap.org/                                                                             | API                          | Daily                       | Fortnightly                           | Geospatial data whose granularity is as small as the size of the polygon | Open Data Commons Open Database License https://www.openstreetmap.org/copyright | Geospatial data using API | -                                                                                                     |
| Community Legal Centres in Victoria https://clcs.org.au/sites/default/files/2021-03/Member%20org%20directory%20March%202021.pdf | PDF                          | 3 months                    | 4 months                              | Directory of all community legal centres in Victoria                     | PDF published by the Community Legal Centres Australia                          | Text data                 | Name of Legal Centre, Telephone Number, Fax Number, Website, location, email-id, , Specialty Services |
| Police Stations in Victoria https://www.police.vic.gov.au/location                                                              | Online data                  | Not specified               | 4 months                              | Directory of all police stations in Victoria                             | © Copyright State Government of Victoria                                        | Text Data                 | Opening Hours, Street Address, Suburb, State, Postcode, Phone, Fax, Email                             |
| Median Rents by LGA https://www.dhhs.vic.gov.au/publications/rental-report                                                      | Available as csv             | 3 months                    | 4 months                              | Median rents for different LGAs across Victoria                          | Creative commons http://creativecommons.org/licen ses/by/3.0/                   | Tabular Data              | Region, LGA, count, median rent, number of bedrooms                                                   |
| Fatigue and Road Safety  http://casr.adelaide.edu.au/rsr/RSR2008/WilliamsonA.pdf                                                | Available as research paper  | Never                       | 4 months                              | Relation of fatigue to road safety                                       | Research Paper published by University of New South Wales                       | Research Data             | -                                                                                                     |
| Victorian Universities https://universityreviews.com.au/list-of-universities/victoria/                                          | Online data                  | Not specified               | 4 months                              | List of all universities and campuses in Victoria                        |                                                                                 | Tabular Data              | University Name, Campus Location                                                                      |
| Shapefile of Victoria  https://data.gov.au/dataset/ds-dga-bdf92691-c6fe-42b9-a0e2-a4cd716fa811/details?q=                       | Available online as shp file | Quarterly                   | 4 months                              | Shapefile of administrative boundaries of Victoria                       | Creative Commons Attribution 4.0 International                                  | Shapefile data            | IDs, Area, Polygon information                                                                        |

### DATA COLLECTION:

Both the data of police stations and Community Legal Centres in Victoria were available online as text data rather than easily downloadable format as you can see below:

<img width="50%" alt="Screen Shot 2021-08-26 at 2 00 54 pm" src="https://user-images.githubusercontent.com/89060866/130898533-dc18b095-39d2-4464-ba97-da2169a4d339.png"><img width="50%" alt="Screen Shot 2021-08-26 at 2 03 50 pm" src="https://user-images.githubusercontent.com/89060866/130898760-69f9cf0a-52be-4134-8aee-41a70d948dc9.png">

Since web scraping isn't permitted, this data was manually updated into 2 csv files:

* Police Stations: https://github.com/Mack-zhou/TA21/blob/main/IE/police%20stations.csv

* Community Legal Centres: https://github.com/Mack-zhou/TA21/blob/main/IE/Community%20Legal%20Centres.csv

The rest of the datasets were easily obtained:

* Rental Data was downloadable as an Excel spreadsheet as seen at: https://github.com/Mack-zhou/TA21/blob/main/IE/Quarterly%20median%20rents%20by%20local%20government%20area%20-%20March%20Quarter%202021.xlsx

* Crime Data per LGAs in Victoria was downloadable as an Excel spreadsheet as seen at: https://github.com/Mack-zhou/TA21/blob/main/IE/Data_Tables_LGA_Criminal_Incidents_Year_Ending_March_2021.xlsx

* Shapefile of Victoria was downloadable as a zipped shapefile. The unzipped folder is seen at: https://github.com/Mack-zhou/TA21/tree/main/IE/vic_lga_polygon_shp


### DATA WRANGLING:

Now that the data is collected, data wrangling could be performed. Data Wrangling was performed using RStudio. The corresponding files are included in the repository
and explained below:

* Wrangling Police Station Data - https://github.com/Mack-zhou/TA21/blob/main/IE/GeocodePoliceStations.R

In this file, duplicate entries are removed and address columns are merged to make one new column to help geocode the locations. Geocoding
is performed using the geocode_OSM() function which makes use of the OpenStreetMap API to help geocode the addresses.

The results are then written to a new CSV file: https://github.com/Mack-zhou/TA21/blob/main/IE/geocodedPS.csv

* Wrangling Community Legal Centres Data - https://github.com/Mack-zhou/TA21/blob/main/IE/GeocodeCLC.R

In this file,the address columns is used to help geocode the locations. Geocoding is performed using the geocode_OSM() 
function which makes use of the OpenStreetMap API to help geocode the addresses.

The results are then written to a new CSV file: https://github.com/Mack-zhou/TA21/blob/main/IE/geocodedCLC.csv

* Wrangling Crime and Rent Data: https://github.com/Mack-zhou/TA21/blob/main/IE/Safety_Rent_Wrangling.R

Here, Crime Data is read in from the excel sheets and then filtered out by year. Only data for the past 3 years is considered for creating a safety metric.
Danger ratings are assigned to both location types and offense types based on their relevance in the context of an international student in Australia.
Homicides, assault, sexual offences, blackmail, stalking, harrassment, threatening behaviour are all given high danger ratings while offences which do not affect 
international students directly like judicial offences are given lower ratings. Similarly danger ratings for location types are also assigned, with private dwellings and educational location types having higher danger ratings. Location danger ratings are scaled down since they are less relevant than the offense types when it comes to danger.

|                   Offense type                  | Rating |       Location Type      | Rating |
|:-----------------------------------------------:|:------:|:------------------------:|:------:|
| Homicide and related offences                   | 10     | Dwelling - private       | 5      |
| Assault and related offences                    | 9      | Dwelling  - non-private  | 4      |
| Sexual offences                                 | 9      | Grounds/surrounding land | 2      |
| Abduction and related offences                  | 8      | Education                | 5      |
| Robbery                                         | 7      | Health                   | 4      |
| Blackmail and extortion                         | 8      | Public Transport         | 5      |
| Stalking, harassment and threatening behaviour  | 8      | Other Transport          | 3      |
| Dangerous and negligent acts endangering people | 7      | Justice                  | 2      |
| Arson                                           | 7      | Open Space               | 3      |
| Property damage                                 | 5      | Street/footpath          | 4      |
| Burglary/Break and enter"                       | 7      | Other community location | 3      |
| Theft                                           | 6      | Admin/professional       | 2      |
| Deception                                       | 6      | Retail                   | 2      |
| Drug dealing and trafficking                    | 3      | Manufacturing            | 1      |
| Cultivate or manufacture drugs                  | 1      | Agricultural             | 1      |
| Drug use and possession                         | 2      | Recreational             | 2      |
| Other drug offences                             | 1      | Other location           | 1      |
| Weapons and explosives offences                 | 6      | Religious                | 2      |
| Disorderly and offensive conduct                | 6      | Financial                | 1      |
| Public nuisance offences                        | 6      | Wholesale                | 1      |
| Justice procedures                              | 1      | Warehousing/storage      | 1      |
| Breaches of orders                              | 1      |                          |        |
| Transport regulation offences                   | 1      |                          |        |
| Other government regulatory offences            | 1      |                          |        |
| Miscellaneous offences                          | 2      |                          |        |
| Public security offences                        | 6      |                          |        |
| Bribery                                         | 1      |                          |        |
| Regulatory driving offences                     | 4      |                          |        |


These ratings are then multiplied by the number of offenses per 100,000 of the population. The mean of this rating is then aggregated over year, offense/location 
type and the LGA. 

The two resultant ratings are then combined in a skewed calculation, with the skew favouring the danger metric based on offense type in a 7:3 ratio.

The plot of the final rating is then analyzed before the values are split into five different ratings from 1-5, 5 being the safest.

Next the median rent data is read in from the excel sheet for both 1 bedroom and 2 bedroom apartments. No other properties were examined because most international students choose to rent these for the most part as can be evidenced by the same being displayed in university websites: https://www.monash.edu/accommodation/accommodation/off-campus-options/renting-costs 

Only the latest data is considered, hence all the other columns are removed. The LGA names are changed to match those from the crime data. Then these dataframes are joined with the safety metric dataframe. Column names are changed for ease and to make it more legible and the LGA names are all changed to upper case to match the shape file data.

The results are then written to a new CSV file: https://github.com/Mack-zhou/TA21/blob/main/IE/safetyrent.csv


### Creating the RShiny Applications.

#### Application 1 - The safety ratings map: https://github.com/Mack-zhou/TA21/blob/main/IE/safemap.R

RShiny is used to create this application. 

Read the shapefile and remove duplicate layerIDs to avoid issues while plotting the map. Next, read the CSV file containing the safety metric and rent data.

For an RShiny application to work and to include interactivity, we use both a UI and a Server side in the application.

* UI side: This part deals with what the User sees on the screen. Here using FluidPage layout, the layout for the maps, the textbox for the user to enter locations and the captions are all set.

* Server side: In the server side, the user input is geocoded using the OpenStreetMap API and the map is designed using leaflet. The boundaries are drawn with the help of the shape files and a choropleth map is designed by setting a colour palette for the different safety ratings. Legends, title and references are also included here. Labels are also designed to show up on hovering over the markers. Clicking the map shapes (LGAs) dynamically displays information about that area within the caption/description on the side.

Run the app to display the application.

#### Application 2 - The services map: https://github.com/Mack-zhou/TA21/blob/main/IE/serviceapp.R

RShiny is used to create this application. 

Read the CSV files containing information about Police Stations and Community Legal Centres. Remove duplicate entries if any and make the column names uniform for ease of coding.

For an RShiny application to work and to include interactivity, we use both a UI and a Server side in the application.

* UI side: This part deals with what the User sees on the screen. Here using FluidPage layout, the layout for the maps, the radiobuttons to select the service, the textbox for the user to enter locations and the captions/usability guide are all set.

* Server side: In the server side, the dataset for plotting the markers is updated based on the user input. If the user enters a valid location, this is geocoded using OpenStreetMap API and a distance matrix of the services to the entered location is created and filtered to only the four closest services. The map is designed using leaflet and labels are added to be displayed on hovering and popups are added to display additional information up on clicking. If there is no user input all the services are shown as circle markers. If the user enters a valid location, then a user marker is added to the leaflet map and the four closest services are displayed as circle markers, with the zoom and view set to focus on this location. 

Run the app to display the application.

### Deployment

After the creation of the two Apps, they were then published on https://www.shinyapps.io/.

* Safety ratings map: https://vicsafe.shinyapps.io/IEProject2/
* Services map: https://vicsafe.shinyapps.io/IEProject/

### Plugins in the website

After publishing the apps, the RShiny applications were then incorporated into the project website using the iframe plugin.

## Usage 

Navigate to the main repository- https://github.com/Mack-zhou/TA21 - click on code and then Download Zip option

![record1](https://user-images.githubusercontent.com/89060866/130928869-1a087950-b9f2-47ee-a2b5-a0c3a7fe9f16.gif)

Unzip the file and open the IE folder. Since all the wrangling tasks have been performed already, the apps can be run straight away. Ensure required libraries have already been installed in your R environment using the install.packages("library name") function. 

Open safemap.R to run the safety map application and open serviceapp.R to run the services application.

After opening the file in RStudio, click on Run App in the top right.

safemap.R

![record3](https://user-images.githubusercontent.com/89060866/130931643-f70e1e2c-4fe7-4b7c-9988-5be88a6983b9.gif)

serviceapp.R

![record4](https://user-images.githubusercontent.com/89060866/130931657-e30f2601-7391-4dd0-b0c0-7bbc23a83264.gif)


Freely play around with both apps.


*All the data used for this project was open source.*

