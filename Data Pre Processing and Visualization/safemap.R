#Map to show areas in Victoria and their safety rating and median rental price

#load required libraries
library(base)
library(datasets)
library(graphics)
library(grDevices)
library(methods)
library(stats)
library(utils)
library(dplyr)
require(shiny)
require(leaflet)
require(ggmap)
require(tmaptools)
require(geosphere)
require(sf)
library(shiny)
library(leaflet)
library(ggmap)
library(tmaptools)
library(geosphere)
library(rgeos)
library(sf)
library(maptools)


#read the data for shapefile
vicmapdata <- rgdal::readOGR("./vic_lga_polygon_shp/VIC_LGA_POLYGON_SHP.shp")


#copy shape file data to a dataframe to see the layout of the data and examine it
df = data.frame(vicmapdata)


#remove duplicate layer IDs
vicmapdata <- vicmapdata[!duplicated(vicmapdata@data[ , c("LGA_PID")]),]

#a sf file for checking lat and long of user entry
vicmapdata2 <- rgdal::readOGR("./vic_lga_polygon_shp/VIC_LGA_POLYGON_SHP.shp")  %>% # this is SpatialPolygonDataFrame
  st_as_sf()%>%
  dplyr::select(ABB_NAME) %>% # keep only country name
  sf::st_transform(crs = 4326) %>%
  st_make_valid()

#read the csv file for safety and rent data
safetyrent<-read.csv("./safetyrent.csv",stringsAsFactors = FALSE)

#check for differences b/w shape file and the csv file in terms of LGA names
setdiff(safetyrent$LGA, df$ABB_NAME)


#fix the difference
safetyrent$LGA[safetyrent$LGA=="COLAC-OTWAY"] <- "COLAC OTWAY"

df = data.frame(vicmapdata)
states_shape1 <- gSimplify(vicmapdata, tol=0.001, topologyPreserve=TRUE)
df.polygon2 = SpatialPolygonsDataFrame(states_shape1, df)
df.polygon2@data <- left_join(df.polygon2@data, safetyrent, by=c("ABB_NAME"="LGA"))
fortest<-df.polygon2




#initialize reactive values
values <- reactiveValues()
values$flag=0

meansafety <- round(mean(as.numeric(safetyrent$SafetyMetric)),1)

#create new column to store star ratings
fortest@data$stars[fortest@data$SafetyMetric==5] <- "★★★★★"
fortest@data$stars[fortest@data$SafetyMetric==4] <- "★★★★"
fortest@data$stars[fortest@data$SafetyMetric==3] <- "★★★"
fortest@data$stars[fortest@data$SafetyMetric==2] <- "★★"
fortest@data$stars[fortest@data$SafetyMetric==1] <- "★"

#UI side
ui <- fluidPage(
  fluidRow(
    #leaflet
    column(8, offset=0, leafletOutput("safetyplot")),
    column(4,
           #Text box input for user location
           fluidRow(column(12, textInput(inputId = "userlocation",
                                         label = "Enter your area/postcode"),
                           #user guide
                           helpText("Example: Clayton"),
                           actionButton(inputId = "reset", label = "Default View"))),
           #Display captions and descriptions for the map
           fluidRow(column(12, p(h5(textOutput("caption1"))),
                           p(h5(textOutput("caption2"))),
                           p(h5(textOutput("mean3")))
           )),
           #user guide
           fluidRow(column(12, p(h4(strong("Usability guide:"))),
                           p(h5(strong("1. More stars = Safer Area (refer the legend) "))),
                           p(h5(strong("2. Use the text box to search for location"))),
                           p(h5(strong("3. Click or hover over the map to see the safety rating for each area"))),
                           p(h5(strong("4. Click on the marker to view the entered location"))),
                           p(h5(strong("5. Click on Default View to clear the Markers")))))))
)

#server side
server <- function(input, output,session) {
  
  #create labels for hover action
  mylabels <- paste(
    "LGA: ", fortest@data$ABB_NAME, "<br/>",
    "Safety rating : ", fortest@data$stars, "<br/>"
  ) %>%
    lapply(htmltools::HTML)
  
  #create colour palette
  factpal <- colorFactor(palette = "Blues", unique(fortest@data$stars))
  
  #map title and references 
  htmltitle <- "<h5> Safety Ratings of Victorian LGAs based on Crime Data</h5>"
  
  
  #render leaflet
  output$safetyplot <- renderLeaflet({
    #geocode user input
    address_latlon <- geocode_OSM(paste(input$userlocation, " VIC"),
                                  details = TRUE, as.data.frame = TRUE)
    
    #if location doesnt exist
    if(is.null(address_latlon)){
      setlon=144.964600
      setlat= -37.020100
    }else{
      #if the location exists
      setlon=address_latlon$lon
      setlat=address_latlon$lat
    }
    
    #leaflet map
    m <- leaflet(fortest) %>%
      addProviderTiles(providers$CartoDB, options = providerTileOptions(minZoom=6, maxZoom=18))%>%
      addPolygons(
        fillColor = ~factpal(fortest@data$stars), 
        stroke = TRUE, 
        color = 'Black', 
        fillOpacity = 1,
        weight = 1.5, 
        label = mylabels,
        layerId = fortest@data$ABB_NAME,
        #highlight on hover
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        #label options
        labelOptions = labelOptions( 
          style = list("font-weight" = "normal", padding = "3px 8px"), 
          textsize = "13px", 
          direction = "auto"
        )
      )%>%
      #adding legend
      addLegend( pal=factpal, 
                 values=~fortest@data$stars, 
                 opacity=1, 
                 title = "Safety Rating", 
                 position = "bottomleft" 
      ) %>%
      #adding title and references
      addControl(html=htmltitle, position = "topright") 
    output$caption1 <- renderText("")
    output$caption2 <- renderText("")
    
    
    
    
    #if user input is valid then show user location marker on the map
    if(input$userlocation!=""&&!is.null(address_latlon)){
      #get coordinates of user location
      coords_df <- tibble::tribble(
        ~lat, ~lon,
        setlat,     setlon
      )%>% 
        dplyr::mutate(id = dplyr::row_number())
      #convert into shape file for checking
      coords_sf <- coords_df %>% 
        tidyr::pivot_longer(cols = 1:2,
                            names_to = "coord_type",
                            values_to = "coord_data") %>% 
        tidyr::separate(col = coord_type, into = c("coord_type", "set"), sep = "_") %>% 
        tidyr::pivot_wider(names_from = coord_type, values_from = coord_data) %>% 
        sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
      # join columns, if you want a country only if the point is within its borders
      within_sf <- sf::st_join(x = coords_sf, 
                               y = vicmapdata2,
                               join = sf::st_within)
      values$safety <- fortest@data$SafetyMetric[fortest@data$ABB_NAME==within_sf$ABB_NAME]
      values$stars <- fortest@data$stars[fortest@data$ABB_NAME==within_sf$ABB_NAME]
      #create labels for hovering display
      mylabels <- paste(
        "Safety Rating: ", values$stars,
        "<br/>", 
        " User Location: ", address_latlon$display_name
      ) %>%
        lapply(htmltools::HTML)
      leafletProxy("safetyplot") %>%
        setView(setlon, setlat, zoom = 10)%>%
        addMarkers(lng = setlon, lat = setlat, label = mylabels)
      values$user <- paste("User Location: ",address_latlon$display_name)
      output$caption1 <- renderText(values$user)
      output$caption2 <- renderText(paste("Safety Rating of ", address_latlon$display_name, " (The more stars, the safer): ",values$stars))
    }
    
    m
    
    
    
  })
  
  #render text captions
  meansafety <- paste("Average Safety Rating of Victoria: ★★★ ")
  output$mean3 <- renderText(meansafety)
  
  # #if user presses default view
  observeEvent(input$reset, {
    if(input$reset-values$flag==1){
      leafletProxy("safetyplot")%>%
        clearMarkers() %>%
        setView(144.964600, -37.020100, zoom = 6)
      updateTextInput(session, "userlocation", value = "")
      output$caption1 <- renderText("")
      output$caption2 <- renderText("")
      values$flag=values$flag+1
    }
    
  })
  #Render text for Captions based on selected location(map click)
  observeEvent(input$safetyplot_shape_click, { # update the location selectInput on map clicks
    values$area <- input$safetyplot_shape_click$id
    values$areas <- paste("Selected Area: ",values$area)
    
    leafletProxy("safetyplot")%>%
      clearMarkers() %>%
      setView(lng=input$safetyplot_shape_click$lng, lat=input$safetyplot_shape_click$lat,zoom=8)%>%
      addMarkers(lng = input$safetyplot_shape_click$lng, lat = input$safetyplot_shape_click$lat, label = paste("Clicked Location: ", values$area))
    
    output$caption1 <- renderText(values$areas)
    values$safety <- fortest@data$stars[fortest@data$ABB_NAME==values$area]
    values$safety <- paste("Safety Rating of ", values$area, " (The more stars, the safer): ",values$safety, " Stars")
    output$caption2 <- renderText(values$safety)
  })
  
  
}

shinyApp(ui = ui, server = server)


