#Map to show services in Victoria 

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
library(shiny)
library(leaflet)
library(ggmap)
library(tmaptools)
library(geosphere)
library(rgeos)
library(maptools)
library(osrm)
library(png)
library(shinyjs)
library(shinydashboard)
library(shinydashboardPlus)
require(shinydashboard)
require(shinydashboardPlus)

#read the data
PS <- read.csv("./geocodedPS.csv", stringsAsFactors = FALSE)
CLC <- read.csv("./geocodedCLC.csv", stringsAsFactors = FALSE)
Uni <- read.csv("./geocodedUnis.csv", stringsAsFactors = FALSE)
newrow<- c("None", "None", "None", "None", "None", "None")
Uni <-rbind(Uni,newrow)
#edit the column names for uniformity
names(PS)[names(PS) == "Place"] <- "Name"
names(CLC)[names(CLC) == "Phone_No"] <- "Phone"

#remove any duplicate entries in the Geocoded police stations.
PS <- PS[!duplicated(PS[ , c("display_name")]),]

#intializing reactive values
values <- reactiveValues()

values$flag=0
values$flag1=0

psnumber = reactiveVal()
psnumber(148)
clcnumber = reactiveVal()
clcnumber(46)
psnumber1 = reactiveVal()
psnumber1(148)
clcnumber1 = reactiveVal()
clcnumber1(46)
values$clcnumber = 46
titleapp <- tags$a(href='http://vicsafe.tk',
                icon("desktop"), 'VicSafe')


#UI side
ui <- dashboardPage(skin='midnight',
                        dashboardHeader(title = titleapp),
                        dashboardSidebar(width=275,
                                         
                                         # The dynamically-generated user panel
                                         uiOutput("userpanel"),
                                         
                                         # Side Bar Menu
                                         sidebarMenu(style = "position: Scroll; overflow: visible;",id = "sidebarmenu",
                                                     
                                                     menuItem("Search by Location", tabName = "location", icon = icon("search-location")),
                                                     
                                                     menuItem("Search by University", tabName = "university", icon = icon("university")),
                                                     
                                                     conditionalPanel(
                                                       "input.sidebarmenu === 'location'",
                                                       # a. FILTERS
                                                       useShinyjs(),
                                                       div(id = "form",
                                                           tags$hr(),
                                                           radioButtons("service1", "Select your required service:",
                                                                        c("Police Stations" = "PS",
                                                                          "Community Legal Centres" = "CLC"), selected = "PS"),
                                                           textInput(inputId = "userlocation",
                                                                     label = "Enter your area/postcode"),
                                                           helpText("Example: Clayton"),
                                                           sliderInput("radius1", "Select the search radius", value = 10, min = 0, max = 100)),
                                                       column(6,offset = 6,height = 100,style='padding100px;',
                                                              actionButton("reset_button", "Reset",icon = icon("repeat")))
                                                     ),
                                                     conditionalPanel(
                                                       "input.sidebarmenu === 'university'",
                                                       # a. FILTERS
                                                       useShinyjs(),
                                                       div(id = "form",
                                                           tags$hr(),
                                                           radioButtons("service2", "Select your required service:",
                                                                        c("Police Stations" = "PS",
                                                                          "Community Legal Centres" = "CLC"), selected = "PS"),
                                                           selectInput(inputId = "Uni", #name of input
                                                                       label = "Select your University:", #label displayed in ui
                                                                       choices = as.character(unique(Uni$Name)),
                                                                       # calls unique values from the State column in the previously created table
                                                                       selected = "None"
                                                           ),
                                                           conditionalPanel(condition = "input.Uni !== null",
                                                                            uiOutput("Campus")),
                                                           sliderInput("radius2", "Select the search radius", value = 10, min = 0, max = 100),
                                                           column(6,offset = 6,height = 100,style='padding100px;',
                                                                   actionButton("reset_button1", "Reset",icon = icon("repeat")))
                                                       )
                                                     )
                                                     )
                        ),
                        dashboardBody(
                          
                          tabItems( 
                            tabItem(tabName = "location",
                                    fluidRow(column(10, offset = 0.5,h1("Searching for Services near User Location"))),
                                    
                                    box(
                                      title = "Usability Guide", 
                                      closable = FALSE, 
                                      width = 10,
                                      status = "warning", 
                                      solidHeader = FALSE, 
                                      collapsible = TRUE,
                                      collapsed= FALSE,
                                      fluidRow(column(12,p(h5(strong("1. Select the preferred method of searching from the sidebar"))),
                                                      p(h5(strong("2. Select the required service from the sidebar"))),
                                                      p(h5(strong("3. Enter a location in the search box in the sidebar to view the four closest services"))),
                                                      p(h5(strong("4. Use the slider in the sidebar to select a search radius"))),
                                                      p(h5(strong("5. Hover over the blue markers to view basic info"))),
                                                      p(h5(strong("6. Click on the markers to view detailed info and display route")))))
                                    ),
                                    br(),
                                    fluidRow(style="height:50px;",
                                             valueBoxOutput("count1",width = 3),
                                             valueBoxOutput("count2",width = 3),
                                             valueBoxOutput("count3",width = 3),
                                             valueBoxOutput("count4",width = 3)
                                    ),
                                    br(), 
                                    fluidRow(column(10, offset = 2.5,leafletOutput('serviceplot1',width=1000, height = 530))),
                                    br()
                            ),
                            tabItem(tabName = "university",
                                    fluidRow(column(10, offset = 0.5, h1("Searching for Services near University"))),
                                    
                                    box(
                                      title = "Usability Guide", 
                                      closable = FALSE, 
                                      width = 10,
                                      status = "warning", 
                                      solidHeader = FALSE, 
                                      collapsible = TRUE,
                                      collapsed = FALSE,
                                      fluidRow(column(12,p(h5(strong("1. Select the preferred method of searching from the sidebar"))),
                                                      p(h5(strong("2. Select the required service from the sidebar"))),
                                                      p(h5(strong("3. Select the University and Campus from the drop down menu to view the four closest services"))),
                                                      p(h5(strong("4. Use the slider in the sidebar to select a search radius"))),
                                                      p(h5(strong("5. Hover over the blue markers to view basic info"))),
                                                      p(h5(strong("6. Click on the markers to view detailed info and display route")))))
                                    ),
                                    br(),
                                    fluidRow(style="height:50px;",
                                             valueBoxOutput("count5",width = 3),
                                             valueBoxOutput("count6",width = 3),
                                             valueBoxOutput("count7",width = 3),
                                             valueBoxOutput("count8",width = 3)
                                    ),
                                    br(),
                                    fluidRow(column(10, offset = 2.5,leafletOutput('serviceplot2', width=1000, height = 530)))
                            )
                          )
                        )
)


#server side
server <- function(input, output,session) {
  output$count1 <- renderValueBox({
    valueBox(paste0(psnumber1()), "POLICE STATIONS", icon = icon("user-shield"),
             color = "green"
    )
  })
  # Value Box 2
  output$count2 <- renderValueBox({
    valueBox( paste0(clcnumber1()), "COMMUNITY LEGAL CENTRES", icon = icon("gavel"),
              color = "olive"
    )
  })
  
  
  # Value Box 3
  output$count3 <- renderValueBox({
    count3 <- "000"
    valueBox(paste0(count3), "EMERGENCY NUMBER", icon = icon("exclamation-circle"),
             color = "blue"
    )
  })
  
  # Value Box 4
  output$count4 <- renderValueBox({
    count4 <- "20.6 Mins"
    valueBox(paste0(count4), "AVG RESPONSE TIME OF VICPOLICE", icon = icon("hourglass-half"),
             color = "blue"
    )
  })

  # Value Box 1
  output$count5 <- renderValueBox({
    valueBox(paste0(psnumber()), "POLICE STATIONS", icon = icon("user-shield"),
             color = "green"
    )
  })
  # Value Box 2
  output$count6 <- renderValueBox({
    valueBox( paste0(clcnumber()), "COMMUNITY LEGAL CENTRES", icon = icon("gavel"),
              color = "olive"
    )
  })
  
  
  # Value Box 3
  output$count7 <- renderValueBox({
    count3 <- "000"
    valueBox(paste0(count3), "EMERGENCY NUMBER", icon = icon("exclamation-circle"),
             color = "blue"
    )
  })
  
  # Value Box 4
  output$count8 <- renderValueBox({
    count4 <- "20.6 Mins"
    valueBox(paste0(count4), "AVG RESPONSE TIME OF VICPOLICE", icon = icon("hourglass-half"),
             color = "blue"
    )
  })
  
  output$Campus <- renderUI({#creates Campus select box object called in ui
    
    data_available = Uni[Uni$Name == input$Uni, "Campus"]
    #creates a reactive list of available campus based on the University selection made
    
    selectInput(inputId = "campus", #name of input
                label = "Select your University Campus:", #label displayed in ui
                choices = unique(data_available), #calls list of available counties
                selected = unique(data_available)[1]
    )
  })
  
  
  selectedData1 <- reactive({if(input$service1=="PS"){
    if(input$userlocation=="")
    {
      psnumber1(148)
      clcnumber1(46)
      PS
      }
    #updating dataset to show 4 closest services based on input
    else{
      #geocode user input
      address_latlon <- geocode_OSM(paste(input$userlocation, " VIC"))
      if(is.null(address_latlon)){psnumber1(148)
        clcnumber1(46)
        PS}
      #calculate distance from user location to all services and store in a matrix
      else{dist <- distm(x = matrix(data = c(PS$lon, PS$lat), ncol = 2),
                         y = c(lon = address_latlon$coords[1], lat = address_latlon$coords[2]),
                         fun = distVincentySphere)
      dist2 <- distm(x = matrix(data = c(CLC$lon, CLC$lat), ncol = 2),
                    y = c(lon = address_latlon$coords[1], lat = address_latlon$coords[2]),
                    fun = distVincentySphere)
      
      radiuss <- as.numeric(input$radius1) *1000
      #distance slider option filter
      PS <- PS[dist < radiuss,]
      CLC <- CLC[dist2 < radiuss,]
      psnumber1(nrow(PS))
      clcnumber1(nrow(CLC))
      PS}
    }
  }
    else{
      if(input$userlocation=="")
      {
        psnumber1(148)
        clcnumber1(46)
        CLC}
      #updating dataset to show 4 closest services based on input
      else{
        #geocode user input
        address_latlon <- geocode_OSM(paste(input$userlocation, " VIC"))
        if(is.null(address_latlon)){
          psnumber1(148)
          clcnumber1(46)
          CLC}
        #calculate distance from user location to all services and store in a matrix
        else{
          dist2 <- distm(x = matrix(data = c(PS$lon, PS$lat), ncol = 2),
                           y = c(lon = address_latlon$coords[1], lat = address_latlon$coords[2]),
                           fun = distVincentySphere)
          dist <- distm(x = matrix(data = c(CLC$lon, CLC$lat), ncol = 2),
                           y = c(lon = address_latlon$coords[1], lat = address_latlon$coords[2]),
                           fun = distVincentySphere)

        radiuss <- as.numeric(input$radius1) *1000
        #distance slider fliter
        PS <- PS[dist2 < radiuss,]
        CLC <- CLC[dist < radiuss,]
        psnumber1(nrow(PS))
        clcnumber1(nrow(CLC))
        CLC}
      }
    }}
  )
  
  output$serviceplot1 <- renderLeaflet({
    #storing reactive data in a variable for ease
    fortest<- selectedData1()
    
    #create labels for hovering display
    mylabels <- paste(
      "Name : ", fortest$Name, "<br/>",
      "Street Address : ", fortest$Street.Address, "<br/>",
      "Phone No. : ", fortest$Phone
    ) %>%
      lapply(htmltools::HTML)
    
    #create popups on click based on selected service
    if(input$service1=="PS"){
      #popup for police stations
      mypopup <- paste(
        "Selected Service: Police Station", "<br/>",
        "Name: ", fortest$Name,"<br/>",
        "Street Address : ", paste(fortest$Street.Address, ", ", fortest$Suburb), "<br/>",
        "Phone No. : ", fortest$Phone, "<br/>",
        "Opening Hours : ", fortest$Opening.Hours, "<br/>",
        "Email ID: ", fortest$Email
      ) %>%
        lapply(htmltools::HTML)
      
    }
    else{
      #popup for community legal centres
      mypopup <- paste(
        "Selected Service: Community Legal Centre", "<br/>",
        "Name: ", fortest$Name,"<br/>",
        "Street Address : ", fortest$Street.Address, "<br/>",
        "Phone No. : ", fortest$Phone, "<br/>",
        "Website: ", fortest$Website, "<br/>",
        "Services Provided : ", fortest$Services.Provided
      ) %>%
        lapply(htmltools::HTML)
    }
    #title for ma\
    htmltitle <- "<h5> Services located in Victoria </h5>"
    basemap <- leaflet(selectedData1()) %>%
      setMaxBounds(lng1 = 140.584967, lat1 = -39.050336, lng2 = 155.423510, lat2 = -35.035311 )%>%
      # add different provider tiles
      addProviderTiles(
        "CartoDB.Voyager",
        options = providerTileOptions(minZoom=7, maxZoom=18),
      
        # give the layer a name
        group = "CartoDB.Voyager"
      ) %>%
      addProviderTiles(
        "Stamen.Toner",
        options = providerTileOptions(minZoom=7, maxZoom=18),
        group = "Stamen.Toner"
      ) %>%
      addProviderTiles(
        "Stamen.Terrain",
        options = providerTileOptions(minZoom=7, maxZoom=18),
        group = "Stamen.Terrain"
      ) %>%
      addProviderTiles(
        "Esri.WorldStreetMap",
        options = providerTileOptions(minZoom=7, maxZoom=18),
        group = "Esri.WorldStreetMap"
      ) %>%
      addProviderTiles(
        "OpenStreetMap",
        options = providerTileOptions(minZoom=7, maxZoom=18),
        group = "OpenStreetMap"
      ) %>%
      addProviderTiles(
        "CartoDB.Positron",
        options = providerTileOptions(minZoom=7, maxZoom=18),
        group = "CartoDB.Positron"
      ) %>%
      addProviderTiles(
        "Esri.WorldImagery",
        options = providerTileOptions(minZoom=7, maxZoom=18),
        group = "Esri.WorldImagery"
      ) %>%
      # add a layers control
      addLayersControl(
        baseGroups = c(
          "CartoDB.Voyager", "Stamen.Toner",
          "Stamen.Terrain", "Esri.WorldStreetMap",
          "OpenStreetMap", "CartoDB.Positron", "Esri.WorldImagery"
        ),
        # position it on the topleft
        position = "topleft"
      )
    #create leaflet
    m <- basemap%>%
      #setView(144.964600, -37.020100, zoom = 6)%>%
      addMarkers(~lon, ~lat,
                 label = mylabels,
                 popup = mypopup,
                 layerId = ~Name,
                 labelOptions = labelOptions(
                   style = list("font-weight" = "normal", padding = "3px 8px"),
                   textsize = "13px",
                   direction = "auto")) %>%#, weight = 3, radius=40,
      #color="#0000FF", stroke = TRUE, fillOpacity = 0.8) %>%
      addControl(html=htmltitle, position = "topright")
    
    #geocode input 
    address_latlon <- geocode_OSM(paste(input$userlocation, " VIC"),
                                  details = TRUE, as.data.frame = TRUE)
    if(input$userlocation!=""&&!is.null(address_latlon)){
      m<- m %>% 
        clearMarkers()%>%
        addCircles(lng = address_latlon$lon, lat = address_latlon$lat, label = paste("User Location: ", address_latlon$display_name), weight = 4, radius=80,
                   color="#0000FF", stroke = TRUE, fillOpacity = 0.8)%>%
        addMarkers(~lon, ~lat, label = mylabels,
                   popup = mypopup,
                   labelOptions = labelOptions(
                     style = list("font-weight" = "normal", padding = "3px 8px"),
                     textsize = "13px",
                     direction = "auto"))#, #weight = 1, #radius=30,
      #color="#0000FF", stroke = TRUE, fillOpacity = 0.8)
    }
    
    m
    
    
  })
  observeEvent(input$serviceplot1_marker_click, { # update the location selectInput on map clicks
    
    marker_lon = input$serviceplot1_marker_click$lng
    marker_lat = input$serviceplot1_marker_click$lat
    if(input$userlocation!="")
    {
      address_latlon <- geocode_OSM(paste(input$userlocation, " VIC"))
      if(is.null(address_latlon)){}
      else{  
        add_lat = address_latlon$coords[2]
        add_lon = address_latlon$coords[1]
        route = osrmRoute(c(add_lon,add_lat), c(marker_lon, marker_lat), overview = 'full')
        # # route_simple = osrmRoute(c(115.6813467,-32.0397559), c(150.3715249,-33.8469759), overview = 'simplified')
        route_summary = osrmRoute(c(add_lon,add_lat), c(marker_lon, marker_lat), overview = FALSE)
        leafletProxy("serviceplot1")%>%
          clearGroup("polys") %>%
          addPolylines(route$lon,route$lat, group = "polys",
                       label = paste(round(route_summary[1]), 'mins - ', round(route_summary[2]), 'km'), 
                       labelOptions = labelOptions(noHide = TRUE))%>%
          addMiniMap(position = "bottomleft", width = 150, height = 150,
                     collapsedWidth = 19, collapsedHeight = 19, zoomLevelOffset = -5,
                     zoomLevelFixed = FALSE, centerFixed = FALSE, zoomAnimation = TRUE,
                     toggleDisplay = TRUE, autoToggleDisplay = TRUE, minimized = TRUE,
                     aimingRectOptions = list(color = "#ff7800", weight = 1, clickable = TRUE),
                     shadowRectOptions = list(color = "#000000", weight = 1, clickable = TRUE,
                                              opacity = 0, fillOpacity = 0), strings = list(hideText = "Hide MiniMap", showText = "Show MiniMap"),
                     mapOptions = list())
      }}
  })
  
  selectedData2 <- reactive({
    selection_uni<- Uni %>%
      filter(Name %in% input$Uni)
    selection_uni <- selection_uni %>%
      filter(Campus %in% input$campus)
    dist <- distm(x = matrix(data = c(PS$lon, PS$lat), ncol = 2),
                  y = c(lon = as.numeric(selection_uni$lon), lat = as.numeric(selection_uni$lat)),
                  fun = distVincentySphere)
    dist2<- distm(x = matrix(data = c(CLC$lon, CLC$lat), ncol = 2),
                  y = c(lon = as.numeric(selection_uni$lon), lat = as.numeric(selection_uni$lat)),
                  fun = distVincentySphere)
    radiuss <- input$radius2*1000
    
    if(input$service2=="PS"){
      if(input$Uni=="None"){
        psnumber(nrow(PS))
        PS
      }
      else{
        #distance slider filter
        PS <- PS[dist < radiuss,]
        CLC <- CLC[dist2 < radiuss,]
        clcnumber(nrow(CLC))
        psnumber(nrow(PS))
        PS
      }
    }
    else{
      if(input$Uni=="None"){
        clcnumber(nrow(CLC))
        CLC
      }
      else{
        #distance slider filter
        PS <- PS[dist < radiuss,]
        CLC <- CLC[dist2 < radiuss,]
        clcnumber(nrow(CLC))
        psnumber(nrow(PS))
        CLC
      }
    }
  })
  
  output$serviceplot2 <- renderLeaflet({
    #storing reactive data in a variable for ease
    fortest<- selectedData2()
    
    #create labels for hovering display
    mylabels <- paste(
      "Name : ", fortest$Name, "<br/>",
      "Street Address : ", fortest$Street.Address, "<br/>",
      "Phone No. : ", fortest$Phone
    ) %>%
      lapply(htmltools::HTML)
    
    #create popups on click based on selected service
    if(input$service2=="PS"){
      #popup for police stations
      mypopup <- paste(
        "Selected Service: Police Station", "<br/>",
        "Name: ", fortest$Name,"<br/>",
        "Street Address : ", paste(fortest$Street.Address, ", ", fortest$Suburb), "<br/>",
        "Phone No. : ", fortest$Phone, "<br/>",
        "Opening Hours : ", fortest$Opening.Hours, "<br/>",
        "Email ID: ", fortest$Email
      ) %>%
        lapply(htmltools::HTML)
      
    }
    else{
      #popup for community legal centres
      mypopup <- paste(
        "Selected Service: Community Legal Centre", "<br/>",
        "Name: ", fortest$Name,"<br/>",
        "Street Address : ", fortest$Street.Address, "<br/>",
        "Phone No. : ", fortest$Phone, "<br/>",
        "Website: ", fortest$Website, "<br/>",
        "Services Provided : ", fortest$Services.Provided
      ) %>%
        lapply(htmltools::HTML)
    }
    #title for ma\
    htmltitle <- "<h5> Services located in Victoria </h5>"
    basemap <- leaflet(selectedData2()) %>%
      setMaxBounds(lng1 = 140.584967, lat1 = -39.050336, lng2 = 155.423510, lat2 = -35.035311 )%>%
      # add different provider tiles
      addProviderTiles(
        "CartoDB.Voyager",
        options = providerTileOptions(minZoom=6, maxZoom=18),
        # give the layer a name
        group = "CartoDB.Voyager"
      ) %>%
      addProviderTiles(
        "Stamen.Toner",
        options = providerTileOptions(minZoom=6, maxZoom=18),
        group = "Stamen.Toner"
      ) %>%
      addProviderTiles(
        "Stamen.Terrain",
        options = providerTileOptions(minZoom=6, maxZoom=18),
        group = "Stamen.Terrain"
      ) %>%
      addProviderTiles(
        "Esri.WorldStreetMap",
        options = providerTileOptions(minZoom=6, maxZoom=18),
        group = "Esri.WorldStreetMap"
      ) %>%
      addProviderTiles(
        "OpenStreetMap",
        options = providerTileOptions(minZoom=6, maxZoom=18),
        group = "OpenStreetMap"
      ) %>%
      addProviderTiles(
        "CartoDB.Positron",
        options = providerTileOptions(minZoom=6, maxZoom=18),
        group = "CartoDB.Positron"
      ) %>%
      addProviderTiles(
        "Esri.WorldImagery",
        options = providerTileOptions(minZoom=6, maxZoom=18),
        group = "Esri.WorldImagery"
      ) %>%
      # add a layers control
      addLayersControl(
        baseGroups = c(
          "CartoDB.Voyager", "Stamen.Toner",
          "Stamen.Terrain", "Esri.WorldStreetMap",
          "OpenStreetMap", "CartoDB.Positron", "Esri.WorldImagery"
        ),
        # position it on the topleft
        position = "topleft"
      )
    #create leaflet
    m <- basemap%>%
      #setView(144.964600, -37.020100, zoom = 6)%>%
      addMarkers(~lon, ~lat,
                 label = mylabels,
                 popup = mypopup,
                 layerId = ~Name,
                 labelOptions = labelOptions(
                   style = list("font-weight" = "normal", padding = "3px 8px"),
                   textsize = "13px",
                   direction = "auto")) %>%#, weight = 3, radius=40,
      #color="#0000FF", stroke = TRUE, fillOpacity = 0.8) %>%
      addControl(html=htmltitle, position = "topright")
    
    if(input$Uni!="None"){
      selection_uni<- Uni %>%
        filter(Name %in% input$Uni)
      selection_uni <- selection_uni %>%
        filter(Campus %in% input$campus)
      #popup for community legal centres
      mypopupuni <- paste(
        "University: ", selection_uni$Name, "<br/>",
        "Campus: ", selection_uni$Campus,"<br/>",
        "Emergency : 000", "<br/>",
        "On-Campus Security. : ", selection_uni$On_Campus_Security, "<br/>",
        "On-Campus Emergency: ", selection_uni$On_Campus_Emergency, "<br/>",
        "Campus Safety Escorts : ", selection_uni$Safety.Escorts,"<br/>",
        "Safer Communities: ", selection_uni$Safer.Community,"<br/>"
      ) %>%
        lapply(htmltools::HTML)
      m<- m %>% 
        clearMarkers()%>%
        addCircles(lng = as.numeric(selection_uni$lon), lat = as.numeric(selection_uni$lat), label = paste("Selected University: ", selection_uni$Name),popup=mypopupuni, weight = 4, radius=80,
                   color="#0000FF", stroke = TRUE, fillOpacity = 1)%>%
        addMarkers(~lon, ~lat, label = mylabels,
                   popup = mypopup,
                   labelOptions = labelOptions(
                     style = list("font-weight" = "normal", padding = "3px 8px"),
                     textsize = "13px",
                     direction = "auto"))
    }
    
    m
    
  })
  
  observeEvent(input$reset_button, {
    if(input$reset_button-values$flag==1){
      selectedData1 <- reactive({PS})
      selectedData2 <- reactive({PS})
      updateTextInput(session, "userlocation", value = "")
      updateRadioButtons(session, "service1", selected = "PS" )
      updateSliderInput(session, "radius1", value= "10")
      psnumber1(148)
      clcnumber1(46)
      values$flag=values$flag+1
    }})
  
  observeEvent(input$reset_button1, {
    if(input$reset_button1-values$flag1==1){
      selectedData2 <- reactive({PS})
      updateRadioButtons(session, "service2", selected = "PS" )
      updateSelectInput(session, "Uni", selected ="None")
      updateSelectInput(session, "campus", selected= "None")
      updateSliderInput(session, "radius2", value= "10")
      psnumber(148)
      clcnumber(46)
      values$flag1=values$flag1+1
    }})
  
  observeEvent(input$serviceplot2_marker_click, { # update the location selectInput on map clicks
    
    marker_lon = input$serviceplot2_marker_click$lng
    marker_lat = input$serviceplot2_marker_click$lat
    if(input$Uni!="None"){
      selection_uni<- Uni %>%
        filter(Name %in% input$Uni)
      selection_uni <- selection_uni %>%
        filter(Campus %in% input$campus)
      uni_lon = as.numeric(selection_uni$lon)
      uni_lat = as.numeric(selection_uni$lat)    
      route = osrmRoute(c(uni_lon,uni_lat), c(marker_lon, marker_lat), overview = 'full')
      # # route_simple = osrmRoute(c(115.6813467,-32.0397559), c(150.3715249,-33.8469759), overview = 'simplified')
      route_summary = osrmRoute(c(uni_lon,uni_lat), c(marker_lon, marker_lat), overview = FALSE)
      leafletProxy("serviceplot2")%>%
        clearGroup("polys") %>%
        addPolylines(route$lon,route$lat, group = "polys",
                     label = paste(round(route_summary[1]), 'mins - ', round(route_summary[2]), 'km'), 
                     labelOptions = labelOptions(noHide = TRUE))%>%
        addMiniMap(position = "bottomleft", width = 150, height = 150,
                   collapsedWidth = 19, collapsedHeight = 19, zoomLevelOffset = -5,
                   zoomLevelFixed = FALSE, centerFixed = FALSE, zoomAnimation = TRUE,
                   toggleDisplay = TRUE, autoToggleDisplay = TRUE, minimized = TRUE,
                   aimingRectOptions = list(color = "#ff7800", weight = 1, clickable = TRUE),
                   shadowRectOptions = list(color = "#000000", weight = 1, clickable = TRUE,
                                            opacity = 0, fillOpacity = 0), strings = list(hideText = "Hide MiniMap", showText = "Show MiniMap"),
                   mapOptions = list())
    }
  })
  
  
}

shinyApp(ui = ui, server = server)

