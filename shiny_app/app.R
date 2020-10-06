## Not run: 
library(leaflet)
library(sf)
library(RColorBrewer)
library(tidyverse)
library(rsconnect)
library(leafgl)
library(rmapshaper)
library(shinythemes)
library(shinydashboard)

rsconnect::setAccountInfo(name='kmaclaine',
                          token='F6D4BBC8277EFEC0ECE4537718422A19',
                          secret='NgUHPWFZ7EcVxluQwjPz9+XKFmz/W08W+/JFGhBd')

covid_counties<- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/live/us-counties.csv")
activity_counties<-read_csv("https://www.gstatic.com/covid19/mobility/2020_US_Region_Mobility_Report.csv")

shape <- tigris::counties(state = "TX", class = "sf")

covid_counties_tx<-filter(covid_counties,state=="Texas")
covid_counties_tx$county<-paste(covid_counties_tx$county,"County")

shape$county<-shape$NAMELSAD
map_dataset<-merge(shape,covid_counties_tx,all=TRUE)

###Retail_recreation
activity_counties_tx_retail<-filter(activity_counties,country_region=="United States" & sub_region_1=="Texas" &!is.na( sub_region_2))[c(4,8,9)]%>%
    pivot_wider(names_from=date,values_from = retail_and_recreation_percent_change_from_baseline)
activity_counties_tx_retail$county<-activity_counties_tx_retail$sub_region_2
activity_counties_tx_retail<-merge(activity_counties_tx_retail,shape, by.x="sub_region_2", by.y="NAMELSAD",all=TRUE)
activity_counties_tx_retail$Type="Retail_Recreation"

####Grocery_Pharmacy

activity_counties_tx_grocery<-filter(activity_counties,country_region=="United States" & sub_region_1=="Texas" &!is.na( sub_region_2))[c(4,8,10)]%>%
    pivot_wider(names_from=date,values_from = grocery_and_pharmacy_percent_change_from_baseline)
activity_counties_tx_grocery$county<-activity_counties_tx_grocery$sub_region_2
activity_counties_tx_grocery<-merge(activity_counties_tx_grocery,shape, by.x="sub_region_2", by.y="NAMELSAD",all=TRUE)
activity_counties_tx_grocery$Type="Grocery_Pharmacy"

####Parks
activity_counties_tx_parks<-filter(activity_counties,country_region=="United States" & sub_region_1=="Texas" &!is.na( sub_region_2))[c(4,8,11)]%>%
    pivot_wider(names_from=date,values_from = parks_percent_change_from_baseline)
activity_counties_tx_parks$county<-activity_counties_tx_parks$sub_region_2
activity_counties_tx_parks<-merge(activity_counties_tx_parks,shape, by.x="sub_region_2", by.y="NAMELSAD",all=TRUE)
activity_counties_tx_parks$Type="Parks"

#####bind

activities_merged<-rbind(activity_counties_tx_retail,activity_counties_tx_grocery,activity_counties_tx_parks)
activities_merged$GEOID<-as.numeric(activities_merged$GEOID)

#####################################################

map_dataset$geometry<-st_cast(map_dataset$geometry,"POLYGON")
map_dataset$geometry<-st_cast(map_dataset$geometry)
map_dataset$geometry<-rmapshaper::ms_simplify(map_dataset$geometry,keep=0.02)

bins <- c(-75,-50,-25,0,25,50,75)
pal <- colorBin("RdYlBu", domain = -75:75, bins = bins,reverse =TRUE)

# Define UI 
ui <- fluidPage(theme = shinytheme("sandstone"),
    # Application title
    titlePanel("Activity in Texas Counties During COVID-19 Pandemic"),

    # Top panel with county name
    sidebarLayout(
        # the map itself
        mainPanel(leafletOutput("map"),width="100%",height="100%"
                  ),
        sidebarPanel(
            sliderInput(inputId = "slider", 
                        label = "Date",
                        min = as.Date(as.POSIXct("2020-02-16")),
                        max = as.Date(as.POSIXct("2020-08-15")),
                        value = as.Date(as.POSIXct("2020-02-16")),
                        step = 3,animate=animationOptions(interval=200)),
            selectInput(inputId = "dataset",label = "Activity Category",choices=c("Grocery_Pharmacy","Retail_Recreation","Parks"),selected = "Grocery_Pharmacy")
        )
    ),
    sidebarLayout(mainPanel('Google LLC "Google COVID-19 Community Mobility Reports".
https://www.google.com/covid19/mobility/ Accessed: 10/3/2020.'),sidebarPanel(""))
    
)

## End(Not run)
map_dataset$confirmed_cases
# Define server logic       
server <- function(input, output) {
    
    temp_data<-reactive({filter(activities_merged,Type==input$dataset)[,as.numeric(input$slider)-18306] })
    temp_data_2<-reactive({filter(activities_merged,Type==input$dataset)[,1] })
    
    output$map <- renderLeaflet({
        leaflet("map", data = map_dataset) %>%
            addProviderTiles("Stamen.TonerLite")%>%
            flyToBounds(-106,26,-93,36)%>%
            addLegend(pal = pal, values = "Relative Activity Score",title= "Relative Activity Score",opacity = 1)

    })
  
    observe({

        labels2 <- sprintf(
            "<strong>%s</strong><br/>%g activity</>",
            temp_data_2(), temp_data()
        ) %>% lapply(htmltools::HTML)
        
        leafletProxy("map", data = map_dataset)%>%
            addPolygons(data = map_dataset, 
                        fillColor = ~pal(temp_data()),
                        weight = 2,
                        opacity = 1,
                        color = "white",
                        dashArray = "3",
                        fillOpacity = 0.7,
                        highlight = highlightOptions(
                            weight = 5,
                            color = "#666",
                            dashArray = "",
                            fillOpacity = 0.7,
                            bringToFront = TRUE),
                        layerId = ~COUNTYNS,
                        label = labels2)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)






