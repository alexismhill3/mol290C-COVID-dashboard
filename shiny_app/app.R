#Install Packages

library(leaflet)
library(sf)
library(RColorBrewer)
library(tidyverse)
library(rsconnect)
library(leafgl)
library(rmapshaper)
library(shinythemes)
library(shinydashboard)


#rsconnect info
rsconnect::setAccountInfo(name='kmaclaine',
                          token='F6D4BBC8277EFEC0ECE4537718422A19',
                          secret='NgUHPWFZ7EcVxluQwjPz9+XKFmz/W08W+/JFGhBd')


####Data import from googlr and nytimes#####
covid_counties_master <- read_csv("us-counties_master.csv")
covid_counties <- read_csv("us-counties.csv")
#google_global_activity_previously saved
global_activity_countries <- readRDS("global_activity.rds")
#world_simple_previously_saved
wrld_simpl <- readRDS("wrld_simpl.rds")
#tigris data for texas previously saved
shape <-readRDS("tigris.rds")

#Names from wrld_simpl
wrld_name<-wrld_simpl$ISO2
#joins the google and wrld datasets
world_and_google<-left_join(as.data.frame(wrld_name),global_activity_countries,by = c("wrld_name"="country_region_code"))

#Filters county data for just Texas and adds " County" to all of the names
covid_counties_tx<-filter(covid_counties,state=="Texas")
covid_counties_tx$county<-paste(covid_counties_tx$county,"County")

#Merges shape data and covid counties so that they will line up
shape$county<-shape$NAMELSAD

covid_counties_tx_master<-filter(covid_counties_master,state=="Texas")
covid_counties_tx_master<-covid_counties_tx_master%>%select(-c(3,4,6))
covid_counties_tx_master$county<-paste(covid_counties_tx_master$county,"County")
covid_counties_tx_master<-covid_counties_tx_master%>%pivot_wider(names_from=date,values_from = cases)
covid_counties_tx_master<-covid_counties_tx_master%>%filter(county!="Unknown County")
covid_counties_tx_master_2<-merge(covid_counties_tx_master,shape,all=TRUE)

#####Data prep for activity data#######
activities_merged <- readRDS("activities_merged.rds")

#####################################################
####Simplifies polygons
map_dataset <- readRDS("map_dataset.rds")
map_dataset$geometry<-rmapshaper::ms_simplify(map_dataset$geometry,keep=0.001)

#####Creates color palattes
#Color pallette for activity
bins <- c(-75,-50,-25,0,25,50,75)
pal <- colorBin("RdYlBu", domain = -75:75, bins = bins,reverse =TRUE)

#Color pallette for cases
bins2 <- c(0,500,2000,8000,32000,124000)
pal2 <- colorBin("YlOrRd", domain = 0:10000, bins = bins2,reverse =FALSE)

#########Define UI #########
ui <- fluidPage(theme = shinytheme("flatly"),
                # Application title
                titlePanel("COVID-19 Dashboard: Community COVID data and disease outcomes"),
                
                #Adds the tabs
                tabsetPanel(
                  id = "navbar",
                  tabPanel(title = "Introduction",
                           titlePanel("COVID-19: When the world stopped moving"),
                           fluidRow(
                             column(12,img(src="world.gif",height = 600)
                             )
                           ),
                           fluidRow(
                             column(12, "The above gif maps the community activity scores in 135 countries around the world at the onset of the COVID-19 pandemic in the category 'retail and recreation'. Darker Blue means less activity.")
                           ),
                           fluidRow(
                             column(12, h2(" "))
                           ),
                           
                           fluidRow(
                             
                             column(12, h4("      The novel coronavirus, designated as the severe acute respiratory syndrome coronavirus-2 (SARS-CoV-2), has been causing an ongoing pandemic worldwide. As of December 2, 2020, the virus has caused 64,844,711 global cases, and 1.5 million deaths have been reported. The common routes of transmission are contact and droplet transmission as well as airborne transmission. Even though the virus is causing deadly symptoms and is highly transmissible, no vaccine is currently available to protect against it. Therefore, we developed a coronavirus disease (COVID-19) information application to help educate the public about the virus and to provide analysis to help people make safer decisions during the pandemic. We created a Shiny app that has two sections: an interactive, local COVID-19 dashboard, and a COVID-19 statistical analysis section. The COVID-19 dashboard contains interactive maps for the state of Texas with both county activity data from a google dataset and COVID-19 cases. There will also be a section that provides local behavior recommendations for individual county, such as the best day to go shopping or to the park during the week. In the statistical analysis section, we explored how COVID-19 outcomes vary by demographic (age, race, sex, etc.), and investigate the role of pre-existing conditions in COVID-19 morbidity."))
                           )
                ),
                  
                  tabPanel(title = "COVID-19 Dashboard:State and County",
                           fluidRow(
                             column(
                               12, h3("County Level COVID-19 Dashboard")
                             )
                           ),
                           
                           fluidRow(column(12,selectInput(inputId="county_choice",label = "Please Select a County To Generate Your Dashboard",choices=filter(activities_merged,Type=="Retail_Recreation")$sub_region_2)
                                           )
                           ),
                           fluidRow(
                                    column(12,selectInput(inputId="type", label= "Activity Type", choices=c("Shopping/Restaurants"="Retail_Recreation", "Grocery"="Grocery_Pharmacy", "Go to the Park"="Parks"))
                                           )
                           ),
                           
                           fluidRow(
                             column(6, tableOutput("best_and_worst")
                                    )
                           ),
                           fluidRow(
                             column(6, plotOutput("county_activity_graph")
                                    ),
                             column(6,plotOutput("county_cumulative_case_data_graph")
                                    )
                             ),
                           fluidRow(
                             column(
                               12, h3("Texas Activity and Case Maps")
                             )
                           ),
                           
                           # Kendra's tab panel
                           fluidRow(
                             # the map itself
                             column(
                               6, leafletOutput("map")
                               ),
                             column(
                               6,leafletOutput("map2")
                               )
                             ),
                           
                           
                           fluidRow(
                             column(
                               6,
                                    sliderInput(inputId = "slider", 
                                                label = "Date",
                                                min = as.Date(as.POSIXct("2020-02-16")),
                                                max = as.Date(as.POSIXct("2020-08-15")),
                                                value = as.Date(as.POSIXct("2020-02-16")),
                                                step = 3,animate=animationOptions(interval=1500)),
                                    selectInput(inputId = "dataset",label = "Activity Category",choices=c("Grocery"="Grocery_Pharmacy","Shopping/Restaurants"="Retail_Recreation","Parks"),selected = "Grocery_Pharmacy")
                             ),
                             column(
                               6,
                                    sliderInput(inputId = "slidey", 
                                                label = "Date",
                                                min = as.Date(as.POSIXct("2020-02-16")),
                                                max = as.Date(as.POSIXct("2020-08-15")),
                                                value = as.Date(as.POSIXct("2020-02-16")),
                                                step = 3,animate=animationOptions(interval=1500))
                               )
                             ),
                             

                           #adds link to google data as source
                           sidebarLayout(
                           mainPanel('Google LLC "Google COVID-19 Community Mobility Reports".
https://www.google.com/covid19/mobility/ Accessed: 10/3/2020.'),sidebarPanel("")
                                         )
                           ),
                  #####you guys should only have to add things between here and 
                  
                tabPanel(title = "Local COVID-19 Plots",
                         titlePanel("Local Covid Demographic Data"),
                         #example of adding an image
                         fluidRow(
                           column(6,(img(src="Travis Confirmed by Race.png",height = 380)
                           ), 
                           column(6, "Figure 1. Travis County Confirmed Cases by Race")
                           ),
                           column(6,(img(src="Travis Confirmed by Ethnicity.png",height = 380)
                           ), 
                           column(6, "Figure 2. Travis County Confirmed Cases by Ethnicity")
                           )),
                         
                         fluidRow(
                           column(6,(img(src="Travis Death by Race.png",height = 380)
                           ), 
                           column(6, "Figure 3. Travis County Death Cases by Race")
                           ),
                           column(6,(img(src="Travis Death by Ethnicity.png",height = 380)
                           ), 
                           column(6, "Figure 4. Travis County Death Cases by Ethnicity")
                           )),
                         
                         fluidRow(
                           column(6,(img(src="Texas Covid Cases by County.png",height = 380)
                           ), 
                           column(6, "Figure 5. Texas Total Cases by County")
                           ),
                           column(6,(img(src="Texas College Total Cases by County.png",height = 380)
                           ), 
                           column(6, "Figure 6. Texas College Total Cases by County")
                           )),
                         
                         fluidRow(
                           column(6,(img(src="Confirmed and Death by Age Group.png",height = 380)
                           ), 
                           column(6, "Figure 7. Travis County Confirmed and Death Cases by Age Group")
                           ),
                           column(6,(img(src="US Cases Confirmed vs Death by State.png",height = 380)
                           ), 
                           column(6, "Figure 8. U.S. Cases Confirmed vs Death by State")
                           ))
                         #####here
                         #####
                )
                )
)


# Define server logic       
server <- function(input, output) {
  #Creates reactive datasets for counties and activity data
  temp_data<-reactive({filter(activities_merged,Type==input$dataset)[,as.numeric(input$slider)-18306] })
  temp_data_2<-reactive({filter(activities_merged,Type==input$dataset)[,1] })
  
  temp_data_3<-reactive({filter(covid_counties_tx_master_2)[,as.numeric(input$slidey)-18302] })
  temp_data_4<-reactive({filter(covid_counties_tx_master_2)[,1] })
  
  
  county_activity_graph_data <- reactive({pivot_longer(subset(filter(activities_merged, sub_region_2==input$county_choice, Type==input$type),select = -c(185:301)),
                                                       cols = starts_with("2020"),
                                                       names_to="Day",
                                                       values_to="Activity")})
  county_cumulative_case_data <- reactive({pivot_longer(subset(filter(covid_counties_tx_master_2, county==input$county_choice),select = -c(185:305)),
                         cols = starts_with("2020"),
                         names_to="Day",
                         values_to="Cases")})
    
  
  

  
  output$best_and_worst<- renderTable({filter(activities_merged,sub_region_2==input$county_choice & Type == input$type) %>%
      subset(select = c(1:(ncol(activities_merged)-26),
                        (ncol(activities_merged)))) %>%
      pivot_longer(cols = starts_with("2020"),names_to = c("Date"),values_to="activity")%>%
      slice(tail(row_number(), 28))%>%
      add_column(Day = weekdays(as.Date(.$Date))) %>% 
      drop_na(.) %>%
      group_by(Day,Type) %>%
      summarize(mean(activity))%>%
      ungroup%>%
      filter(Day!="Saturday"&Day!="Sunday") %>%
      slice(which.max(`mean(activity)`),which.min(`mean(activity)`)) %>%
      add_column(" "=c("Worst Day:","Best Day:"),.before = "Day")
    
  })
  
  
  
  #Adds basics to the map, stuff that is not interactive
  output$map <- renderLeaflet({
    leaflet("map", data = map_dataset) %>%
      addProviderTiles("Stamen.TonerLite")%>%
      flyToBounds(-106,26,-93,36)%>%
      addLegend(pal = pal, values = "Relative Activity Score",title= "Relative Activity Score",opacity = 1)
  })
  
  output$map2<- renderLeaflet({
    leaflet("map2", data = map_dataset) %>%
      addProviderTiles("Stamen.TonerLite")%>%
      flyToBounds(-106,26,-93,36)%>%
      addLegend(pal = pal2, values = "COVID Cases",title= "COIVD Cases",opacity = 1)
    
  })
  

  
  
  observe({
    
    labels3 <- sprintf(
      "<strong>%s</strong><br/>%g cases</>",
      temp_data_4(), temp_data_3()
    ) %>% lapply(htmltools::HTML)
    
    labels2 <- sprintf(
      "<strong>%s</strong><br/>%g Activity</>",
      temp_data_2(), temp_data()
    ) %>% lapply(htmltools::HTML)
    
    
    #This is the interactive part of the map, polygons are remade every time the options are changed
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
                  label=labels2)
    
    leafletProxy("map2", data = map_dataset)%>%
      addPolygons(data = map_dataset, 
                  fillColor = ~pal2(temp_data_3()),
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
                  layerId = ~county,
                  label = labels3)
    
    output$county_activity_graph <- renderPlot({ggplot(county_activity_graph_data(), aes(as.Date(Day), Activity)) +
        geom_point()+
        theme_classic()+
        xlab(" ")+
        ggtitle("Activity Data Each Day")
        
        })
    
    output$county_cumulative_case_data_graph <- renderPlot({ggplot(county_cumulative_case_data(), aes(as.Date(Day), Cases)) +
        geom_point()+
        theme_classic()+
        xlab(" ")+
        ggtitle("Cumulative Case Data Each Day")
      })
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)


