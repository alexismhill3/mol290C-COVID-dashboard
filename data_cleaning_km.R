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


####Data import from googlr and nytimes#####
covid_counties<- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/live/us-counties.csv")
activity_counties<-read_csv("https://www.gstatic.com/covid19/mobility/2020_US_Region_Mobility_Report.csv")

#Gets the polygons data for the counties in texas
shape <- tigris::counties(state = "TX", class = "sf")

#Filters county data for just Texad and adds " County" to all of the names
covid_counties_tx<-filter(covid_counties,state=="Texas")
covid_counties_tx$county<-paste(covid_counties_tx$county,"County")
#Merges shape data and covid counties so that they will line up
shape$county<-shape$NAMELSAD
map_dataset<-merge(shape,covid_counties_tx,all=TRUE)

#####Data prep for activity data#######
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
####Simplifies polygons
map_dataset$geometry<-st_cast(map_dataset$geometry,"POLYGON")
map_dataset$geometry<-st_cast(map_dataset$geometry)



