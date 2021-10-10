#Install Libraries
library(tidyverse)
library(dplyr)
library(readr)
library(sf)
library(raster)
library(spData)
library(spDataLarge)
library(tmap) 
library(leaflet) 
library(ggplot2) 
library(shiny)
library(spDataLarge)
library(RColorBrewer)


setwd("~/Downloads/LIHTC")

LIHTCPUB <- read_csv("LIHTCPUB.CSV")
dat <- LIHTCPUB
remove(LIHTCPUB)

#Filters to only Tennessee
nashdat <- dat %>%
    filter(proj_st=="TN")

#Filters to only Nashville data
nashdat <- nashdat %>%
    filter(proj_cty=="NASHVILLE")

#Dropped NA for Project Addresses
nashdat <- nashdat%>%
    drop_na(proj_add)

#Dropped NA for Project Zipcodes
nashdat <- nashdat%>%
    drop_na(proj_zip)

#Filtered out the data points without years
nashdat <- nashdat%>%
    filter(yr_pis!= 9999 & yr_pis!= 8888)


#Copied from rstudio page on leaflet
r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

#Change project name
names(nashdat)[names(nashdat)=='project']<-"projectname"

# Define UI ----
ui <- fluidPage(
    
    titlePanel(h1("Riley Black and Chloe Hall's Hackathon Map")),
    
    mainPanel(
        h3("Low-Income Housing Tax Credit Properties Map"),
        fluidRow("This map shows the distribution of low-income properties built in Nashville from 1987-2019."),
        tabsetPanel(
            tabPanel("Nashville Cluster", leafletOutput("NashvilleClusterMap"),
                     fluidRow(
                         "These clusters display areas of higher concentration of LIHTC units. Click on a cluster or zoom in to see how units are distributed!")),
    
            tabPanel("Nashville Marker", leafletOutput("NashvilleMarkerMap"),
                     fluidRow(
                         "These markers display each LIHTC property.Click on a marker to see the number of units and the tax dollars allocated!")),
            tabPanel("Moon", leafletOutput("MoonMap"),
                     fluidRow(
                         "There are no LIHTC properties on the moon."))
        ),
        h6("This data comes from the HUD National Low Income Housing Tax Credit (LIHTC) Database 1987-2019 Data Dictionary")
    ))

# Define server logic ----
server <- function(input, output, session) {
    output$NashvilleClusterMap <- renderLeaflet({
        leaflet(nashdat) %>%
            addTiles() %>%
            addMarkers(lng=~longitude, 
                       lat=~latitude,
                       popup=~paste0("Project: ",
                                     projectname,
                                     "<br/>Project Address: ",
                                     proj_add, 
                                     "<br/>ZIP: ",
                                     proj_zip, 
                                     "<br/>Annual dollar amount of tax credits allocated: $",
                                     allocamt, 
                                     "<br/>Total Number of Units: ",
                                     n_units),
                       clusterOptions = markerClusterOptions())
    })
    
    output$NashvilleMarkerMap <- renderLeaflet({
        leaflet(nashdat) %>%
            addTiles() %>%
            addMarkers(lng=~longitude, 
                       lat=~latitude,
                       popup=~paste0("Project: ",
                                     projectname,
                                     "<br/>Project Address: ",
                                     proj_add, 
                                     "<br/>ZIP: ",
                                     proj_zip, 
                                     "<br/>Annual dollar amount of tax credits allocated: $",
                                     allocamt, 
                                     "<br/>Total Number of Units: ",
                                     n_units))})
    
    output$MoonMap <- renderLeaflet({
        leaflet() %>%
            addTiles('https://s3.amazonaws.com/opmbuilder/301_moon/tiles/w/hillshaded-albedo/{z}/{x}/{y}.png')
    })
    
}
# Run the app ----
shinyApp(ui = ui, server = server)
