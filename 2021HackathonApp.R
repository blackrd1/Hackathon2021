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


#ADD WORKING DIRECTORY AND LOADING OF CSV DATA IN HERE!!!!!!!!!!!!
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
  
    titlePanel(h1("LIHTC Hackathon Map: By Riley Black and Chloe Hall")),
    
    mainPanel(
           h3("Nashville Map"),
           tabsetPanel(
           tabPanel("Cluster", leafletOutput("NashvilleClusterMap")),
           tabPanel("Marker", leafletOutput("NashvilleMarkerMap"))
  )
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
                               "<br/>Annual dollar amount of tax credits allocated: ",
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
                               "<br/>Annual dollar amount of tax credits allocated: ",
                               allocamt, 
                               "<br/>Total Number of Units: ",
                               n_units))})
  
}
# Run the app ----
shinyApp(ui = ui, server = server)
