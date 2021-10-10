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
  
    titlePanel(h1("Riley Black and Chloe Hall's Hackathon Project")),
    
    mainPanel(
           h3("Low-Income Housing Tax Credit Properties Map"),
           tabsetPanel(
           tabPanel("Nashville Cluster", leafletOutput("NashvilleClusterMap")),
           tabPanel("Nashville Marker", leafletOutput("NashvilleMarkerMap")),
           tabPanel("Moon", leafletOutput("MoonMap"))
  )
))

# Define server logic ----
server <- function(input, output, session) {
  AnchorDown <- makeIcon(
    iconUrl = "https://www.logolynx.com/images/logolynx/f7/f7d08cb2999bf2d07c1c016555a64f16.png",
    iconWidth = 70, iconHeight = 35)

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
                 clusterOptions = markerClusterOptions()) %>%
    addMarkers(lat=36.14312813278591,
               lng=-86.80566855798781,
               icon=AnchorDown,
               popup='Anchor Down!')
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
                               n_units)) %>%
    addMarkers(lat=36.14312813278591,
               lng=-86.80566855798781,
               icon=AnchorDown,
               popup='Anchor Down!')
    })
  
  output$MoonMap <- renderLeaflet({
    leaflet() %>%
      addTiles('https://s3.amazonaws.com/opmbuilder/301_moon/tiles/w/hillshaded-albedo/{z}/{x}/{y}.png')
  })
  
}
# Run the app ----
shinyApp(ui = ui, server = server)
