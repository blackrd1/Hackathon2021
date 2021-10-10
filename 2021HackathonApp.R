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

#ADD IN WORKING DIRECTORY
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
    
    titlePanel(h3("Riley Black and Chloe Hall's Vandy Hackathon Project")),
    
    mainPanel(
        h1("Low-Income Housing Tax Credit Properties Map"),
        fluidRow("This interactive map shows the distribution of LIHTC properties built in Nashville from 1987-2019."),
        br(),
        tabsetPanel(
            tabPanel("Nashville Cluster", leafletOutput("NashvilleClusterMap"),
                     h4(
                         "These clusters display areas of higher concentration of LIHTC developments. Click on a cluster or zoom in to see how developments are distributed!")),
            
            tabPanel("Nashville Marker", leafletOutput("NashvilleMarkerMap"),
                     h4(
                         "These markers display each LIHTC property.Click on a marker to see the number of units and the tax dollars allocated!")),
            tabPanel("Moon", leafletOutput("MoonMap"),
                     h3(
                         "There are no LIHTC properties on the moon."),
                     h6("(yet)"))
        ),
        fluidRow("This data comes from the HUD National Low Income Housing Tax Credit (LIHTC) Database 1987-2019 Data Dictionary."),
        fluidRow("For more information, visit https://www.huduser.gov/portal/datasets/lihtc.html")
    ),
        sidebarPanel(
            h4("What is a LIHTC?"),
            fluidRow("Low-Income Housing Tax credits are funds given to private developers by the state or local government to finance qualifying housing developments that fit guidelines of affordability and create new rental units for low income tenants."), 
                     br(),
                     fluidRow("This program helps fill the market need for low income housing that is often unadressed by the private development market.")
            ),
    
    )

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
                                     "<br/>Annual dollar amount of tax credits allocated: $",
                                     allocamt, 
                                     "<br/>Total Number of Affordable Units: ",
                                     li_units),
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
                                     "<br/>Annual dollar amount of tax credits allocated: $",
                                     allocamt, 
                                     "<br/>Total Number of Affordable Units: ",
                                     li_units)) %>%
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
