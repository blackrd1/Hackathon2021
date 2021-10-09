library(shiny)

# Define UI ----
ui <- fluidPage(
    titlePanel(h1("2021 Hackathon App: By Riley Black and Chloe Hall")),
    
    sidebarLayout(
      mainPanel(h3("Nashville Map")),
      sidebarPanel(h3("Housing Info")))
      
)

# Define server logic ----
server <- function(input, output) {
    
}

# Run the app ----
shinyApp(ui = ui, server = server)
