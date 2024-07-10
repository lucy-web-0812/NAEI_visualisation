library(readxl)
library(dplyr)
library(tidyverse)
library(shiny)
library(bslib)
library(plotly)

# ---------- Preprocessing required is in the script data_processing ------------
# The output of this script is the combined_historic_and_projected csv dataset


combined_historic_and_projected <- read.csv("data/combined_historic_and_projected.csv") |> 
  mutate(year = as.Date(year, format = "%Y")) |> 
  select(-"X")



# ----- The user interface ------------   
  
  ui <- fluidPage( 
    theme = bs_theme(version = 5, bootswatch = "minty"),
    
    titlePanel("National Atmospheric Emissions Inventory Visualiser", windowTitle = "NAEI Projections"), 
    
    navbarPage(title = "All data is taken from the UK NAEI", inverse = "TRUE",
      tabPanel("By Source",
               
               
  
    
    sidebarLayout(
      
      sidebarPanel(
      p("Use the drop down boxes below to select a pollutant, a NFR category and the subcatergories of interest. The graph below is interactive; drag the x and y scales to change them, double click on a dataset to isolate."),
             selectInput("selected_pollutant", "Select Pollutants", choices = unique(combined_historic_and_projected$pollutant), multiple = TRUE),
      
             selectInput("selected_NFR", "Select Category", choices = unique(combined_historic_and_projected$NFR_wide), multiple = TRUE),
      
             selectInput("selected_source", "Select source", choices = NULL, multiple = TRUE)),
    
      
    mainPanel(
    fluidRow(
      
      
      column(10, plotlyOutput("line_graphs_one_category"))
      
     )) 
      )),
    
    
    tabPanel("Totals", 
             fluidRow(
               p("If you are reading this then well done"), 
               column(4, selectInput("selected_pollutant", "Pollutant", choices = unique(combined_historic_and_projected$pollutant))), 
               column(8, img(src='millie.png', height = "80%", width = "60%")
               
             )       
             
    ) 
    
    )))
      
    
  
  
# ---- And the server function ------

server <- function(input, output, session) {
  
  
  selected_data <- reactive(combined_historic_and_projected |> # Test to see what one of these looks like
                              filter(pollutant %in% input$selected_pollutant) |> 
                              filter(NFR_mid != "NA")
    
  )
  
 
  
  
  selected_data_just_one_category <- reactive(combined_historic_and_projected |> # Test to see what one of these looks like
                              filter(pollutant %in% input$selected_pollutant) |> 
                              filter(NFR_wide %in% input$selected_NFR) 
  )
  
  
  observeEvent(selected_data_just_one_category(), {
    choices <- unique(selected_data_just_one_category()$NFR_mid)
    updateSelectInput(inputId = "selected_source", choices = choices)
  })

  
  selected_source <- reactive({
    #req(input$selected_source)
    filter(selected_data_just_one_category(), NFR_mid %in% input$selected_source)
  })
  

  
  output$line_graphs_one_category <- renderPlotly({
    
    req(input$selected_source)
    
    ggplotly(
    ggplot(selected_source()) +
      geom_line(aes(x=year, y = emission, colour = source_description, linetype = status)) +
      geom_point(aes(x=year, y = emission, colour = source_description, shape = status)) +
      ggtitle(label = "Emissions from selected source:") +
      scale_y_continuous(name = "Emissions (kilotonnes)", limits = c(0,NA)) +
      facet_wrap(~pollutant + NFR_wide, scales = "free", ncol = 2) +
      scale_x_date(name = "Year", limits = as.Date(c("1970-01-01", "2050-01-01"))) +
      theme_classic() +
      theme(panel.grid.major.y = element_line(colour = "lightgrey"), 
            strip.background = element_rect(colour = "white"), 
            strip.placement = "bottom") +
      theme(legend.position = "none") +
      theme(axis.title = element_text(size = 20, face = "bold"), 
            strip.text = element_text(size = 10, face = "bold")), height = 600, width = 1000)
    })
  
  
  
  session$onSessionEnded(function() {
    stopApp()
  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)
