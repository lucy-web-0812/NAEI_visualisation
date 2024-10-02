library(readxl)
library(dplyr)
library(tidyverse)
library(shiny)
library(plotly)
library(bslib)
library(scales)
library(RColorBrewer)

# ---------- Preprocessing required is in the script data_processing ------------
# The output of this script is the combined_historic_and_projected csv dataset


combined_historic_and_projected <- read.csv("data/combined_historic_and_projected.csv") |> 
  mutate(year = as.Date(year, format = "%Y")) |> 
  select(-"X") |> 
  filter(is.na(NFR_wide) == FALSE)



totals <- read.csv("data/totals.csv") |> 
  mutate(year = as.Date(year, format = "%Y")) |> 
  select(-"X") |> 
  pivot_longer(cols = c(emission, emissions_relative_to_baseline), names_to = "emission_type", values_to = "emission_long")



n_colours <- length(unique(combined_historic_and_projected$source_description)) 


source_colour_mappings <- data.frame( 
  source_description = unique(combined_historic_and_projected$source_description), 
  colour = sample(colorRampPalette(brewer.pal(9, "Set1"))(n_colours), n_colours) # Sample makes the colours random rather than in order
  )

colour_mappings <- setNames(source_colour_mappings$colour, source_colour_mappings$source_description)

# ----- The user interface ------------   
  
  ui <- fluidPage(
    
    titlePanel("National Atmospheric Emissions Inventory Visualiser", windowTitle = "NAEI Projections"),
    
    tags$head(
      tags$style(HTML("
      .navbar.navbar-inverse {
        background-color: #4F1271;
        border-color: #4F1271;
        border-radius: 10px;
      }
      .navbar.navbar-inverse .navbar-brand {
        color: #FFFFFF;
      }
      .navbar.navbar-inverse .navbar-nav > li > a {
        color: #FFFFFF;
      }
     .navbar.navbar-inverse .navbar-nav > li > a:hover,
      .navbar.navbar-inverse .navbar-nav > li > a:focus {
        background-color: #FFFFFF !important;
        color: #783F8E !important;
      }
      .navbar.navbar-inverse .navbar-nav > .active > a,
      .navbar.navbar-inverse .navbar-nav > .active > a:hover,
      .navbar.navbar-inverse .navbar-nav > .active > a:focus {
        background-color: #FFFFFF !important;
        color: #783F8E !important;   
      }
      
    "))
    ),
    
    
    tags$head(
      tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css?family=Roboto:300,400,500,700&display=swap"),
      tags$style(HTML("
    body {
      font-family: 'Roboto', sans-serif;
    }
    .navbar.navbar-inverse .navbar-brand {
      font-weight: 500;
    }
  "))
    ),
    
    navbarPage(title = tags$a(href = "https://naei.beis.gov.uk/data/", "All data is taken from the UK NAEI", style = "color: #FFFFFF; text-decoration: none;"), inverse = "TRUE",
               
    # First page focusing on the the totals         
               
      tabPanel("Totals", 
                  fluidRow(
                          column(4, selectInput("relative_or_absolute", "Change the y axis scale:", choices = c("Relative to baseline" = "emissions_relative_to_baseline", "Absolute" = "emission")))), 
                     fluidRow(column(8, plotlyOutput("totals_plot")))),        
   # The second page            
               
      tabPanel("By Source",
               
      sidebarLayout(
      
      sidebarPanel(
        
        width = 4,
        
      p("Use the drop down boxes below to select a pollutant, a NFR category and the subcatergories of interest. A graph will appear below which is interactive; drag the x and y scales to change them, double click to reset the axes."),
      
             selectInput("selected_pollutant", "Select Pollutants", choices = unique(combined_historic_and_projected$pollutant), multiple = TRUE),
      
             selectInput("selected_source", "Select Source", choices = unique(combined_historic_and_projected$source_description), multiple = TRUE),
      
      h3("2021 top pollution sources:"), tableOutput("top_sources")),  
    
      
    mainPanel(
    # fluidRow(
    #   column(10, h3("2021 top pollution sources:"), tableOutput("top_sources"))) 
    # ,
    fluidRow(
      column(8, h3("Trends over time:"), plotlyOutput("line_graphs_one_category", height = "90vh"))
     )))
    
   )))
      
     
  
  
# ---- And the server function ------

server <- function(input, output, session){
  
  
  selected_data <- reactive(combined_historic_and_projected |> # Test to see what one of these looks like
                              filter(pollutant %in% input$selected_pollutant) |> 
                              filter(NFR_mid != "NA") 
    
  )
  
  totals_data <- reactive({
    totals %>%
      filter(emission_type == input$relative_or_absolute)
  })
  
  
  selected_source <- reactive(combined_historic_and_projected |> # Test to see what one of these looks like
                              filter(pollutant %in% input$selected_pollutant) |> 
                              filter(source_description %in% input$selected_source) 
  )
  
  
  
  top_sources_table <- reactive({
    req(input$selected_pollutant) 
    filter(combined_historic_and_projected, pollutant %in% input$selected_pollutant) |> 
      mutate(year = substring(as.character(year), 1,4) ) |> 
      filter(year == "2021") |> 
      group_by(pollutant) |> 
      arrange(desc(emission)) |> 
      do(head(., n=5)) |> 
      arrange(desc(emission)) 
  })
  
  
  output$top_sources <- renderTable(top_sources_table() |> 
                                      select("NFR_code", "pollutant", "emission", "source_description") |> 
                                      rename("NFR Code" = NFR_code, "Pollutant" = "pollutant", "Emission (kilotonnes)" = "emission", "Source" = "source_description"))
  
  y_axis_label <- reactive({
    if (input$relative_or_absolute == "emission") {
      return("Emissions (kilotonnes)")
    } else {
     return("Emissions (%, relative to baseline)")
    }
  })
  
  
  
  output$totals_plot <- renderPlotly({
    ggplotly(
      ggplot(totals_data()) +
        geom_point(aes(x = year, y = emission_long, colour = pollutant, shape = data_source, 
                       text = paste(
          "Year:", format(year, "%Y"), "<br>",
          "Emission:", round(emission_long, 2), "<br>",
          "Pollutant:", pollutant, "<br>",
          "Data Source:", data_source
        ))) +
        geom_line(aes(x = year, y = emission_long, colour = pollutant, linetype = data_source)) +
        scale_color_brewer(palette = "Set1") +
        scale_x_date(name = "Year", limits = as.Date(c("1970-01-01", "2050-01-01")), breaks = seq(as.Date("1970-01-01"), as.Date("2050-01-01"), by = "10 years"), labels = date_format("%Y")) +
        scale_y_continuous(name =   
                             y_axis_label(), limits = c(0, NA), expand = c(0,0)) +
        theme_classic() +
        theme(panel.grid.major.y = element_line(colour = "lightgrey"),
              strip.background = element_rect(colour = "white"),
              strip.placement = "bottom") +
        theme(legend.position = "none") +
        theme(axis.title = element_text(size = 20)),
      height = 600,
      width = 1000, 
      tooltip = "text"
    )
  })

    
  
  
  output$line_graphs_one_category <- renderPlotly({
    
    req(input$selected_source)
    req(input$selected_pollutant)
    
    ggplotly(
    ggplot(selected_source()) +
      geom_line(aes(x=year, y = emission, colour = source_description, linetype = status)) +
      geom_point(aes(x=year, y = emission, colour = source_description, shape = status, 
                     text = paste(
        "Year:", format(year, "%Y"), "<br>",
        "Emissions:", round(emission, 2), "kilotonnes <br>",
        "Pollutant:", source_description, "<br>",
        "Data Source:", status, "<br>",
        "NFR Code:", NFR_code
      ))) +
      scale_y_continuous(name = "Emissions (kilotonnes)", limits = c(0,NA)) +
      facet_wrap(~pollutant , scales = "free_y", ncol = 2) +
      scale_x_date(name = "Year", limits = as.Date(c("1970-01-01", "2050-01-01"))) +
      scale_colour_manual(values = colour_mappings) +
      theme_classic() +
      theme(panel.grid.major.y = element_line(colour = "lightgrey"), 
            strip.background = element_rect(colour = "white"), 
            strip.placement = "bottom") +
      theme(legend.position = "none") +
      theme(axis.title = element_text(size = 20), 
            strip.text = element_text(size = 10)), height = 600, width = 1000, tooltip = "text")
    })
  
  
  
  session$onSessionEnded(function() {
    stopApp()
  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)
