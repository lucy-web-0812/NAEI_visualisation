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
  select(-"X") 

list_of_pollutants_and_units <- read.csv("data/list_of_pollutants.csv") |> 
  select(-"X") |> 
  mutate(Pollutant = ifelse(Pollutant == "Total 1-4", "Total PAHs", Pollutant))

totals <- read.csv("data/totals.csv") |> 
  mutate(year = as.Date(year, format = "%Y")) |> 
  select(-"X") |> 
  pivot_longer(cols = c(emission, emissions_relative_to_baseline), names_to = "emission_type", values_to = "emission_long")


ghg_data <- read.csv("data/ghg_data.csv") |> 
  mutate(year = as.Date(paste0(year, "-01-01"), format = "%Y-%m-%d"))



n_colours <- length(unique(combined_historic_and_projected$source_description)) 

set.seed(123)


source_colour_mappings <- data.frame( 
  source_description = unique(combined_historic_and_projected$source_description), 
  colour = sample(colorRampPalette(brewer.pal(9, "Paired"))(n_colours), n_colours) # Sample makes the colours random rather than in order
)

colour_mappings <- setNames(source_colour_mappings$colour, source_colour_mappings$source_description)

# ----- The user interface ------------   

ui <- fluidPage(
  
 
  
  tags$head(
    tags$style(HTML("
    .navbar.navbar-inverse {
      background-color: #8CC084;
      border-color: #8CC084;
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
    .title-banner {
      background-color: #004643; 
      color: white; 
      padding: 15px;
      text-align: center; 
      font-size: 24px;
      font-weight: bold;
      border-radius: 5px;
      margin-bottom: 0px;
      box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2);
    }
    .full-width-tabs .nav-tabs {
      width: 100%;
      display: flex;
      font-weight: bold;
      justify-content: space-around;
      outline-color: darkgreen;
    }
  ")),
    
    
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
  
  
  titlePanel(div("National Atmospheric Emissions Inventory Visualiser", class = "title-banner"), windowTitle = "NAEI Projections"),
  
  
  navbarPage(title = tags$a(href = "https://naei.energysecurity.gov.uk/air-pollutants/air-pollutant-emissions-data", "All data is taken from the UK NAEI", style = "color: #FFFFFF; text-decoration: none;"), inverse = TRUE,
             
             # First page focusing on the the totals         
             
             tabPanel("Air Pollutant Totals", 
                      fluidRow(
                        column(4, selectInput("relative_or_absolute", "Change the y axis scale:", choices = c("Relative to baseline" = "emissions_relative_to_baseline", "Absolute" = "emission")))), 
                      fluidRow(column(8, plotlyOutput("totals_plot")))),        
             # The second page            
             
             tabPanel("Air Pollutants By Source",
                      
                      sidebarLayout(
                        
                        sidebarPanel(
                          
                          width = 4,
                          
                          p("Use the drop down boxes below to select a pollutant, a NFR category and the subcatergories of interest. A graph will appear below which is interactive; drag the x and y scales to change them, double click to reset the axes."),
                          
                          selectInput("selected_pollutant", "Select Pollutants", choices = unique(combined_historic_and_projected$pollutant), multiple = TRUE),
                          
                          selectInput("selected_source", "Select Source", choices = unique(combined_historic_and_projected$source_description), multiple = TRUE),
                          
                          h3("2022 top pollution sources:"), tableOutput("top_sources")),  
                        
                        
                        mainPanel(
                          fluidRow(
                            column(12, h3("Trends over time:"), plotlyOutput("line_graphs_one_category"))
                          )))
                      
             ),
             
             
             tabPanel("Greenhouse Gases By Source",
                      
                      sidebarLayout(
                        
                        sidebarPanel(
                          
                          width = 4,
                          
                          p("Use the drop down boxes below to select a greenhouse gas, a NFR category and the subcatergories of interest. A graph will appear below which is interactive; drag the x and y scales to change them, double click to reset the axes."),
                          
                          selectInput("selected_ghg", "Select Greenhouse Gases", choices = unique(ghg_data$greenhouse_gas), multiple = TRUE),
                          
                          selectInput("selected_source_ghg", "Select Source", choices = unique(ghg_data$Source), multiple = TRUE),
                          
                          h3("2022 top GHG sources:"), tableOutput("top_sources_ghg")),  
                        
                        
                        mainPanel(
                          fluidRow(
                            column(12, h3("Trends over time:"), plotlyOutput("line_graphs_one_category_ghg"))
                          )))
                      
             )
             
             ))




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
      filter(year == "2022") |> 
      group_by(pollutant) |> 
      arrange(desc(emission)) |> 
      do(head(., n=5)) |> 
      arrange(desc(emission)) 
  })
  
  
  output$top_sources <- renderTable(top_sources_table() |> 
                                      select("NFR_code", "pollutant", "emission", "source_description") |> 
                                      rename("NFR Code" = NFR_code, "Pollutant" = "pollutant", "Emission (variable units)" = "emission", "Source" = "source_description"))
  
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
        scale_color_brewer(palette = "Dark2") +
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
  
  
  # NEED TO CHANGE BECAUSE CAN HAVE MORE THAN ONE POLLUTANT SELECTED AND HENCE MORE THAN ONE UNIT!!! 
  
  
  y_axis_label_correct_units <- reactive({
    
    
    # Filter and create a named vector of units for each selected pollutant
    units_vector <- list_of_pollutants_and_units |> 
      filter(Pollutant %in% input$selected_pollutant) |> # Get the pollutants selected, allowing for the fact there can be more than one
      select(Pollutant, Units) |> 
      deframe()  # Converts to a named vector where Pollutant is the name and Units is the value
    
    return(units_vector)
  })
  
  
  
  
  output$line_graphs_one_category <- renderPlotly({
    
    req(input$selected_source)
    req(input$selected_pollutant)
    
    # Check if there is data available in selected_source
    data <- selected_source()
    
    # Use validate and need to show a message if no data is available... 
    validate(
      need(nrow(data) > 0 & !all(is.na(data$emission)), "No data available for the selected source and pollutant. Please select an alternative"))
    
    
    pollutant_labeller <- function(variable, value) {
      units <- y_axis_label_correct_units() # Takes the unit vectors from the y_axis_label function call 
      paste0(value, " (", units[value], ")") # Returns the label values 
    }
    
    ggplotly(
      ggplot(selected_source()) +
        geom_line(aes(x=year, y = emission, colour = source_description, linetype = status)) +
        geom_point(aes(x=year, y = emission, colour = source_description, shape = status, 
                       text = paste(
                         "Year:", format(year, "%Y"), "<br>",
                         "Emissions:", round(emission, 2), y_axis_label_correct_units(),  "<br>",
                         "Pollution source:", source_description, "<br>",
                         "Data Source:", status, "<br>",
                         "NFR Code:", NFR_code
                       ))) +
        scale_y_continuous(name = "Emissions", limits = c(0,NA)) +
        facet_wrap(~pollutant , scales = "free_y", ncol = 2, labeller = pollutant_labeller) +
        scale_x_date(name = "Year", limits = as.Date(c("1990-01-01", "2050-01-01")), breaks = seq(as.Date("1990-01-01"), as.Date("2050-01-01"), by = "10 years"), labels = date_format("%Y")) +
        scale_colour_manual(values = colour_mappings) +
        theme_classic() +
        theme(panel.grid.major.y = element_line(colour = "lightgrey"), 
              strip.background = element_rect(colour = "white"), 
              strip.placement = "bottom") +
        theme(legend.position = "none") +
        theme(axis.title = element_text(size = 20), 
              strip.text = element_text(size = 10)), height = 600, width = 1000, tooltip = "text")
  })
  
  
  
# Now the GHG section..... 
  
  selected_data_ghg <- reactive(ghg_data |> # Test to see what one of these looks like
                              filter(greenhouse_gas %in% input$selected_ghg) |> 
                              filter(NFR_mid != "NA") 
                            
  )
  
  
  
  selected_source_ghg <- reactive(ghg_data |> # Test to see what one of these looks like
                                filter(greenhouse_gas %in% input$selected_ghg) |> 
                                filter(Source %in% input$selected_source_ghg) 
  )
  
  
  
  
  output$line_graphs_one_category_ghg <- renderPlotly({
    
    req(input$selected_source_ghg)
    req(input$selected_ghg)
    
    # Check if there is data available in selected_source
    data <- selected_source_ghg()
    
    # Use validate and need to show a message if no data is available... 
    validate(
      need(nrow(data) > 0 & !all(is.na(data$emission)), "No data available for the selected source and greenhouse gas. Please select an alternative"))
    
    
    
    ggplotly(
      ggplot(selected_source_ghg()) +
        geom_line(aes(x=year, y = emission, colour = Activity)) +
        geom_point(aes(x=year, y = emission, colour = Activity, 
                       text = paste(
                         "Year:", format(year, "%Y"), "<br>",
                         "Emissions:", round(emission, 2), "<br>",
                         "Pollution source:", Source, "<br>",
                         "Activity:", Activity, "<br>",
                         "NFR Code:", NFR_code
                       ))) +
        scale_y_continuous(name = "Emissions", limits = c(0,NA)) +
        facet_wrap(~greenhouse_gas , scales = "free_y", ncol = 2) +
        scale_x_date(name = "Year", limits = as.Date(c("1990-01-01", "2050-01-01")), breaks = seq(as.Date("1990-01-01"), as.Date("2050-01-01"), by = "10 years"), labels = date_format("%Y")) +
        theme_classic() +
        scale_colour_brewer(palette = "Set2") +
        theme(panel.grid.major.y = element_line(colour = "lightgrey"), 
              strip.background = element_rect(colour = "white"), 
              strip.placement = "bottom") +
        theme(legend.position = "none") +
        theme(axis.title = element_text(size = 20), 
              strip.text = element_text(size = 10)), height = 600, width = 1000, tooltip = "text")
    
    
    
    
    
  })
  
  top_sources_ghg_table <- reactive({
    req(input$selected_ghg) 
    filter(ghg_data, greenhouse_gas %in% input$selected_ghg) |> 
     mutate(year = substring(as.character(year), 1,4) ) |> 
      filter(year == "2022") |> 
      group_by(greenhouse_gas) |> 
      arrange(desc(emission)) |> 
      do(head(., n=5)) |> 
      arrange(desc(emission)) 
  })
  
  
  output$top_sources_ghg <- renderTable(top_sources_ghg_table() |> 
                                      select("NFR_code", "greenhouse_gas", "emission", "source_description") |> 
                                      rename("NFR Code" = NFR_code, "Greenhouse Gas" = "greenhouse_gas", "Emission (variable units)" = "emission", "Source" = "source_description"))
  
  
  session$onSessionEnded(function() {
    stopApp()
  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)