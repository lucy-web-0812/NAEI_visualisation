library(readxl)
library(tidyverse)
library(shiny)
library(plotly)
library(bslib)
library(scales)
library(RColorBrewer)
library(shinydashboard)
library(DT)

# ---------- Preprocessing required is in the script data_processing ------------
# The output of this script is the combined_historic_and_projected csv dataset


combined_historic_and_projected <- read.csv("data/comparative_to_baseline_test.csv") |> # just changed this line here
  mutate(year = as.Date(year, format = "%Y")) |> 
  select(-"X") 

list_of_pollutants_and_units <- read.csv("data/list_of_pollutants.csv") |> 
  select(-"X") |> 
  mutate(pollutant = ifelse(Pollutant == "Total 1-4", "Total PAHs", Pollutant))

totals <- read.csv("data/totals.csv") |> 
  mutate(year = as.Date(year, format = "%Y")) |> 
  select(-"X") |> 
  pivot_longer(cols = c(emission, emissions_relative_to_baseline), names_to = "emission_type", values_to = "emission_long")


ghg_data <- read.csv("data/ghg_data.csv") |> 
  mutate(year = as.Date(paste0(year, "-01-01"), format = "%Y-%m-%d"))


# Air Pollution Colours....

n_colours <- length(unique(combined_historic_and_projected$source_description))

set.seed(123)


source_colour_mappings <- data.frame(
  source_description = unique(combined_historic_and_projected$source_description),
  colour = sample(colorRampPalette(brewer.pal(9, "Paired"))(n_colours), n_colours) # Sample makes the colours random rather than in order
)

colour_mappings <- setNames(source_colour_mappings$colour, source_colour_mappings$source_description)




# Greenhouse gas colours.... 

n_colours_ghg <- length(unique(interaction(ghg_data$Activity, ghg_data$Source)))

source_colour_mappings_ghg <- data.frame(
  Source = unique(interaction(ghg_data$Activity, ghg_data$Source)),
  colour = sample(colorRampPalette(brewer.pal(9, "Paired"))(n_colours_ghg), n_colours_ghg) # Sample makes the colours random rather than in order
)

colour_mappings_ghg <- setNames(source_colour_mappings_ghg$colour, source_colour_mappings_ghg$Source)







# ----- The user interface ------------   

ui <- dashboardPage(
  skin = "blue", 
  
  dashboardHeader(title = "NAEI Visualiser"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("   Air Pollutants", icon = icon("head-side-cough"),
               menuSubItem("Totals", tabName = "air_pollutants_totals", icon = icon("chart-line")),
               menuSubItem("By Source", tabName = "air_pollutants_by_source", icon = icon("layer-group"))),
      menuItem("   Greenhouse Gases", icon = icon("cloud"),
               menuSubItem("Totals", tabName = "ghg_totals", icon = icon("chart-line")),
               menuSubItem("By Source", tabName = "ghg_by_source", icon = icon("layer-group")))

  )),
  
  dashboardBody(
    
    tags$head(
      tags$link(
        href="https://fonts.googleapis.com/css2?family=Open+Sans:ital,wght@0,300..800;1,300..800&display=swap",
        rel = "stylesheet"
      ),
      
      tags$style(HTML("
      body {
        font-family: 'Open Sans', sans-serif;
        font-size: 14px;
        line-height: 1.6;
      }
      .sidebar-menu > li > a {
        font-weight: 400;
      }
      .skin-blue .main-header .navbar {
       background-color: #C3E1E3;
      } 
     
      .skin-blue .main-sidebar {
        background-color: #383F51;
        transition: background-color 0.3s, color 0.3s;
      }
      .skin-blue .sidebar-menu > li.active > a,
      .skin-blue .sidebar-menu > li:hover > a {
        background-color: #ff69b4;
        color: #FFFFFF;
      }
      .skin-blue .sidebar-menu > li > a {
        color: #FFFFFF;
        border-radius: 4px;
      }
      .skin-blue .sidebar-menu > li > a:hover {
        background-color: #ff69b4;
        color: #FFFFFF;
        transition: background-color 0.3s, color 0.3s;
      }
      .content-wrapper {
        background-color: #ffffff;
      }
      .table-striped-green tbody tr:nth-child(odd) {
        background-color: #ACC3B8;
      }
      .table-striped-green tbody tr:nth-child(even) {
        background-color: #f8f9fa;
      }
      .skin-blue .main-header .logo {
      text-overflow: ellipsis; /* Show ellipsis for overflow text */
      background-color: #C3E1E3; /* Correct background-color  */
      color: #000000; /* Ensure title text is visible */
      font-size: 24px; /* Adjust title size */
      font-weight: 700; /* Bold text for emphasis */
      padding-left: 10px; /* Add some spacing */
      }
      .box {
        border-radius: 8px;
      }
      .box {
         box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1);
      }
    "))),
    
    tabItems(
      # First page - Air Pollutant Totals
      tabItem(tabName = "air_pollutants_totals",
              
              h2("Relative and absolute change in pollutants over time:"), 
              
              fluidRow(
                column(4, selectInput("relative_or_absolute", "Change the y axis scale:", choices = c("Relative to baseline" = "emissions_relative_to_baseline", "Absolute" = "emission")))
              ),
              fluidRow(column(8, plotlyOutput("totals_plot")))
      ),
      
      # Second page - Air Pollutants By Source
      tabItem(tabName = "air_pollutants_by_source",
              h2("Air Pollutants By Source:"), 
              
              # Top-level layout
              fluidRow(
                # Sidebar with inputs and table
                column(
                  width = 4,
                  wellPanel(
                    p("Use the drop-down boxes below to select a pollutant, a NFR category, and the subcategories of interest."),
                    selectInput(
                      "selected_pollutant", 
                      "Select Pollutants", 
                      choices = unique(combined_historic_and_projected$pollutant), 
                      multiple = TRUE
                    ),
                    
                    selectInput(
                      "selected_source", 
                      "Select Source", 
                      choices = unique(combined_historic_and_projected$source_description), 
                      multiple = TRUE
                    ),
                    
                    downloadButton("source_data_download", label = "Download this data")
                  ),
                  br(), # Add spacing between the sidebar and the table
                  
                  div(
                    style = "background-color: #F9F9F9; padding: 15px; border-radius: 10px; box-shadow: 0px 2px 5px rgba(0,0,0,0.1);",
                    h3("2022 Top Pollution Sources:"),
                    dataTableOutput("top_sources")
                  )),
                
                # Main panel for graph
                column(
                  width = 8,
                  h3("Trends Over Time:"),
                  plotlyOutput("line_graphs_one_category")
                )
              )
      ), 
      
      
      # Third page - Greenhouse Gases Totals -- IN DeVELOPMENT
      tabItem(tabName = "ghg_totals",
              
              h2("Relative and absolute change in greenhouse gases over time:"),
              
              h3("In development...."), 
              
              fluidRow(
                column(4, selectInput("relative_or_absolute", "Change the y axis scale:", choices = c("Relative to baseline" = "emissions_relative_to_baseline", "Absolute" = "emission")))
              ),
              fluidRow(column(8, plotlyOutput("totals_plot_ghg")))
      ), 
      
      
      
      # Fourth page - Greenhouse Gases by Source
      tabItem(tabName = "ghg_by_source",
              h2("Greenhouse Gases By Source:"), 
              
              # Main layout
              fluidRow(
                # Sidebar for GHG inputs and table
                column(
                  width = 4,
                  wellPanel(
                    p("Use the drop-down boxes below to select a greenhouse gas, a NFR category, and the subcategories of interest."),
                    selectInput(
                      "selected_ghg", 
                      "Select Greenhouse Gases", 
                      choices = unique(ghg_data$greenhouse_gas), 
                      multiple = TRUE
                    ),
                    selectInput(
                      "selected_source_ghg", 
                      "Select Source", 
                      choices = unique(ghg_data$Source), 
                      multiple = TRUE
                    ),
                    downloadButton("source_data_download_ghg", label = "Download this data")
                  ),
                  br(),
                  div(
                    style = "background-color: #f9f9f9; padding: 15px; border-radius: 5px; box-shadow: 0px 2px 5px rgba(0,0,0,0.1);",
                    h3("2022 Top GHG Sources:"),
                    dataTableOutput("top_sources_ghg")
                  )
                ),
                
                # Main panel for GHG graph
                column(
                  width = 8,
                  h3("Trends Over Time:"),
                  plotlyOutput("line_graphs_one_category_ghg")
                )
              )
      )
      
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
      mutate(year = substring(as.character(year), 1,4), emission = round(emission,2) ) |> 
      filter(year == "2022") |>
      group_by(pollutant) |> 
      arrange(desc(emission)) 
  })
  
  
  
  output$top_sources <- DT::renderDT({
    top_sources_table() |>
      select("NFR_code", "pollutant", "emission", "source_description") |> 
      rename("NFR Code" = NFR_code, "Pollutant" = "pollutant", "Emission (variable units)" = "emission", "Source" = "source_description")
            }, 
            options = list(
              scrollX = TRUE,
              pageLength = 5,    # Number of rows per page 
              dom = 'Bfrtip',     # Add buttons for export and search
              style = "bootstrap4" # Modern styling
            )) 
            
  
  
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
                       )), show.legend = FALSE) +
        geom_line(aes(x = year, y = emission_long, colour = pollutant, linetype = data_source), show.legend = FALSE) +
        scale_color_brewer(name = "Pollutant", palette = "Dark2") +
        scale_x_date(name = "Year", limits = as.Date(c("1970-01-01", "2050-01-01")), breaks = seq(as.Date("1970-01-01"), as.Date("2050-01-01"), by = "10 years"), labels = date_format("%Y")) +
        scale_y_continuous(name =   
                             y_axis_label(), limits = c(0, NA), expand = c(0,0)) +
        theme_classic() +
        theme(panel.grid.major.y = element_line(colour = "lightgrey"),
              strip.background = element_rect(colour = "white"),
              strip.placement = "bottom") +
        theme(legend.position = "right") +
        guides(colour = guide_legend(override.aes = list(shape = NA, linetype = NA))) + 
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
              plot.margin = margin(0.25,0.25,0.25,0.55, unit = "cm"),
              strip.text = element_text(size = 10)), height = 600, width = 1000, tooltip = "text")
  })
  
 
  
# If the user wants to download that data..... 
  
  
  output$source_data_download <- downloadHandler(
    filename = function() {
      paste0(input$selected_pollutant,"_", input$selected_source,".csv")
    },
    content = function(file) {
      
      download_content <- selected_source() |> 
        select(NFR_wide.x:status) |> 
        rename(NFR_decsription = NFR_wide.x) |> 
        left_join(list_of_pollutants_and_units, by = "pollutant")
      
      write.csv(download_content, file, row.names = FALSE)
    }
  )
  
  
   
  
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
        geom_line(aes(x=year, y = emission, colour = interaction(Activity, Source), group = source_description)) +
        geom_point(aes(x=year, y = emission, colour = interaction(Activity, Source), group = source_description,
                       text = paste(
                         "Year:", format(year, "%Y"), "<br>",
                         "Emissions:", round(emission, 2), `Units`, "<br>",
                         "Pollution source:", Source, "<br>",
                         "Activity:", Activity, "<br>",
                         "NFR Code:", NFR_code
                       ))) +
        scale_y_continuous(name = "Emissions", limits = c(NA,NA)) +
        facet_wrap(~greenhouse_gas , scales = "free_y", ncol = 2) +
        scale_x_date(name = "Year", limits = as.Date(c("1990-01-01", "2025-01-01")), breaks = seq(as.Date("1990-01-01"), as.Date("2025-01-01"), by = "5 years"), labels = date_format("%Y")) +
        theme_classic() +
        scale_colour_manual(values = colour_mappings_ghg) +
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
      arrange(desc(emission)) #|> 
      # do(head(., n=25)) |> 
      # arrange(desc(emission)) 
  })
  
  
  output$top_sources_ghg <- DT::renderDT({
    top_sources_ghg_table() |>
      select("NFR_code", "greenhouse_gas", "emission", "Units", "Source") |>
      rename("NFR Code" = NFR_code, "Greenhouse Gas" = greenhouse_gas,"Emission" = emission)
  }, 
  options = list(
    scrollX = TRUE,
    pageLength = 5,    # Number of rows per page 
    dom = 'Bfrtip',     # Add buttons for export and search
    style = "bootstrap4" # Modern styling
  ),
  class = "table table-striped table-bordered"
  ) 
  
  
  
  output$source_data_download_ghg <- downloadHandler(
    filename = function() {
      paste0(input$selected_ghg,"_", input$selected_source,".csv")
    },
    content = function(file) {
      
      download_content <- selected_source_ghg() |> 
        select(greenhouse_gas:emission)
      
      write.csv(download_content, file, row.names = FALSE)
    }
  )
  
  
  
  
  session$onSessionEnded(function() {
    stopApp()
  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)