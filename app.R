library(readxl)
library(dplyr)
library(tidyverse)
library(shiny)
library(bslib)

# ---------- Preprocessing required ------------


# Read in the data and separate the excel file into the separate csv sheets 

excel_file <- "data/UK_AnnexIV_Submission_2024_ProjectedEmissions_to2050.xlsx" # Where the file is in the wd

output_folder <- "data" # The folder where the csvs should go

sheet_names <- readxl::excel_sheets(excel_file) # The names of the sheets as strings


# Loop to extract the data from each spreadsheet and save as separate csv
for (sheet_name in sheet_names) {
  
  sheet_data <- readxl::read_excel(excel_file, sheet = sheet_name)
  
  csv_file <- file.path(output_folder, paste0(sheet_name,   ".csv"))
  
  write.csv(sheet_data, file = csv_file, row.names = FALSE) 
  
  cat(paste("Sheet", sheet_name, "exported to", csv_file, "\n")) #Outputs to console when saved a file
  
} 


files <- list.files(path = "data") 

# Keeping a record of just the files that are actually the useful csvs 

year_assigned_files <- data.frame(files)  |>    
  filter(str_detect(files, "20") & str_detect(files, "csv")) |> 
  mutate(year = substr(files, 1, 4))  |> 
  filter(year != "tren")

# The spreadsheet data is not nicely formatted. Reformat in R rather than in excel so reproducible. 
# The following sheets have the same format so a function can be used to do this processing - 
# "2005" 
# "PROJ_BASE_YEAR"
# "2025_WM"
# "2030_WM"
# "2035_WM"
# "2040_WM"
# "2045_WM"
# "2050_WM"


# make an empty list to store the outputs of the loop

processed_data <- list()


# This loops over each of the seven csv files to perform the data cleansing required 


for (i in 1:nrow(year_assigned_files)) { 
  
  raw_data <- read.csv(paste0("data/",year_assigned_files$files[i]))[-(1:8),] # Removing all the top spiel 
  
  row.names(raw_data) <- 1:nrow(raw_data) # Reindexing to start rows at 1
  
  # Extracting a list of pollutants and their units 
  
  list_of_pollutants_and_units <- raw_data[(3:4),(5:30)] |> 
    t() 
  
  
  
  row.names(list_of_pollutants_and_units) <- 1:nrow(list_of_pollutants_and_units) # Rename rows
  colnames(list_of_pollutants_and_units) <- c("Pollutant", "Units") # Rename columns 
  
  
  colnames(raw_data) <- c("NFR Aggregation for Gridding and LPS (GNFR)", "NFR Code", "Long name", "Notes", raw_data[3,(5:30)]) # Changing the column names 
  
  
  raw_data_reshaped <- raw_data |> 
    select(c("NFR Aggregation for Gridding and LPS (GNFR)": "PCBs")) # Removing the activity information 
  
  
  # And store in the list 
  
  processed_data[[i]]<- raw_data_reshaped[-(1:4),] |>  
    mutate(year = year_assigned_files$year[i]) |> # Add in the year as a column 
    pivot_longer(cols = "NOx\n(as NO2)":"PCBs", names_to = "pollutant", values_to = "emission") |> # Perform pivot to have data in long format thats easier to analyse 
    select(-c("Notes")) |>  # Remove unnecessary column 
    filter(is.na(emission) == FALSE)
  
  # Need to figure out a way to keep just the important data and not the bottom bit - removing where emission is blank should work 
  
  
}

# Bind all the rows together to get a data frame and some final changes to make the analysis work 

processed_data_frame <- bind_rows(processed_data) |> 
  rename(NFR_wide = `NFR Aggregation for Gridding and LPS (GNFR)`, NFR_narrow = `Long name`, NFR_code = `NFR Code`) |> # making names easier to write out
  mutate(year = as_date(year, format = "%Y"), NFR_wide = as.factor(NFR_wide), NFR_narrow = as.factor(NFR_narrow), NFR_code = as.factor(NFR_code)) |> 
  mutate(emission = as.numeric(emission)) # Get a warning with this line but is what we want to happen - non-numeric values are turned to IE

# ----- The user interface ------------   
  
  ui <- fluidPage(
    
    titlePanel("NAEI Projections", windowTitle = "Hi Charlotte"), 
    
    tabsetPanel(
      tabPanel("Totals", 
        fluidRow(
          column(4, selectInput("selected_pollutant", "Pollutant", choices = unique(processed_data_frame$pollutant))),
        )       
               
               ), 
      tabPanel("By Source",
  
    
    fluidRow(
      column(4, 
             selectInput("selected_pollutant", "Pollutant", choices = unique(processed_data_frame$pollutant))),
      
      column(4, 
             selectInput("selected_NFR", "NFR Category", choices = unique(processed_data_frame$NFR_wide))),
      
      column(4, 
             selectInput("selected_source", "Source", choices = NULL))),
    
      
    
    fluidRow(
      
      
      column(10, plotOutput("line_graphs_one_category")), 
      
      column(10, plotOutput("line_graphs"))
      
     ))
    ))
      
    
  
  
# ---- And the server function ------

server <- function(input, output, session) {
  
  
  selected_data <- reactive(processed_data_frame |> # Test to see what one of these looks like
                              filter(pollutant == input$selected_pollutant) |> 
                              filter(NFR_wide != "NA")
    
  )
  
 
  
  
  selected_data_just_one_category <- reactive(processed_data_frame |> # Test to see what one of these looks like
                              filter(pollutant == input$selected_pollutant) |> 
                              filter(NFR_wide == input$selected_NFR) 
  )
  
  
  observeEvent(selected_data_just_one_category(), {
    choices <- unique(selected_data_just_one_category()$NFR_narrow)
    updateSelectInput(inputId = "selected_source", choices = choices)
  })

  
  selected_source <- reactive({
    req(input$selected_source)
    filter(selected_data_just_one_category(), NFR_narrow == input$selected_source)
  })
  

  
  output$line_graphs_one_category <- renderPlot({
    
    req(input$selected_source)
    
    ggplot(selected_source()) +
      geom_line(aes(x=year, y = emission, colour = NFR_narrow)) +
      geom_point(aes(x=year, y = emission, colour = NFR_narrow)) +
      ggtitle(label = "Emissions from selected source:") +
      scale_y_continuous(name = "Emissions") +
      scale_x_date(name = "Year") +
      theme_minimal() +
      theme(axis.title = element_text(size = 20, face = "bold"), 
            strip.text = element_text(size = 20, face = "bold")) +
      theme(legend.position = "none") 
    })
  
  
  
  
  output$line_graphs <- renderPlot(
    
    ggplot(selected_data()) +
      geom_line(aes(x=year, y = emission, colour = NFR_narrow)) +
      geom_point(aes(x=year, y = emission, colour = NFR_narrow)) +
      ggtitle(label = "All sectors and their emissions:") +
      scale_y_continuous(name = "Emissions") +
      scale_x_date(name = "Year") +
      facet_wrap(~NFR_wide, scale = "free", nrow = 3) +
      theme_minimal() +
      theme(axis.title = element_text(size = 20, face = "bold"), 
            strip.text = element_text(size = 20, face = "bold")) +
      theme(legend.position = "none") 
    )
      
  session$onSessionEnded(function() {
    stopApp()
  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)
