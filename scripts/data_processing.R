
library(readxl)
library(dplyr)
library(tidyverse)
library(plotly)
library(stringr)


# NFR Codes and descriptions 

codes_and_descriptions <- read.csv("data/NFRcodes_descriptions.csv") |>  # Taken from https://naei.beis.gov.uk/glossary?view=nfr
  rename(NFR_code = "NFR.Code", source_description = "Source.Description")



# ----- PROJECTED ------

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
  filter(str_detect(files, "20") & str_detect(files, "csv") & !str_detect(files, "NFR") & !str_detect(files, "green")) |> # This filtering required to remove everything else
  mutate(year = substr(files, 1, 4)) |> 
  filter(year != "tren") |> 
  mutate(full_paths = paste0("data/",  files))

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
  rename(NFR_wide = `NFR Aggregation for Gridding and LPS (GNFR)`, source = `Long name`, NFR_code = `NFR Code`) |> # making names easier to write out
  mutate(year = as_date(year, format = "%Y"), NFR_wide = as.factor(NFR_wide), source = as.factor(source), NFR_code = as.factor(NFR_code)) |> 
  mutate(emission = as.numeric(emission)) |>  # Get a warning with this line but is what we want to happen - non-numeric values are turned to IE 
  mutate(status = "Projected")


file.remove(year_assigned_files$full_paths) # Removing the extra csv files no longer needed

# --- Extra data wrangling to extract features ----- 
  
  
# Extract the totals from the dataframe - these also relate to CLRTAP etc targets


total_emissions_by_year <- processed_data_frame |> 
  filter(str_detect(NFR_code, regex("total", ignore_case = TRUE))) |> 
  select(-c("NFR_wide"))


ggplot(total_emissions_by_year) +
  geom_line(aes(x = year, y = emission, colour = pollutant)) +
  facet_wrap(~NFR_code)


# And then just keeping the rest of the data e.g. not the total categories

pollutants_by_year <- processed_data_frame |> 
  filter(!str_detect(NFR_code, regex("total", ignore_case = TRUE)))








# ----- TESTING OUT USING THE ALTERNATIVE MEGA SPREADSHEET DATA FOR the historic data ------- 

# Read in the data and separate the excel file into the separate csv sheets 

excel_file <- "data/UK_Annex1_Submission_2024_1990to2022_7 (2).xlsx" # Where the file is in the wd

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
  filter(str_detect(files, "NFR-19") & str_detect(files, "csv")) |> 
  mutate(year = substr(files, nchar(files) - 7, nchar(files) - 4)) |> # Getting the year from the sheet name
  mutate(full_paths = paste0("data/",  files))

# The spreadsheet data is not nicely formatted. Reformat in R rather than in excel so reproducible. 
# The sheets within the year_assigned_files have the same format so a function can be used to do this processing in the same way that happened for the projected

# Make an empty list to store the outputs of the loop

processed_data_historic <- list()


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
  processed_data_historic[[i]]<- raw_data_reshaped[-(1:4),] |>  
    mutate(year = year_assigned_files$year[i]) |> # Add in the year as a column 
    pivot_longer(cols = "NOx\n(as NO2)":"PCBs", names_to = "pollutant", values_to = "emission") |> # Perform pivot to have data in long format thats easier to analyse 
    select(-c("Notes")) |>  # Remove unnecessary column 
    filter(is.na(emission) == FALSE)
  
  # Need to figure out a way to keep just the important data and not the bottom bit - removing where emission is blank should work 
  
  
}

write.csv(list_of_pollutants_and_units, file = "data/list_of_pollutants.csv")

# Bind all the rows together to get a data frame and some final changes to make the analysis work 

processed_data_frame_historic <- bind_rows(processed_data_historic) |> 
  rename(NFR_wide = `NFR Aggregation for Gridding and LPS (GNFR)`, source = `Long name`, NFR_code = `NFR Code`) |> # making names easier to write out
  mutate(year = as_date(year, format = "%Y"), NFR_wide = as.factor(NFR_wide), source = as.factor(source), NFR_code = as.factor(NFR_code)) |> 
  mutate(emission = as.numeric(emission)) |>  # Get a warning with this line but is what we want to happen - non-numeric values are turned to IE 
  mutate(status = "Historic")

file.remove(year_assigned_files$full_paths) # Removing the extra csv files no longer needed



combined_historic_and_projected <- processed_data_frame_historic |> 
  rbind(processed_data_frame) |>  
  left_join(codes_and_descriptions, by = "NFR_code", relationship = "many-to-many") |> # Joining the projected and historic emissions 
  filter(is.na(NFR_wide.x) == FALSE) |> 
  mutate(pollutant = ifelse(pollutant == "Total 1-4", "Total PAHs", pollutant))


write.csv(combined_historic_and_projected, file = "data/combined_historic_and_projected.csv") # save this csv to be used... 

 

# Attempting to also add in the Greenhouse Gas Data.... 
# Currently some sources not working due to the fact NFR codes not in the codes and descriptions 
# And need units on this 

ghg_data <- read_csv("data/greenhouse_gases_1990_to_2022_a.csv") |> 
  pivot_longer(cols = c("1990":"2022"), names_to = "year", values_to = "emission") |> 
  rename(greenhouse_gas = "Gas") |> 
  mutate(year = as.numeric(year), emission = as.numeric(emission)) |> 
  rename(NFR_code = "NFR/CRF Group") |> 
  left_join(codes_and_descriptions, by = "NFR_code", relationship = "many-to-many") #|> 
 # filter(is.na(NFR_wide) == FALSE)






write.csv(ghg_data, file = "data/ghg_data.csv")



