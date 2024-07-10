
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
  filter(str_detect(files, "20") & str_detect(files, "csv")) |> 
  mutate(year = substr(files, 1, 4)) |> 
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
  rename(NFR_wide = `NFR Aggregation for Gridding and LPS (GNFR)`, source = `Long name`, NFR_code = `NFR Code`) |> # making names easier to write out
  mutate(year = as_date(year, format = "%Y"), NFR_wide = as.factor(NFR_wide), source = as.factor(source), NFR_code = as.factor(NFR_code)) |> 
  mutate(emission = as.numeric(emission)) |>  # Get a warning with this line but is what we want to happen - non-numeric values are turned to IE 
  mutate(status = "Projected")


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



# ----- HISTORIC ------ 

# NAEI database splits into air pollutants and particulate matter
# Firstly read in these datafiles and then combine into one dataframe 

air_pollutants <- read.csv("data/air_pollutants_historic_emissions.csv") 

pm_pollutants <- read.csv("data/particulate_matter_historic_emissions.csv")

historic_emissions_raw <- rbind(air_pollutants, pm_pollutants) |> 
  select(-"Units") # Can get rid of the units column as all in kilotonnes

colnames(historic_emissions_raw) <- c("pollutant","NFR_code", "source", "activity", seq(1970,2021, by = 1))



historic_emissions <- historic_emissions_raw |> 
  pivot_longer(cols = c("1970":"2021"), names_to = "year", values_to = "emission") |> 
  mutate(year = as_date(year, format = "%Y"), emission = as.numeric(emission)) |> 
  mutate(pollutant = ifelse(pollutant == "PM10 (Particulate Matter < 10&micro;m)", "PM10", pollutant)) |> 
  mutate(pollutant = ifelse(pollutant == "PM2.5 (Particulate Matter < 2.5&micro;m)", "PM2.5", pollutant)) |> 
  filter(pollutant != "PM0.1 (Particulate Matter < 0.1&micro;m)" & pollutant != "PM1 (Particulate Matter < 1&micro;m)") |> 
  group_by(pollutant, NFR_code, year) |> 
  summarise(total_emission = sum(emission)) |> 
  rename(emission = total_emission) |> 
  mutate(status = "Historic")
  

# ---- BASELINE EMISSIONS ----- 


baseline_emissions <-  historic_emissions |> 
  group_by(pollutant, NFR_code, year) |> 
  summarise(summed_emission = sum(emission)) |> 
  group_by(NFR_code, pollutant) |> 
  filter(is.na(summed_emission) == FALSE) |> 
  arrange(is.na(summed_emission)) |> 
  slice_head(n = 1) |> 
  rename(baseline_emission = summed_emission)



# ---- COMBINING THE HISTORIC AND PROJECTED --- 

# Join the datasets and then add in the baseline emissions 


combined_historic_and_projected <- processed_data_frame |> 
  select(-c(NFR_wide, source)) |> 
  filter(pollutant %in% c("NOx\n(as NO2)", "NMVOC", "SOx \n(as SO2)",  "PM2.5", "PM10", "CO")) |> 
  mutate(pollutant = ifelse(pollutant == "NOx\n(as NO2)", "Nitrogen oxides (NOx expressed as NO2)", pollutant)) |> # Renaming the pollutants to make consistent between the datasets. 
  mutate(pollutant = ifelse(pollutant == "NMVOC", "Non Methane VOC", pollutant)) |> 
  mutate(pollutant = ifelse(pollutant == "CO", "Carbon Monoxide", pollutant)) |> 
  mutate(pollutant = ifelse(pollutant == "SOx \n(as SO2)", "Sulphur Dioxide", pollutant)) |> 
  rbind(historic_emissions) |> 
  left_join(codes_and_descriptions)


# A list of sources that can be used for the filtering later on when visualising 

list_of_sources <- unique(combined_historic_and_projected$NFR_code)


combined_historic_and_projected_filtered <- combined_historic_and_projected |> 
  filter(str_detect(NFR_code,"^1B")) |> 
  left_join(codes_and_descriptions)

ggplot(combined_historic_and_projected_filtered) +
  geom_line(aes(x= year, y = emission, linetype = status, colour = pollutant)) +
  facet_wrap(~source_description, scales = "free_y")


write.csv(combined_historic_and_projected, file = "data/combined_historic_and_projected.csv")




