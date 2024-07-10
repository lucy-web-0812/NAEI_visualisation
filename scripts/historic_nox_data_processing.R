library(tidyverse)
library(lubridate)

# Historic Emissions 

# Attempting to add in the historic NOx emissions from each sector 

historic_nox <- read.csv("data/NOx_historic_data.csv") |> # Read in data and get rid of pollutant and unit column - all in kilotonnes!
  select(-c("Gas", "Units")) 

colnames(historic_nox) <- c("NFR_code", "source", "activity", seq(1970,2021, by = 1)) # Renaming columns 


  
# Reformat the data to make it long 

historic_nox_long <- historic_nox |>  
  pivot_longer(cols = c("1970":"2021"), names_to = "year", values_to = "emission") |> 
  mutate(year = as_date(year, format = "%Y"), emission = as.numeric(emission)) |> 
  left_join(codes_and_descriptions, by = "NFR_code") # Adding in the source description

# Extracting the category totals 

category_totals <- historic_nox_long |> 
  filter(source == "") |> 
  select(-c("source", "activity"))

baseline_emissions <- historic_nox_long |> 
  filter(year == "1990-01-01") |> 
  group_by(NFR_code) |> 
  summarise(total_emission_by_NFR_source = sum(emission)) |> 
  rename(baseline_emission = total_emission_by_NFR_source) 
 

ggplot(category_totals) +
  geom_point(aes(x = year, y = emission, colour = source_description)) +
  geom_line(aes(x = year, y = emission, colour = source_description)) +
  facet_wrap(~source_description, scales = "free_y") +
  theme(legend.position = "none")

# Now need to bind to the projection data.However, this is much less detailed in terms of source, so will need to add 
# together the emissions from the same NFR code

a <- historic_nox_long |> 
  #filter(source != "") |>  # Firstly removed the category totals 
  group_by(NFR_code, year) |> 
  summarise(emission = sum(emission, na.rm = TRUE)) |> 
  left_join(codes_and_descriptions, by = "NFR_code") |> 
  mutate(status = "Historic")




# Bind to the projected data 

nox_test_data <-  pollutant_by_year |> # Extract just the NOX
  filter(pollutant == "NOx\n(as NO2)") |> 
  left_join(codes_and_descriptions, by = "NFR_code") |> 
  select(-c("NFR_wide","NFR_narrow", "pollutant")) |>  # Prep dataframe to be merged
  filter(is.na(emission) == FALSE) |> 
  mutate(status = "Projection")

joined_data <- nox_test_data |> 
  rbind(a) |> 
  left_join(baseline_emissions, by = "NFR_code") |> 
  mutate(emission_relative_to_baseline = emission / baseline_emission * 100)



p <- ggplot(joined_data) +
  geom_point(aes(x = year, y = emission_relative_to_baseline, colour = source_description, shape = status)) +
  geom_line(aes(x = year, y = emission_relative_to_baseline, colour = source_description, linetype = status)) +
  scale_x_date(name = "Year") + #limits = as.Date(c('1990-01-01','2050-01-01'), format = "%Y")) +
  scale_y_continuous(name = "Emissions (kilotonnes)") +
  theme(legend.position = "none") +
  facet_wrap(~source_description, scales = "free_y")


ggplotly(p)


filtered_source <- joined_data |> 
  filter(str_detect(NFR_code, "^3D")) 


ggplot(filtered_source) +
  geom_point(aes(x = year, y = emission_relative_to_baseline, colour = source_description, shape = status)) +
  geom_line(aes(x = year, y = emission_relative_to_baseline, colour = source_description, linetype = status)) +
  scale_x_date(name = "Year") + #limits = as.Date(c('1990-01-01','2050-01-01'), format = "%Y")) +
  scale_y_continuous(name = "Emissions (kilotonnes)", limits = c(0,120)) +
  theme(legend.position = "none") +
  facet_wrap(~source_description, scales = "fixed")



