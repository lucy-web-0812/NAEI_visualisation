# Historic and Projected totals before looking at the subcategories 


# Need to run the data_processing script prior to this 

historic_totals <- read_csv("data/trends_in_air_emissions_2021.csv") |>  # https://naei.beis.gov.uk/data/data-selector?view=air-pollutants data from here
  mutate(year = as_date(as.character(year), format = "%Y")) |> 
  pivot_longer(cols = ("NH3":"SOx (as SO2)"), values_to = "emission", names_to = "pollutant") |> 
  mutate(data_source = "historic")

# Extracting the baseline emissions

baseline_emissions <- historic_totals |>  # e.g. what the emissions where at the time of first reporting so that we can compare on same scale
  filter(year == "1970-01-01" & is.na(emission) == FALSE | pollutant == "NH3" & year == "1980-01-01") |> 
  mutate(baseline_emission = emission) |> 
  select(c(pollutant, baseline_emission))

# And just retaining the air pollutant totals from the projected data 

projected_totals <- total_emissions_by_year |> 
  filter(NFR_code == "NATIONAL TOTAL") |> 
  select(!c(NFR_code, status, source)) |> 
  mutate(data_source = "projected") |> 
  mutate(pollutant = ifelse(pollutant == "NOx\n(as NO2)", "NOx (as NO2)", pollutant), 
         pollutant = ifelse(pollutant == "SOx \n(as SO2)", "SOx (as SO2)", pollutant)) 

# Binding these two data sets together

totals <- rbind(historic_totals, projected_totals) |> 
  filter(pollutant %in% c("NOx (as NO2)", "NH3", "SOx (as SO2)", "PM10", "PM2.5")) |> 
  left_join(baseline_emissions) |> 
  mutate(emissions_relative_to_baseline = emission/baseline_emission * 100)

write.csv(totals, file = "data/totals.csv")


ggplot(totals) +
  geom_point(aes(x = year, y = emissions_relative_to_baseline, colour = pollutant, shape = data_source)) +
  geom_line(aes(x = year, y = emissions_relative_to_baseline, colour = pollutant, linetype = data_source)) +
  scale_y_continuous(name = "Emissions relative to 1970* baseline")

