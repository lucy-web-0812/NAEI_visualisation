# Trialilng to determine what has biggest rates of change


# Could do all as relative to the average of the first 5 years? 



first_5_years_data <- combined_historic_and_projected |> 
  filter(status == "Historic") |> 
  group_by(pollutant, source_description) |> 
  mutate(
    start_year = (min(year)),          # start year for each pollutant
    cutoff_year = start_year + years(5) ) |>  # and the cut off year 5 years later......
  filter(year >= start_year & year < cutoff_year) |>  # Retain rows within 5-year range
  select(-start_year, -cutoff_year)  



mean_of_first_5_years <- first_5_years_data |> 
  group_by(pollutant, source_description) |> 
  summarise(mean_baseline_emission = mean(emission, na.rm = T))


ggplot(first_5_years_data) +
  geom_point(aes(x=year, y = emission, colour = pollutant))




comparative_to_baseline <- combined_historic_and_projected |> 
  right_join(mean_of_first_5_years, by = c("pollutant", "source_description")) |> 
  mutate(emission = (emission / mean_baseline_emission) * 100) 



p <- ggplot(filter(comparative_to_baseline, pollutant == "NOx\n(as NO2)")) +
  geom_line(aes(x = year, y = emission_relative_to_baseline, colour = source_description)) +
  scale_y_continuous(limits = c(NA, 200)) +
  facet_wrap(~NFR_wide.x) +
  theme(legend.position = "none")



ggplotly(p)


write.csv(comparative_to_baseline, file = "data/comparative_to_baseline_test.csv")

