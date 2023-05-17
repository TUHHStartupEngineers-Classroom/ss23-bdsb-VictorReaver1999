# Challenge 5

# Part 2

# Import Libraries

library(ggplot2)
library(tidyverse)
library(dplyr)
library(lubridate)
library(maps)
library(scales)

# Load the world map data
world <- map_data("world")

# Read the dataset
covid_data_tbl <- read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv")

# Filter out unnecessary cols
covid_data_tbl <- covid_data_tbl %>%
  select(location, date, total_deaths, total_cases, population)

# Fix the country names
covid_data_tbl <- covid_data_tbl %>%
  mutate(location = case_when(
    location == "United Kingdom" ~ "UK",
    location == "United States" ~ "USA",
    location == "Democratic Republic of Congo" ~ "Democratic Republic of the Congo",
    TRUE ~ location
  )) %>%
  distinct()

# Filter the data for the desired date
target_date <- as.Date("2021-04-16")
filtered_data <- covid_data_tbl %>%
  filter(date == target_date)

# Calculate mortality rate
plot_data <- filtered_data %>% mutate(mortality_rate = total_deaths / population)

# Join the datasets
world_covid_data <- left_join(world, plot_data, by = c("region" = "location"))

# Filter world_covid_data to only keep rows that correspond to locations in filtered_data
world_covid_data <- world_covid_data %>% mutate(mortality_rate = mortality_rate * 100)

# Plot the map
plot <- ggplot() +
  geom_polygon(data = world_covid_data, aes(x = long, y = lat, group = group, fill = mortality_rate)) +
  scale_fill_gradient(name = "Mortality Rate", low = "lightpink", high = "darkred", na.value = "gray", limits = c(0, 0.7)) +
  labs(title = paste("Distribution of Mortality Rate (As of", format(target_date, "%d/%m/%Y"), ")")) +
  theme_void()

# Save the map
ggsave("covid_map.png", plot)

