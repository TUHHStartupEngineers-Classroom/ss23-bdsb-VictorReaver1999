library(ggplot2)
library(maps)
library(lubridate)
library(tidyverse)
library(dplyr)

world <- map_data("world")

# Read the dataset
covid_data_tbl <- read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv")

# Select the required columns
filtered_data <- covid_data_tbl %>%
  select(location, population, total_deaths, date)

# Filter the data for a specific date (April 16, 2021 in this case)
selected_date <- as.Date("2021-04-16")
filtered_data <- filtered_data %>%
  filter(date == selected_date) %>%
  select(location, population, total_deaths)

# Calculate the mortality rate
filtered_data <- filtered_data %>%
  mutate(mortality_rate = total_deaths / population)

# Rename specific countries for consistency
filtered_data$location <- case_when(
  filtered_data$location == "United Kingdom" ~ "UK",
  filtered_data$location == "United States" ~ "USA",
  filtered_data$location == "Democratic Republic of Congo" ~ "Democratic Republic of the Congo",
  TRUE ~ filtered_data$location
)

# Merge with the map data based on region/location
world_map <- map_data("world")
merged_data <- merge(world_map, filtered_data, by.x = "region", by.y = "location", all.x = TRUE)

# Create the plot using geom_polygon
plot <- ggplot() +
  geom_polygon(data = merged_data, aes(x = long, y = lat, group = group, fill = mortality_rate)) +
  scale_fill_gradient(low = "red", high = "purple", na.value = "gray", limits = c(0, 0.7)) +
  theme_void()

ggsave("map_covid.png", plot)
