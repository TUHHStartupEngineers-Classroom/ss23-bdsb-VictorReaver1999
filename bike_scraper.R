# WEBSCRAPING ----

# 1.0 LIBRARIES ----

library(rvest)
library(knitr)

# Define the URLs for each category and subcategory
urls <- c(
  "https://www.radon-bikes.de/en/mountainbike/hardtail/",
  "https://www.radon-bikes.de/en/mountainbike/fullsuspension/",
  "https://www.radon-bikes.de/en/trekking-cross/trekking/",
  "https://www.radon-bikes.de/en/trekking-cross/cross/",
  "https://www.radon-bikes.de/en/e-bike/mountainbike/",
  "https://www.radon-bikes.de/en/e-bike/trekking/",
  "https://www.radon-bikes.de/en/roadbike/carbon/",
  "https://www.radon-bikes.de/en/roadbike/alu/",
  "https://www.radon-bikes.de/en/roadbike/gravel/"
)

# Initialize an empty list to store the data for each category and subcategory
bike_list <- list()

# Iterate through each URL
for (url in urls) {

  # Get the HTML content from the URL
  html_content <- read_html(url)

  # Extract the category and subcategory from the URL
  category <- sub("^.*/([^/]+)/([^/]+)/?$", "\\1", url)
  subcategory <- sub("^.*/([^/]+)/([^/]+)/?$", "\\2", url)

  # Extract the bike names and prices from the HTML content
  bike_names <- html_content %>% html_nodes(".mod-serienpanel .a-heading--medium") %>% html_text()
  prices <- html_content %>%
    html_nodes(".info .currentPrice") %>%
    html_text()

  # Remove any empty prices
  prices <- prices[prices != ""]

  # Create a data frame for the subcategory with the bike names and prices
  subcategory_df <- data.frame(bike_name = bike_names[1:length(prices)], price = prices)

  # Add the subcategory data frame to the bike_list with the corresponding category and subcategory names
  bike_list[[paste0(category, "_", subcategory)]] <- subcategory_df

}

# Combine all the data frames in the bike_list into one data frame
bike_df <- do.call(rbind, bike_list)

# Add a column for the category and subcategory names
bike_df$category <- gsub("_.*", "", rownames(bike_df))
bike_df$subcategory <- gsub(".*_", "", rownames(bike_df))

# Reorder the columns
bike_df <- bike_df[,c(3,4,1,2)]

# Print the resulting data frame
# bike_df

# display dataframe as a table using kable()
kable(bike_df, format = "html", row.names = FALSE)



