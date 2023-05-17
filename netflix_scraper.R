library(httr)
library(jsonlite)
library(ggplot2)
library(lubridate)
library(knitr)
library(markdown)

url <- "https://unogs-unogs-v1.p.rapidapi.com/search/titles"

queryString <- list(
  order_by = "date",
  type = "movie"
)

api_key = Sys.getenv("X-RapidAPI-Key")

response <- VERB("GET", url, query = queryString, add_headers('X-RapidAPI-Key' = api_key, 'X-RapidAPI-Host' = 'unogs-unogs-v1.p.rapidapi.com'), content_type("application/octet-stream"))

# Convert JSON response to list
response_list <- fromJSON(content(response, "text"))

# Convert JSON response to data frame
movies_df <- response_list[[2]]

movies_df <- movies_df[, c("title", "year", "rating", "synopsis")]

movies_df$rating <- ifelse(is.na(movies_df$rating), "N/A", movies_df$rating)
movies_df$rating[movies_df$rating == ""] <- "N/A"

# create a table with kable
movies_table <- kable(movies_df, format = "markdown", booktabs = TRUE, align = "c", col.names = toupper(colnames(movies_df)), space = "small")

# Display the Table
movies_table

# Write to CSV file
#write.csv(movies_df, file = "netflix_movies.csv", row.names = FALSE)