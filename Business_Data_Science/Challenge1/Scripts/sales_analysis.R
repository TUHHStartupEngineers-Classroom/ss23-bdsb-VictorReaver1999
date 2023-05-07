# Data Science at TUHH ------------------------------------------------------
# SALES ANALYSIS ----

# 1.0 Load libraries ----
library(tidyverse)
library(readxl)
library(scales)
library(ggplot2)
library(lubridate)

# 2.0 Importing Files ----
# A good convention is to use the file name and suffix it with tbl for the data structure tibble
bikes_tbl <- read_excel(path = "ds_data/01_bike_sales/01_raw_data/bikes.xlsx")



orderlines_tbl <- read_excel("ds_data/01_bike_sales/01_raw_data/orderlines.xlsx")



bikeshops_tbl  <- read_excel("ds_data/01_bike_sales/01_raw_data/bikeshops.xlsx")



# 3.0 Examining Data ----
# print("bikes_tbl")
# bikes_tbl
# 
# 
# print("orderlines_tbl")
# orderlines_tbl
# 
# 
# print("bikeshops_tbl")
# bikeshops_tbl


# 4.0 Joining Data ----
left_join(orderlines_tbl, bikes_tbl, by = c("product.id" = "bike.id"))

bike_orderlines_joined_tbl <- orderlines_tbl %>%
  left_join(bikes_tbl, by = c("product.id" = "bike.id")) %>%
  left_join(bikeshops_tbl, by = c("customer.id" = "bikeshop.id"))

print("bike orderlines joined tbl")
glimpse(bike_orderlines_joined_tbl)


# 5.0 Wrangling Data ----
# print("bike orderlines joined category col, first 20 rows")
# head(bike_orderlines_joined_tbl$category, n=20)
# All actions are chained with the pipe already. You can perform each step separately and use glimpse() or View() to validate your code. Store the result in a variable at the end of the steps.
bike_orderlines_wrangled_tbl <- bike_orderlines_joined_tbl %>%
  # 5.1 Separate category name
  separate(col    = category,
           into   = c("category.1", "category.2", "category.3"),
           sep    = " - ") %>%

  # 5.2 Add the total price (price * quantity)
  # Add a column to a tibble that uses a formula-style calculation of other columns
  mutate(total.price = price * quantity) %>%

  # 5.3 Optional: Reorganize. Using select to grab or remove unnecessary columns
  # 5.3.1 by exact column name
  select(-...1, -gender) %>%

  # 5.3.2 by a pattern
  # You can use the select_helpers to define patterns.
  # Type ?ends_with and click on Select helpers in the documentation
  select(-ends_with(".id")) %>%

  # 5.3.3 Actually we need the column "order.id". Let's bind it back to the data
  bind_cols(bike_orderlines_joined_tbl %>% select(order.id)) %>%

  # 5.3.4 You can reorder the data by selecting the columns in your desired order.
  # You can use select_helpers like contains() or everything()
  select(order.id, contains("order"), contains("model"), contains("category"),
         price, quantity, total.price,
         everything()) %>%

  # 5.4 Rename columns because we actually wanted underscores instead of the dots
  # (one at the time vs. multiple at once)
  rename(bikeshop = name) %>%
  set_names(names(.) %>% str_replace_all("\\.", "_"))

# print("Bike OrderLines Wrangled Table")
# head(bike_orderlines_wrangled_tbl, n=10)


# 6.0 Business Insights ----
# 6.1 Sales by Year ----

library(lubridate)
# Step 1 - Manipulate
sales_by_year_tbl <- bike_orderlines_wrangled_tbl %>%

  # Select columns
  select(order_date, total_price) %>%

  # Add year column
  mutate(year = year(order_date)) %>%

  # Grouping by year and summarizing sales
  group_by(year) %>%
  summarize(sales = sum(total_price)) %>%

  # Optional: Add a column that turns the numbers into a currency format
  # (makes it in the plot optically more appealing)
  # mutate(sales_text = scales::dollar(sales)) <- Works for dollar values
  mutate(sales_text = scales::dollar(sales, big.mark = ".",
                                     decimal.mark = ",",
                                     prefix = "",
                                     suffix = " €"))

# print("Sales By Year Table")
# sales_by_year_tbl

# Step 2 - Visualize

sales_by_year_tbl %>%

  # Setup canvas with the columns year (x-axis) and sales (y-axis)
  ggplot(aes(x = year, y = sales)) +

  # Geometries
  geom_col(fill = "#2DC6D6") + # Use geom_col for a bar plot
  geom_label(aes(label = sales_text)) + # Adding labels to the bars
  geom_smooth(method = "lm", se = FALSE) + # Adding a trendline

  # Formatting
  # scale_y_continuous(labels = scales::dollar) + # Change the y-axis.
  # Again, we have to adjust it for euro values
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".",
                                                    decimal.mark = ",",
                                                    prefix = "",
                                                    suffix = " €")) +
  labs(
    title    = "Revenue by year",
    subtitle = "Upward Trend",
    x = "", # Override defaults for x and y
    y = "Revenue"
  )

# ggsave("sales_by_year_plot.png", width = 10, height = 8, dpi = 300)


# 6.2 Sales by Year and Category 2 ----

# Step 1 - Manipulate
sales_by_year_cat_1_tbl <- bike_orderlines_wrangled_tbl %>%

  # Select columns and add a year
  select(order_date, total_price, category_1) %>%
  mutate(year = year(order_date)) %>%

  # Group by and summarize year and main catgegory
  group_by(year, category_1) %>%
  summarise(sales = sum(total_price)) %>%
  ungroup() %>%

  # Format $ Text
  mutate(sales_text = scales::dollar(sales, big.mark = ".",
                                     decimal.mark = ",",
                                     prefix = "",
                                     suffix = " €"))

# print("Sales By Year Category1 Table")
# sales_by_year_cat_1_tbl

# Step 2 - Visualize
sales_by_year_cat_1_tbl %>%

  # Set up x, y, fill
  ggplot(aes(x = year, y = sales, fill = category_1)) +

  # Geometries
  geom_col() + # Run up to here to get a stacked bar plot

  # Facet
  facet_wrap(~ category_1) +

  # Formatting
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".",
                                                    decimal.mark = ",",
                                                    prefix = "",
                                                    suffix = " €")) +
  labs(
    title = "Revenue by year and main category",
    subtitle = "Each product category has an upward trend",
    fill = "Main category" # Changes the legend name
  )
# ggsave("revenue_by_year_and_category.png", width = 10, height = 8, dpi = 300)


# 7.0 Writing Files ----

# 7.1 Excel ----

# install.packages("writexl", repos='http://cran.us.r-project.org')
# library("writexl")
# bike_orderlines_wrangled_tbl %>%
#   write_xlsx("ds_data/01_bike_sales/02_wrangled_data/bike_orderlines.xlsx")
# 
# # # 7.2 CSV ----
# bike_orderlines_wrangled_tbl %>%
#   write_csv("ds_data/01_bike_sales/02_wrangled_data/bike_orderlines.csv")
# 
# # # 7.3 RDS ----
# bike_orderlines_wrangled_tbl %>%
#   write_rds("ds_data/01_bike_sales/02_wrangled_data/bike_orderlines.rds")


# 8.0 Challenge #1 

# Challenge Part 1
bike_orderlines_wrangled2_tbl <- bike_orderlines_joined_tbl %>%
  separate(location, into = c("city", "state"), sep = ", ") %>%
  group_by(state) %>%
  summarize(total_revenue = sum(price * quantity)) %>%
  arrange(desc(total_revenue)) %>%
  ggplot(aes(x = state, y = total_revenue)) +
  geom_bar(stat = "identity", fill = "#0072B2") +
  xlab("State") +
  ylab("Total Revenue") +
  ggtitle("Total Revenue by State") +
  scale_y_continuous(labels = comma) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# print("bike_orderlines_wrangled2_tbl")
# head(bike_orderlines_wrangled2_tbl, n=10)

# ggsave("sales_by_state.png", width = 10, height = 8, dpi = 300)


# Challenge Part 2
# Group the data by location (state) and year, and calculate the total revenue
bike_orderlines_wrangled3_tbl <- bike_orderlines_joined_tbl %>%
  separate(location, into = c("city", "state"), sep = ", ") %>%
  mutate(year = year(order.date)) %>%
  mutate(revenue = quantity * price) %>%
  group_by(state, year) %>%
  summarise(total_revenue = sum(revenue))

# Plot the data using facet_wrap
ggplot(bike_orderlines_wrangled3_tbl, aes(x = year, y = total_revenue)) +
  geom_col() +
  labs(x = "Year", y = "Total Revenue") +
  facet_wrap(~ state, ncol = 4) +
  scale_y_continuous(labels = comma) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Sales by Location and Year")

# Save the plot as a PNG file
ggsave("sales_by_location_and_year.png", dpi = 300)
