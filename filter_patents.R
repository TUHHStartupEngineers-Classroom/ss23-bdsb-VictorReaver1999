# Data Wrangling

# Import libraries
library(data.table)
library(knitr)

# Import data
assignee <- fread("patent_data_reduced\\assignee.tsv")
patent_assignee <- fread("patent_data_reduced\\patent_assignee.tsv")
patent <- fread("patent_data_reduced\\patent.tsv")
uspc <- fread("patent_data_reduced\\uspc.tsv")


# Question 1


# Merge assignee and patent_assignee tables
assignee_patent <- merge(assignee, patent_assignee, by.x = "id", by.y = "assignee_id")

# Filter for US corporations only
us_corp_patents <- assignee_patent[type == 2, .(num_patents = .N), by = organization]

# Sort in descending order
us_corp_patents <- us_corp_patents[order(-num_patents)]

# Display the top 10 companies with the most patents
us_corp_patents[1:10]


# Question 2


# Merge the patent_assignee and patent tables
patent_merged <- merge(patent, patent_assignee, by.x = "id", by.y = "patent_id")

# Extract the year and month from the date column
patent_merged[, `:=`(year = year(date), month = month(date))]

# Filter for August 2014
patent_aug_2014 <- patent_merged[year == 2014 & month == 8]

# Merge the assignee and patent_aug_2014 tables
assignee_patent_aug_2014 <- merge(assignee, patent_aug_2014, by.x = "id", by.y = "assignee_id")

# Filter for US corporations only
us_corp_patents_aug_2014 <- assignee_patent_aug_2014[type == 2]

# Count the number of patents for each organization
num_patents_aug_2014 <- us_corp_patents_aug_2014[, .(num_patents = .N), by = organization]

# Sort in descending order
top_10_patents_aug_2014 <- num_patents_aug_2014[order(-num_patents)][1:10]

# Display the top 10 companies with the most patents granted in August 2014
top_10_patents_aug_2014


# Question 3


# Count the number of patents for each assignee
assignee_count <- patent_assignee[, .N, by = assignee_id]

# Merge assignee and assignee_count to get the number of patents for each assignee
assignee_count <- merge(assignee, assignee_count, by.x = "id", by.y = "assignee_id")

# Filter for only the Corporations
assignee_count <- assignee_count[type %in% c(2, 3)]

# Join assignee_count and patent_assignee to get the number of patents for each US corporation/individual
patent_count <- merge(assignee_count, patent_assignee, by.x = "id", by.y = "assignee_id")

# Join patent_count and uspc to get the main class for each patent
patent_class <- merge(patent_count, uspc, by = "patent_id")

# Count the number of patents for each organization and sort by the number of patents
company_count <- patent_class[, .N, by = c("type", "organization")]
company_count <- company_count[type %in% c(2, 3)]
company_count <- company_count[order(-N)]

# Select the top 10 companies
top_companies <- company_count[1:10]

# Filter patent_class for the top 10 companies
patent_class_top <- patent_class[patent_class$organization %in% top_companies$organization]

# Count the number of patents for each main class and sort by the number of patents
class_count <- patent_class_top[, .N, by = mainclass_id]
class_count <- class_count[order(-N)]

# Select the top 5 main classes
top_classes <- class_count[1:5]

# Order the patent_class by the number of patents for each company
patent_class_ordered <- patent_class_top[order(-patent_class_top$N), ]

# Print the top 10 companies with the most patents and the top 5 main classes for these companies
print(top_companies)
cat("\n")
print(paste("The main classes are:", paste(top_classes$mainclass_id, collapse = ", ")))

