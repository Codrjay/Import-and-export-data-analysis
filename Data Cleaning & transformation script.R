# Load necessary libraries
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
# Read the Excel file
trade_data <- readxl::read_excel("C:/Users/stu/Desktop/Project/import and export of good and services - Copy.xlsx")
# Filter African countries
african_countries <- c("Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi",
                       "Cabo Verde", "Cameroon", "Central African Republic", "Chad", "Comoros",
                       "Congo, Dem. Rep.", "Congo, Rep.", "Cote d'Ivoire", "Djibouti", "Egypt",
                       "Equatorial Guinea", "Eritrea", "Eswatini", "Ethiopia", "Gabon", "Gambia",
                       "Ghana", "Guinea", "Guinea-Bissau", "Kenya", "Lesotho", "Liberia", "Libya",
                       "Madagascar", "Malawi", "Mali", "Mauritania", "Mauritius", "Morocco",
                       "Mozambique", "Namibia", "Niger", "Nigeria", "Rwanda", "Sao Tome and Principe",
                       "Senegal", "Seychelles", "Sierra Leone", "Somalia", "South Africa",
                       "South Sudan", "Sudan", "Tanzania", "Togo", "Tunisia", "Uganda", "Zambia",
                       "Zimbabwe")
african_trade_data <- trade_data %>%
  filter(`Country Name` %in% african_countries)
# Reshape the data into a tidy format using gather or pivot_longer function
tidy_african_trade_data <- african_trade_data %>%
  pivot_longer(cols = starts_with("20"),
               names_to = "Year",
               values_to = "Value")
# Now 'tidy_african_trade_data' contains the dataset in a tidy format
View(tidy_african_trade_data)
# Check for duplicates
duplicates <- tidy_african_trade_data[duplicated(tidy_african_trade_data), ]
# Print the duplicates (if any)
if (nrow(duplicates) > 0) {
  print(duplicates)
} else {
  cat("No duplicates found in the dataset.\n")
}
library(dplyr)

# Convert import and export columns to numeric format
tidy_african_trade_data <- tidy_african_trade_data %>%
  mutate(Value = as.numeric(Value))

# Check for any non-numeric values after conversion
non_numeric_values <- tidy_african_trade_data %>%
  filter(!is.na(Value) & !is.numeric(Value))

if (nrow(non_numeric_values) > 0) {
  print(non_numeric_values)
} else {
  cat("All values in import and export columns are now numeric.\n")
}
library(dplyr)



# Count countries with missing values in import or export columns
na_countries_count <- tidy_african_trade_data %>%
  filter(is.na(Value) & (`Series Name` %in% c("Exports of goods and services (% of GDP)", "Imports of goods and services (% of GDP)"))) %>%
  group_by(`Country Name`, `Series Name`) %>%
  summarize(Count = n())

# Print the count of countries with missing values and whether it's in import or export
print(na_countries_count, n=38)


# Count the number of missing values (NA) for each country and series
missing_values_count <- tidy_african_trade_data %>%
  group_by(`Country Name`, `Series Name`) %>%
  summarize(Count_NA = sum(is.na(Value))) %>%
  ungroup()

# Filter countries with more than 10 missing values in either series
countries_to_keep <- missing_values_count %>%
  group_by(`Country Name`) %>%
  filter(all(Count_NA <= 10)) %>%
  pull(`Country Name`)

# Filter the tidy dataset to keep only countries with less than or equal to 10 missing values
tidy_african_trade_data_filtered <- tidy_african_trade_data %>%
  filter(`Country Name` %in% countries_to_keep)
View(tidy_african_trade_data_filtered)
# Assuming your dataset is named 'tidy_african_trade_data_filtered'

# Extract numeric part of the Year column
tidy_african_trade_data_filtered$Year <- as.numeric(sub("\\[.*\\]", "", tidy_african_trade_data_filtered$Year))

# Check the structure of the dataset
str(tidy_african_trade_data_filtered)


# Convert Year column from character to numeric
tidy_african_trade_data_filtered$Year <- as.numeric(tidy_african_trade_data_filtered$Year)

# Check the structure of the dataset
str(tidy_african_trade_data_filtered)


# Define a function for LOCF imputation
lofc_impute <- function(x) {
  na_idx <- which(is.na(x))
  if (length(na_idx) > 0) {
    if (na_idx[1] == 1) {
      for (i in 1:length(na_idx)) {
        if (!is.na(x[na_idx[i] + 1])) {
          x[na_idx[i]] <- x[na_idx[i] + 1]
        }
      }
    } else {
      for (i in 1:length(na_idx)) {
        x[na_idx[i]] <- x[na_idx[i] - 1]
      }
    }
  }
  return(x)
}


# Group by 'Country Name' and 'Series Name' and apply LOCF imputation
filled_data <- tidy_african_trade_data_filtered %>%
  group_by(`Country Name`, `Series Name`) %>%
  mutate(Value = lofc_impute(Value))

# View the filled data
head(filled_data)
View(filled_data)

# Assuming contains the imputed data
tidy_african_trade_data_filtered <- filled_data

View(tidy_african_trade_data_filtered)


# And tidy_african_trade_data_filtered contains the current dataset

# Get the list of countries from the original dataset
original_countries <- unique(tidy_african_trade_data$`Country Name`)

# Get the list of countries from the filtered dataset
filtered_countries <- unique(tidy_african_trade_data_filtered$`Country Name`)

# Find the countries that were removed
removed_countries <- setdiff(original_countries, filtered_countries)

# Count the number of removed countries
num_removed_countries <- length(removed_countries)

# Display the number of removed countries
cat("Number of countries removed:", num_removed_countries, "\n")

# Display the names of removed countries
cat("Names of removed countries:", paste(removed_countries, collapse = ", "))



# Count the number of distinct countries in the dataset
num_countries <- tidy_african_trade_data_filtered %>%
  distinct(`Country Name`) %>%
  nrow()

# Display the number of countries
cat("Number of countries in the current dataset:", num_countries, "\n")

african_trade_data <- tidy_african_trade_data_filtered

# Define a function to classify countries into regions
classify_region <- function(country_name) {
  north_africa <- c("Algeria", "Egypt", "Libya", "Morocco", "Sudan", "Tunisia", "Western Sahara")
  west_africa <- c("Benin", "Burkina Faso", "Cabo Verde", "Cote d'Ivoire", "Gambia", "Ghana", "Guinea", 
                   "Guinea-Bissau", "Liberia", "Mali", "Mauritania", "Niger", "Nigeria", "Senegal", 
                   "Sierra Leone", "Togo")
  central_africa <- c("Angola", "Cameroon", "Central African Republic", "Chad","Congo, Dem. Rep.",
                      "Congo, Rep.", "Equatorial Guinea", "Gabon", "São Tomé & Príncipe")
  east_africa <- c("Burundi", "Comoros", "Djibouti", "Eritrea", "Ethiopia", "Kenya", "Madagascar", 
                   "Malawi", "Mauritius", "Mayotte", "Mozambique", "Réunion", "Rwanda", "Seychelles", 
                   "Somalia", "South Sudan", "Tanzania", "Uganda", "Zambia", "Zimbabwe")
  southern_africa <- c("Botswana", "Eswatini", "Lesotho", "Namibia", "South Africa")
  
  if (country_name %in% north_africa) {
    return("North Africa")
  } else if (country_name %in% west_africa) {
    return("West Africa")
  } else if (country_name %in% central_africa) {
    return("Central Africa")
  } else if (country_name %in% east_africa) {
    return("East Africa")
  } else if (country_name %in% southern_africa) {
    return("Southern Africa")
  } else {
    return("Other")
  }
}

# Apply the function to classify countries in the dataframe
african_trade_data <- african_trade_data %>%
  mutate(Region = sapply(`Country Name`, classify_region))

View(african_trade_data)

# Filter the data to include only countries with NA values in import or export data
countries_with_missing_data <- african_trade_data %>%
  group_by(`Country Name`) %>%
  filter(any(is.na(Value)))

# Count the distinct countries
num_countries_with_missing_data <- n_distinct(countries_with_missing_data$`Country Name`)

print(num_countries_with_missing_data)



# Fill missing values with mean
african_trade_data <- african_trade_data %>%
  group_by(`Country Name`) %>%
  mutate(Value = ifelse(is.na(Value), mean(Value, na.rm = TRUE), Value))

View(african_trade_data)


# Calculate total exports and imports for each region
region_summary <- african_trade_data %>%
  group_by(Region, `Series Name`) %>%
  summarise(Total_Value = sum(Value, na.rm = TRUE)) %>%
  pivot_wider(names_from = `Series Name`, values_from = Total_Value, values_fill = 0) %>%
  mutate(Total_Trade = `Exports of goods and services (% of GDP)` + `Imports of goods and services (% of GDP)`)

# Calculate trade balance ratio
region_summary <- region_summary %>%
  mutate(
    Trade_Balance_Ratio = (`Exports of goods and services (% of GDP)` - `Imports of goods and services (% of GDP)`) / Total_Trade,
    Higher_Total = if_else(`Exports of goods and services (% of GDP)` > `Imports of goods and services (% of GDP)`, "Exports", "Imports")
  )

# Print the summary
print(region_summary)


View(region_summary)

# Group the data by region and series name (export or import) and calculate total values
region_trade_summary <- african_trade_data %>%
  group_by(Region, `Country Name`, `Series Name`, `Series Code`) %>%
  summarise(Total_Value = sum(Value, na.rm = TRUE)) %>%
  filter(`Series Code` %in% c("NE.EXP.GNFS.ZS", "NE.IMP.GNFS.ZS")) # Filter only export and import series

# Identify top trading partners for exports and imports within each region
top_export_partners <- region_trade_summary %>%
  filter(`Series Code` == "NE.EXP.GNFS.ZS") %>%
  group_by(Region) %>%
  top_n(1, wt = Total_Value) %>%
  arrange(Region, desc(Total_Value))

top_import_partners <- region_trade_summary %>%
  filter(`Series Code` == "NE.IMP.GNFS.ZS") %>%
  group_by(Region) %>%
  top_n(1, wt = Total_Value) %>%
  arrange(Region, desc(Total_Value))

# Combine the top export and import partners data
top_trade_partners <- bind_rows(top_export_partners, top_import_partners)

# Print the top trading partners for each region
print(top_trade_partners)

View(top_trade_partners)

# 1. Filter the data for exports and imports separately
exports_data <- african_trade_data %>%
  filter(`Series Name` == "Exports of goods and services (% of GDP)")
imports_data <- african_trade_data %>%
  filter(`Series Name` == "Imports of goods and services (% of GDP)")

# 2. Group the data by region and year for exports and imports
exports_summary <- exports_data %>%
  group_by(Region, Year) %>%
  summarise(Total_Exports = sum(Value, na.rm = TRUE))

imports_summary <- imports_data %>%
  group_by(Region, Year) %>%
  summarise(Total_Imports = sum(Value, na.rm = TRUE))

# 3. Merge the exports and imports summaries
region_trade_summary <- left_join(exports_summary, imports_summary, by = c("Region", "Year"))

# 4. Calculate trade balances
region_trade_summary <- region_trade_summary %>%
  mutate(Trade_Balance = Total_Exports - Total_Imports)

region_trade_summary$Year <- as.numeric(region_trade_summary$Year)


african_trade_data_wide <- african_trade_data %>%
  pivot_wider(names_from = `Series Name`, values_from = Value)

# Print the first few rows of the wide-format dataframe
head(african_trade_data_wide)

View(african_trade_data_wide)

# Filter for import and export data separately
imports <- african_trade_data %>%
  filter(`Series Name` == "Imports of goods and services (% of GDP)") %>%
  select(`Country Name`, `Country Code`, Continent, `Series Code`, Year, Value)  # Select desired columns

exports <- african_trade_data %>%
  filter(`Series Name` == "Exports of goods and services (% of GDP)") %>%
  select(`Country Name`, `Country Code`, Continent, `Series Code`, Year, Value)  # Select desired columns

# Merge import and export data based on country and year
merged_data <- merge(imports, exports, by = c("Country Name", "Country Code", "Continent", "Year"), suffixes = c("_Import", "_Export"))

# Reorder the columns to have import and export columns next to each other
merged_data <- merged_data %>%
  select(`Country Name`, `Country Code`, Continent, `Series Code`, Year, Value_Import, Value_Export)

# Print the first few rows of the merged dataframe
head(merged_data)


# Filter for import and export data separately
imports <- african_trade_data %>%
  filter(`Series Name` == "Imports of goods and services (% of GDP)") %>%
  select(`Country Name`, `Country Code`, `Continent`, `Year`, `Value`, `Region` )  # Include Region column

exports <- african_trade_data %>%
  filter(`Series Name` == "Exports of goods and services (% of GDP)") %>%
  select(`Country Name`, `Country Code`, `Continent`, `Year`, `Value`, `Region` )  # Include Region column

# Merge import and export data based on region, country, and year
merged_data <- merge(imports, exports, by = c("Country Name", "Country Code", "Continent", "Year", "Region" ), suffixes = c("_Import", "_Export"))

# Reorder the columns to have import and export columns next to each other
merged_data <- merged_data %>%
  select(`Country Name`, `Country Code`, `Continent`, `Year`, `Value_Import`, `Value_Export`,`Region`)

# Print the first few rows of the merged dataframe
head(merged_data)

View(merged_data)

library(writexl)

# Write the data frame to an Excel file
write_xlsx(merged_data, "Clean african data.xlsx")


