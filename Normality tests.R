# Data Loading and Exploration

library(readxl)
trade_data <- read_xlsx("C:/Users/stu/Desktop/Project/Import and export patterns/Clean african data.xlsx")
View(trade_data)

library(ggplot2)

# Density plot for import values
ggplot(trade_data, aes(x = `Import value (% of GDP)`)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Density Plot of Import Values",
       x = "Import Value (% of GDP)",
       y = "Density")

# Density plot for export values
ggplot(trade_data, aes(x = `Export value (% of GDP)`)) +
  geom_density(fill = "green", alpha = 0.5) +
  labs(title = "Density Plot of Export Values",
       x = "Export Value (% of GDP)",
       y = "Density")

shapiro_import <- shapiro.test(trade_data$`Import value (% of GDP)`)
shapiro_export <- shapiro.test(trade_data$`Export value (% of GDP)`)

print(shapiro_import)
print(shapiro_export)

friedman_import <- friedman.test(`Import value (% of GDP)` ~ Year | `Country Name`, data = trade_data)
friedman_export <- friedman.test(`Export value (% of GDP)` ~ Year | `Country Name`, data = trade_data)

print(friedman_import)
print(friedman_export)

# Perform Shapiro-Wilk normality test for import values
shapiro_import <- shapiro.test(trade_data$`Import value (% of GDP)`)

# Perform Shapiro-Wilk normality test for export values
shapiro_export <- shapiro.test(trade_data$`Export value (% of GDP)`)

# Print the results
print(shapiro_import)
print(shapiro_export)

# Perform Friedman test on import value
friedman_import <- friedman.test(`Import value (% of GDP)` ~ `Country Name` | `Year`, data = trade_data)
friedman_import

# Perform Friedman test on export value
friedman_export <- friedman.test(`Export value (% of GDP)` ~ `Country Name` | `Year`, data = trade_data)
friedman_export


# Pivot the data from long to wide format
trade_data_wide <- trade_data %>%
  pivot_wider(
    id_cols = c("Country Name", "Country Code", "Continent", "Region"),
    names_from = "Year",
    values_from = c("Import value (% of GDP)", "Export value (% of GDP)"),
    names_sep = "_"
  ) %>%
  filter(complete.cases(.))  # Remove rows with missing values

