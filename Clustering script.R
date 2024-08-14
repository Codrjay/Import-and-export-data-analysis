# Install the factoextra package
install.packages("factoextra")

# Load necessary libraries
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(cluster)
library(writexl)

# Load the data
trade_data <- read_xlsx("C:/Users/stu/Desktop/Project/Import and export patterns/Clean african data.xlsx")

# Calculate mean import and export values for each country
merged_data <- trade_data %>%
  group_by(`Country Code`, `Country Name`) %>%
  summarise(
    Mean_Import_Value = mean(`Import value (% of GDP)`, na.rm = TRUE),
    Mean_Export_Value = mean(`Export value (% of GDP)`, na.rm = TRUE)
  )

# Elbow Method for Import Values
fviz_nbclust(merged_data[, "Mean_Import_Value"], kmeans, method = "wss") +
  labs(title = "Elbow Method for Optimal k (Import Values)")

# Elbow Method for Export Values
fviz_nbclust(merged_data[, "Mean_Export_Value"], kmeans, method = "wss") +
  labs(title = "Elbow Method for Optimal k (Export Values)")

# Silhouette Method for Import Values
fviz_nbclust(merged_data[, "Mean_Import_Value"], kmeans, method = "silhouette") +
  labs(title = "Silhouette Method for Optimal k (Import Values)")

# Silhouette Method for Export Values
fviz_nbclust(merged_data[, "Mean_Export_Value"], kmeans, method = "silhouette") +
  labs(title = "Silhouette Method for Optimal k (Export Values)")


# Define the number of clusters (k) for K-means
k <- 3  

# Perform K-means clustering for mean import values
set.seed(123)  # Set a seed for reproducibility
kmeans_import <- kmeans(merged_data[, "Mean_Import_Value"], centers = k, nstart = 10)
import_cluster_labels <- kmeans_import$cluster

# Perform K-means clustering for mean export values
kmeans_export <- kmeans(merged_data[, "Mean_Export_Value"], centers = k, nstart = 10)
export_cluster_labels <- kmeans_export$cluster

# Assign clusters to countries for imports and exports
merged_data$import_cluster <- factor(import_cluster_labels)
merged_data$export_cluster <- factor(export_cluster_labels)

# Create cluster plots
ggplot(merged_data, aes(x = Mean_Import_Value, y = Mean_Export_Value, color = import_cluster)) +
  geom_point() +
  labs(title = "Cluster Plot of Import Values",
       x = "Import Value (% of GDP)",
       y = "Export Value (% of GDP)",
       color = "Import Cluster") +
  theme_minimal()

ggplot(merged_data, aes(x = Mean_Import_Value, y = Mean_Export_Value, color = export_cluster)) +
  geom_point() +
  labs(title = "Cluster Plot of Mean Export Values",
       x = "Import Value (% of GDP)",
       y = "Export Value (% of GDP)",
       color = "Export Cluster") +
  theme_minimal()

# Calculate max and min values for imports for each cluster
import_summary <- merged_data %>%
  group_by(import_cluster) %>%
  summarise(
    Import_Max = max(Mean_Import_Value, na.rm = TRUE),
    Import_Max_Country = `Country Name`[which.max(Mean_Import_Value)],
    Import_Min = min(Mean_Import_Value, na.rm = TRUE),
    Import_Min_Country = `Country Name`[which.min(Mean_Import_Value)],
    Num_Countries = n()
  )

# Calculate max and min values for exports for each cluster
export_summary <- merged_data %>%
  group_by(export_cluster) %>%
  summarise(
    Export_Max = max(Mean_Export_Value, na.rm = TRUE),
    Export_Max_Country = `Country Name`[which.max(Mean_Export_Value)],
    Export_Min = min(Mean_Export_Value, na.rm = TRUE),
    Export_Min_Country = `Country Name`[which.min(Mean_Export_Value)],
    Num_Countries = n()
  )

# Print the summaries
cat("Import Summary:\n")
print(import_summary)

cat("\nExport Summary:\n")
print(export_summary)

# View the summary tables (optional)
View(import_summary)
View(export_summary)

# View countries in each import cluster
countries_in_import_clusters <- merged_data %>%
  group_by(import_cluster) %>%
  summarise(Countries = paste(`Country Name`, collapse = ", ")) %>%
  arrange(import_cluster)

# View countries in each export cluster
countries_in_export_clusters <- merged_data %>%
  group_by(export_cluster) %>%
  summarise(Countries = paste(`Country Name`, collapse = ", ")) %>%
  arrange(export_cluster)

# Print the lists of countries in each cluster
cat("\nCountries in each import cluster:\n")
print(countries_in_import_clusters)

cat("\nCountries in each export cluster:\n")
print(countries_in_export_clusters)

# View the lists of countries in each cluster (optional)
View(countries_in_import_clusters)
View(countries_in_export_clusters)

# Save summaries to Excel files
write_xlsx(import_summary, "import_summary.xlsx")
write_xlsx(export_summary, "export_summary.xlsx")

write_xlsx(countries_in_import_clusters, "impo_summary.xlsx")
write_xlsx(countries_in_export_clusters, "expo_summary.xlsx")



