# Shah Wali Ullah Baig

# Install necessary libraries if not already installed
install.packages(c("tidyverse", "ggcorrplot", "DataExplorer"))

# Load the libraries
library(tidyverse)
library(ggcorrplot)
library(DataExplorer)

# Read the CSV file
demographic_data <- read.csv("Demographic_Employment_Data.csv")

# Display the first few rows of the dataset
head(demographic_data)

# Summary statistics
summary(demographic_data)

# Overview of the dataset
plot_intro(demographic_data)

# Correlation heatmap
correlation_matrix <- cor(demographic_data %>% select_if(is.numeric))
ggcorrplot(correlation_matrix, title = "Correlation Heatmap", lab = TRUE)

# Univariate analysis
plot_histogram(demographic_data)

# Boxplot for numerical variables
plot_boxplot(demographic_data, by = "income")


# Pairwise scatter plot matrix
pairs(demographic_data[, sapply(demographic_data, is.numeric)])

# Bar plot for categorical variables
ggplot(demographic_data, aes(x = income)) +
  geom_bar() +
  ggtitle("Distribution of Income")

# Missing value plot
plot_missing(demographic_data)

# Interactive density plot
plot_density(demographic_data)



