# Shah Wali Ullah Baig

# Install necessary libraries
install.packages(c("tidyverse", "ggcorrplot"))

# Load the libraries
library(tidyverse)
library(ggcorrplot)

data <- read.csv("house_prices.csv")

# Pairwise scatter plot matrix
pairs(data[, -5], main = "Pairwise Scatter Plot Matrix")

# Correlation heatmap
correlation_matrix <- cor(data)
ggcorrplot(correlation_matrix, title = "Correlation Heatmap", lab = TRUE)

# Boxplot of prices for each bedroom count
ggplot(data, aes(x = factor(bedrooms), y = price)) +
  geom_boxplot() +
  labs(title = "Boxplot of Prices by Bedroom Count",
       x = "Number of Bedrooms",
       y = "Price")

# Histogram of prices
ggplot(data, aes(x = price)) +
  geom_histogram(binwidth = 10000, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Prices",
       x = "Price",
       y = "Frequency")

# Floor-wise density plot of prices
ggplot(data, aes(x = price, fill = factor(floors))) +
  geom_density(alpha = 0.7) +
  labs(title = "Density Plot of Prices by Floors",
       x = "Price",
       y = "Density",
       fill = "Floors")


# Scatter plot of area vs. price
ggplot(data, aes(x = area, y = price)) +
  geom_point() +
  labs(title = "Scatter Plot of Area vs. Price",
       x = "Area (sq ft)",
       y = "Price")

# Bar chart of bedroom counts
ggplot(data, aes(x = factor(bedrooms))) +
  geom_bar() +
  labs(title = "Distribution of Bedrooms",
       x = "Number of Bedrooms",
       y = "Count")
