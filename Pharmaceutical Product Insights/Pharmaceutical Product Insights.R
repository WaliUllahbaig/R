# Load necessary libraries
library(ggplot2)
library(tidyverse)

# Read the dataset
medicine_data <- read.csv("Medicine_Details.csv")

# Display the structure of the dataset
str(medicine_data)

# Summary statistics
summary(medicine_data)

# Explore missing values
missing_values <- colSums(is.na(medicine_data))
print("Missing Values:")
print(missing_values)

# Data Cleaning (if necessary)
# For example, removing special characters from column names
colnames(medicine_data) <- make.names(colnames(medicine_data))

# Bar chart of Manufacturer
ggplot(medicine_data, aes(x = Manufacturer)) +
  geom_bar(fill = "green", color = "black") +
  labs(title = "Manufacturer Distribution",
       x = "Manufacturer",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Histogram of Excellent Review Percent
ggplot(medicine_data, aes(x = "Excellent Review %")) +
  geom_bar(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Excellent Review Percent",
       x = "Excellent Review Percent",
       y = "Frequency")

# Boxplot of Average Review Percent by Side Effects
ggplot(medicine_data, aes(x = Side_effects, y = 'Average Review Percent')) +
  geom_boxplot(fill = "orange", color = "black") +
  labs(title = "Boxplot of Average Review Percent by Side Effects",
       x = "Side Effects",
       y = "Average Review Percent") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Create a word cloud for side effects
library(wordcloud)
side_effects_text <- tolower(paste(medicine_data$Side_effects, collapse = " "))
wordcloud(words = names(table(strsplit(side_effects_text, "\\s+")[[1]])),
          freq = table(strsplit(side_effects_text, "\\s+")[[1]]),
          min.freq = 1, scale = c(3, 0.5), colors = brewer.pal(8, "Dark2"))

# Bar plot for Manufacturers
ggplot(medicine_data, aes(x = Manufacturer)) +
  geom_bar(fill = "skyblue", color = "black") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Distribution of Medicines by Manufacturer")


