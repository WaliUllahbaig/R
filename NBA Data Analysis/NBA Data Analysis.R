# Install necessary libraries
install.packages(c("tidyverse", "ggcorrplot"))

# Load the libraries
library(tidyverse)
library(ggcorrplot)

# Load the dataset
nba_data <- read.csv("nba18-19.csv")

# Display the first few rows of the dataset
head(nba_data)

# Summary statistics
summary(nba_data)

# Pairwise scatter plot matrix
pairs(nba_data[, c("Age", "G", "GS", "MP", "PTS")], main = "Pairwise Scatter Plot Matrix")

# Correlation heatmap
correlation_matrix <- cor(nba_data[, c("Age", "G", "GS", "MP", "PTS")])
ggcorrplot(correlation_matrix, title = "Correlation Heatmap", lab = TRUE)

# Bar chart of positions
ggplot(nba_data, aes(x = Pos, fill = Pos)) +
  geom_bar() +
  labs(title = "Distribution of Positions",
       x = "Position",
       y = "Count")

# Boxplot of points by position
ggplot(nba_data, aes(x = Pos, y = PTS, fill = Pos)) +
  geom_boxplot() +
  labs(title = "Boxplot of Points by Position",
       x = "Position",
       y = "Points")

# Age distribution histogram
ggplot(nba_data, aes(x = Age)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Ages",
       x = "Age",
       y = "Frequency")

# Scatter plot of Minutes Played (MP) vs. Points (PTS)
ggplot(nba_data, aes(x = MP, y = PTS)) +
  geom_point() +
  labs(title = "Scatter Plot of Minutes Played vs. Points",
       x = "Minutes Played",
       y = "Points")

# More visualizations can be added based on specific analysis goals.
