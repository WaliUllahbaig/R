# Install necessary libraries
install.packages(c("tidyverse", "ggcorrplot"))

# Load the libraries
library(tidyverse)
library(ggcorrplot)

# Load the dataset
movie_data <- read.csv("movie.csv")

# Display the first few rows of the dataset
head(movie_data)

# Summary statistics
summary(movie_data)

# Bar chart of movies
ggplot(movie_data, aes(x = movie, fill = movie)) +
  geom_bar() +
  labs(title = "Distribution of Movies",
       x = "Movie",
       y = "Count")

# Boxplot of ages by movie
ggplot(movie_data, aes(x = movie, y = age, fill = movie)) +
  geom_boxplot() +
  labs(title = "Boxplot of Ages by Movie",
       x = "Movie",
       y = "Age")

# Scatter plot of age vs. ratings, color-coded by gender
ggplot(movie_data, aes(x = age, y = movie, color = is_male)) +
  geom_point() +
  labs(title = "Scatter Plot of Age vs. Ratings (Color-coded by Gender)",
       x = "Age",
       y = "Rating")

# Bar chart of the number of movies watched by gender
ggplot(movie_data, aes(x = is_male, fill = movie)) +
  geom_bar(position = "dodge") +
  labs(title = "Number of Movies Watched by Gender",
       x = "Gender",
       y = "Count")

# Histogram of ages, separated by gender
ggplot(movie_data, aes(x = age, fill = is_male)) +
  geom_histogram(binwidth = 1, position = "identity", alpha = 0.7) +
  labs(title = "Distribution of Ages by Gender",
       x = "Age",
       y = "Frequency")

# Boxplot of ratings, grouped by movies and gender
ggplot(movie_data, aes(x = movie, y = is_male, fill = is_male)) +
  geom_boxplot(position = "dodge") +
  labs(title = "Boxplot of Ratings by Movie and Gender",
       x = "Movie",
       y = "Rating")

# More visualizations can be added based on specific analysis goals.

