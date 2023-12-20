# Load necessary libraries
install.packages("factoextra")
library(factoextra)
library(tidyverse)
library(reshape2)  # Add this line to load reshape2
library(ggplot2)

# Read the dataset
pulsar_data <- read.csv("pulsar_stars.csv")

# Display summary statistics
summary(pulsar_data)

# Check for missing values
missing_values <- colSums(is.na(pulsar_data))
print("Missing Values:")
print(missing_values)

# Distribution of target_class
ggplot(pulsar_data, aes(x = factor(target_class))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution of Target Class",
       x = "Target Class",
       y = "Frequency")

# Pairwise scatter plots for numerical variables
pairs(pulsar_data[, 1:8], col = pulsar_data$target_class + 1)

# Boxplot for each variable by target class
melted_data <- melt(pulsar_data, id.vars = "target_class")
ggplot(melted_data, aes(x = variable, y = value, fill = factor(target_class))) +
  geom_boxplot() +
  labs(title = "Boxplot of Variables by Target Class",
       x = "Variable",
       y = "Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Correlation heatmap
correlation_matrix <- cor(pulsar_data[, 1:8])
ggplot(data = as.data.frame(as.table(correlation_matrix)),
       aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Density plots for each variable by target class
ggplot(melted_data, aes(x = value, fill = factor(target_class))) +
  geom_density(alpha = 0.5) +
  facet_wrap(~variable, scales = "free_y") +
  labs(title = "Density Plots by Target Class",
       x = "Value",
       y = "Density") +
  theme_minimal()

# Violin plots for each variable by target class
ggplot(melted_data, aes(x = variable, y = value, fill = factor(target_class))) +
  geom_violin() +
  labs(title = "Violin Plots of Variables by Target Class",
       x = "Variable",
       y = "Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Principal Component Analysis (PCA)
pca_result <- prcomp(pulsar_data[, 1:8], scale. = TRUE)
summary(pca_result)

# Perform PCA with scaled data
scaled_data <- scale(pulsar_data[, 1:8])
pca_result <- prcomp(scaled_data)

# Visualize PCA with factoextra
library(factoextra)

# Scree plot
fviz_eig(pca_result, addlabels = TRUE)

# Biplot
fviz_pca_biplot(pca_result, col.ind = pulsar_data$target_class)


