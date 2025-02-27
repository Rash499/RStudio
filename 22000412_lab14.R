# Load necessary libraries
library(ggplot2)

# Load the Iris dataset
data(iris)

# 1. Dataset Exploration
# Display the structure of the dataset
str(iris)

# Summary statistics
summary(iris)

# Display the first few rows
head(iris)

# Identify the number of species present
species_count <- table(iris$Species)
print(species_count)

# Calculate mean, median, and standard deviation for numerical features
stats <- data.frame(
  Feature = colnames(iris[, 1:4]),
  Mean = sapply(iris[, 1:4], mean),
  Median = sapply(iris[, 1:4], median),
  Std_Dev = sapply(iris[, 1:4], sd)
)
print(stats)

# 2. Data Visualization

# Pie Chart for species distribution
pie(species_count, labels = names(species_count), main = "Species Distribution")

# Bar Chart for species count
ggplot(iris, aes(x = Species)) +
  geom_bar(fill = "steelblue") +
  ggtitle("Count of Each Species") +
  xlab("Species") +
  ylab("Count")

# Histogram for Sepal Length
ggplot(iris, aes(x = Sepal.Length)) +
  geom_histogram(binwidth = 0.5, fill = "lightblue", color = "black") +
  ggtitle("Histogram of Sepal Length") +
  xlab("Sepal Length") +
  ylab("Frequency")

# Histogram for Petal Length
ggplot(iris, aes(x = Petal.Length)) +
  geom_histogram(binwidth = 0.5, fill = "lightgreen", color = "black") +
  ggtitle("Histogram of Petal Length") +
  xlab("Petal Length") +
  ylab("Frequency")

# Scatterplot between Sepal Length and Petal Length
ggplot(iris, aes(x = Sepal.Length, y = Petal.Length, color = Species)) +
  geom_point() +
  ggtitle("Scatterplot of Sepal Length vs Petal Length") +
  xlab("Sepal Length") +
  ylab("Petal Length")

# 3. Hypothesis Testing
alpha <- 0.05

# Lower Tail Test: Sepal Length < 5.8 cm
sepal_length_test <- t.test(iris$Sepal.Length, mu = 5.8, alternative = "less")
print("Lower Tail Test: Sepal Length < 5.8 cm")
print(sepal_length_test)

# Upper Tail Test: Petal Length > 3.5 cm
petal_length_test <- t.test(iris$Petal.Length, mu = 3.5, alternative = "greater")
print("Upper Tail Test: Petal Length > 3.5 cm")
print(petal_length_test)

# Two-Tailed Test: Sepal Width ≠ 3.0 cm
sepal_width_test <- t.test(iris$Sepal.Width, mu = 3.0, alternative = "two.sided")
print("Two-Tailed Test: Sepal Width ≠ 3.0 cm")
print(sepal_width_test)

