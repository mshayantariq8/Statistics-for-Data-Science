# Load necessary libraries
library(titanic)

# Load the Titanic dataset
data("titanic_train")

# Extract the 'Age' column and remove NA values
ages <- na.omit(titanic_train$Age)

# Function to compute the trimmed mean
compute_trimmed_mean <- function(data, trim_level) {
  mean(data, trim = trim_level)
}

# Function to compute the standard error of the trimmed mean
compute_trimmed_mean_se <- function(data, trim_level) {
  n <- length(data)
  trimmed_mean <- mean(data, trim = trim_level)
  # Compute the number of trimmed values
  g <- floor(trim_level * n)
  # Winsorize the data
  sorted_data <- sort(data)
  winsorized_data <- c(rep(sorted_data[g + 1], g), sorted_data[(g + 1):(n - g)], rep(sorted_data[n - g], g))
  # Compute the Winsorized variance
  sw <- sqrt(sum((winsorized_data - mean(winsorized_data))^2) / (n - 1))
  se <- sw / ((1 - 2 * trim_level) * sqrt(n))
  return(se)
}

# Levels of trimming
trim_levels <- c(0.1, 0.2)

# Data frame to store results
results <- data.frame(
  Trim_Level = trim_levels,
  Trimmed_Mean = NA,
  SE = NA
)

# Calculate trimmed means and standard errors
for (i in 1:length(trim_levels)) {
  trim_level <- trim_levels[i]
  results[i, "Trimmed_Mean"] <- compute_trimmed_mean(ages, trim_level)
  results[i, "SE"] <- compute_trimmed_mean_se(ages, trim_level)
}

# Print results
print(results)

# Plotting the trimmed means for different trimming levels
barplot(
  results$Trimmed_Mean, 
  names.arg = results$Trim_Level, 
  ylab = "Trimmed Mean", 
  xlab = "Trimming Level", 
  col = "lightblue", 
  main = "Trimmed Means for Different Trimming Levels", 
  ylim = c(0, max(results$Trimmed_Mean) + 5)
)

