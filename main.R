library(readr)
library(ggplot2)
data <- read.csv("Baseball_Databank_Teams_1871_2023_Modded.csv")

######################### Step 1 #########################################
# Select relevant columns
selected_data <- data[, c("R", "E", "HRA", "RA", "SOA")]
# Remove rows with missing values
clean_data <- na.omit(selected_data)
# Compute covariance matrix
cov_matrix <- cov(clean_data)
# Compute correlation matrix
corr_matrix <- cor(clean_data)
# Print the covariance and correlation matrices
print("Covariance Matrix:")
print(cov_matrix)
print("Correlation Matrix:")
print(corr_matrix)


######################### Step 2 #########################################

# Select relevant columns for scatter plots
# AB = At Bats, H = Hits by Batters, HA = Hits Allowed, BBA = Walks Allowed
selected_data <- data[, c("AB", "H", "HA", "BBA")]
# Remove rows with missing values
clean_data <- na.omit(selected_data)
# Create a new column for AB/H and HA/BBA
clean_data$AB_H <- clean_data$AB / clean_data$H
clean_data$HA_BBA <- clean_data$HA / clean_data$BBA
# Scatter plot for AB vs H
plot1 <- ggplot(clean_data, aes(x = AB, y = H)) +
  geom_point(color = "blue") +
  labs(title = "Scatter Plot of At Bats (AB) vs Hits (H)",
       x = "At Bats (AB)",
       y = "Hits by Batters (H)")
# Scatter plot for HA vs BBA
plot2 <- ggplot(clean_data, aes(x = HA, y = BBA)) +
  geom_point(color = "red") +
  labs(title = "Scatter Plot of Hits Allowed (HA) vs Walks Allowed (BBA)",
       x = "Hits Allowed (HA)",
       y = "Walks Allowed (BBA)")
# Display the scatter plots
print(plot1)
print(plot2)

######################### Step 3 #########################################

