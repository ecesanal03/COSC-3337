library(readr)
library(ggplot2)
library(dplyr)
library(rpart)  # For decision tree model
library(rpart.plot)  # For plotting decision trees
library(caret)  # For splitting data and evaluating model accuracy
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
# Filter for the Houston Astros and two randomly selected teams
selected_teams <- data %>% 
  filter(name %in% c("Houston Astros", "Chicago Cubs", "New York Yankees")) # Example teams, you can choose any two

# Split the data into two periods: 2004-2013 and 2014-2023
period_1 <- selected_teams %>% filter(yearID >= 2004 & yearID <= 2013)
period_2 <- selected_teams %>% filter(yearID >= 2014 & yearID <= 2023)

# Create histograms/bar plots for Target classes in each period
# Plot for 2004-2013
plot_period_1 <- ggplot(period_1, aes(x = TARGET, fill = name)) +
  geom_bar(position = "dodge") +
  facet_wrap(~ name) +
  labs(title = "Target Classes (2004-2013)",
       x = "Target Class",
       y = "Count")

# Plot for 2014-2023
plot_period_2 <- ggplot(period_2, aes(x = TARGET, fill = name)) +
  geom_bar(position = "dodge") +
  facet_wrap(~ name) +
  labs(title = "Target Classes (2014-2023)",
       x = "Target Class",
       y = "Count")

# Display the plots
print(plot_period_1)
print(plot_period_2)

######################### Step 4 #########################################

# Select relevant columns for the box plots
selected_data <- data[, c("BB", "SB", "TARGET")]

# Remove rows with missing values
clean_data <- na.omit(selected_data)

# Box plot for BB (Walks by Batters) by Target Class
boxplot_BB <- ggplot(clean_data, aes(x = TARGET, y = BB, fill = TARGET)) +
  geom_boxplot() +
  labs(title = "Box Plot of Walks by Batters (BB) by Target Class",
       x = "Target Class",
       y = "Walks by Batters (BB)")

# Box plot for SB (Stolen Bases) by Target Class
boxplot_SB <- ggplot(clean_data, aes(x = TARGET, y = SB, fill = TARGET)) +
  geom_boxplot() +
  labs(title = "Box Plot of Stolen Bases (SB) by Target Class",
       x = "Target Class",
       y = "Stolen Bases (SB)")

# Box plot for all instances (ignoring Target Class) for BB
boxplot_BB_all <- ggplot(clean_data, aes(x = "All Instances", y = BB)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Box Plot of Walks by Batters (BB) for All Instances",
       x = "All Instances",
       y = "Walks by Batters (BB)")

# Box plot for all instances (ignoring Target Class) for SB
boxplot_SB_all <- ggplot(clean_data, aes(x = "All Instances", y = SB)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Box Plot of Stolen Bases (SB) for All Instances",
       x = "All Instances",
       y = "Stolen Bases (SB)")

# Display the plots
print(boxplot_BB)
print(boxplot_SB)
print(boxplot_BB_all)
print(boxplot_SB_all)

######################### Step 5 #########################################

# Select relevant columns for scatter plots based on the provided names
selected_data <- data[, c("HBP", "SO", "CG", "SHO", "IPouts", "DP", "TARGET")]

# Remove rows with missing values
clean_data <- na.omit(selected_data)

# Scatter plot for HBP vs SO, colored by Target class
plot_HBP_SO <- ggplot(clean_data, aes(x = HBP, y = SO, color = TARGET)) +
  geom_point() +
  labs(title = "Scatter Plot of Hit by Pitch (HBP) vs Strikeouts (SO)",
       x = "Hit by Pitch (HBP)",
       y = "Strikeouts (SO)") +
  scale_color_manual(values = c("red", "blue", "green"))

# Scatter plot for CG vs SHO, colored by Target class
plot_CG_SHO <- ggplot(clean_data, aes(x = CG, y = SHO, color = TARGET)) +
  geom_point() +
  labs(title = "Scatter Plot of Complete Games (CG) vs Shutouts (SHO)",
       x = "Complete Games (CG)",
       y = "Shutouts (SHO)") +
  scale_color_manual(values = c("red", "blue", "green"))

# Scatter plot for IPouts vs DP, colored by Target class
plot_IPouts_DP <- ggplot(clean_data, aes(x = IPouts, y = DP, color = TARGET)) +
  geom_point() +
  labs(title = "Scatter Plot of Outs Pitched (IPouts) vs Double Plays (DP)",
       x = "Outs Pitched (IPouts)",
       y = "Double Plays (DP)") +
  scale_color_manual(values = c("red", "blue", "green"))

# Display the plots
print(plot_HBP_SO)
print(plot_CG_SHO)
print(plot_IPouts_DP)

######################### Step 6 #########################################

# Select relevant columns for density plots
selected_data <- data[, c("WP", "E", "TARGET")]

# Remove rows with missing values
clean_data <- na.omit(selected_data)

# Density plot for Wins Percentage (WP) by Target class
density_WP <- ggplot(clean_data, aes(x = WP, fill = TARGET)) +
  geom_density(alpha = 0.6) +
  labs(title = "Density Plot of Wins Percentage (WP) by Target Class",
       x = "Wins Percentage (WP)",
       y = "Density") +
  scale_fill_manual(values = c("red", "blue", "green"))

# Density plot for Errors per game (E) by Target class
density_E <- ggplot(clean_data, aes(x = E, fill = TARGET)) +
  geom_density(alpha = 0.6) +
  labs(title = "Density Plot of Errors per Game (E) by Target Class",
       x = "Errors per Game (E)",
       y = "Density") +
  scale_fill_manual(values = c("red", "blue", "green"))

# Display the density plots
print(density_WP)
print(density_E)

######################### Step 7 #########################################

# Filter data for teams that won the World Series (WSWIN = 'Y')
world_series_winners <- data %>% filter(WSWin == "Y")

# Count how many times each class of the Target attribute each team obtained
target_count <- world_series_winners %>%
  group_by(name) %>%
  summarise(
    High = sum(TARGET == "HIGH"),
    Average = sum(TARGET == "AVERAGE"),
    Low = sum(TARGET == "LOW")
  )

# Create histograms for Wins (W) and Losses (L) attributes for each of the teams
# Histogram for Wins (W)
hist_W <- ggplot(world_series_winners, aes(x = W, fill = name)) +
  geom_histogram(binwidth = 5, alpha = 0.7, position = "dodge") +
  labs(title = "Histogram of Wins (W) for World Series Winners",
       x = "Wins",
       y = "Frequency") +
  scale_fill_discrete(name = "Teams")

# Histogram for Losses (L)
hist_L <- ggplot(world_series_winners, aes(x = L, fill = name)) +
  geom_histogram(binwidth = 5, alpha = 0.7, position = "dodge") +
  labs(title = "Histogram of Losses (L) for World Series Winners",
       x = "Losses",
       y = "Frequency") +
  scale_fill_discrete(name = "Teams")

# Display the table and histograms
print(target_count)
print(hist_W)
print(hist_L)

######################### Step 8 #########################################

# Select relevant columns for z-score transformation
selected_data <- data[, c("H", "SO", "SOA", "SHO", "FP", "WP")]

# Remove rows with missing values
clean_data <- na.omit(selected_data)

# Function to apply z-score transformation
z_score <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

# Apply z-score transformation to the selected columns
clean_data$H_z <- z_score(clean_data$H)
clean_data$SO_z <- z_score(clean_data$SO)
clean_data$SOA_z <- z_score(clean_data$SOA)
clean_data$SHO_z <- z_score(clean_data$SHO)
clean_data$FP_z <- z_score(clean_data$FP)

# Fit a linear model to predict WP (Win Percentage) using the z-scored variables
linear_model <- lm(WP ~ H_z + SO_z + SOA_z + SHO_z + FP_z, data = clean_data)

# Summary of the linear model
model_summary <- summary(linear_model)

# Extract R-squared and coefficients
r_squared <- model_summary$r.squared
coefficients <- model_summary$coefficients

# Print the R-squared and coefficients
print(paste("R-squared: ", r_squared))
print("Coefficients:")
print(coefficients)

######################### Step 9 #########################################

# Select only the relevant columns (Attributes 9 (R) to 34 (FP) and Target)
selected_data <- data[, c("R","AB","H","X2B","X3B","HR","BB","SO","SB","CS","HBP","SF","RA","ER","ERA","CG","SHO","SV","IPouts","HA","HRA","BBA","SOA","E","DP","FP","TARGET")]

# Remove rows with missing values
clean_data <- na.omit(selected_data)

# Split the data into training (70%) and testing (30%) sets
set.seed(123)  # For reproducibility
trainIndex <- createDataPartition(clean_data$TARGET, p = 0.7, list = FALSE)
train_data <- clean_data[trainIndex,]
test_data <- clean_data[-trainIndex,]

# Create decision tree models (restricting to 25 nodes or less)
control_params <- rpart.control(maxdepth = 5, minsplit = 20, cp = 0.01)

# Model 1: First decision tree
tree_model_1 <- rpart(TARGET ~ ., data = train_data, method = "class", control = control_params)
rpart.plot(tree_model_1, main = "Decision Tree Model 1")

# Model 2: Second decision tree with a different complexity parameter
control_params_2 <- rpart.control(maxdepth = 6, minsplit = 15, cp = 0.02)
tree_model_2 <- rpart(TARGET ~ ., data = train_data, method = "class", control = control_params_2)
rpart.plot(tree_model_2, main = "Decision Tree Model 2")

# Model 3: Third decision tree with adjusted parameters
control_params_3 <- rpart.control(maxdepth = 4, minsplit = 25, cp = 0.015)
tree_model_3 <- rpart(TARGET ~ ., data = train_data, method = "class", control = control_params_3)
rpart.plot(tree_model_3, main = "Decision Tree Model 3")

# Evaluate the models (training accuracy)
train_preds_1 <- predict(tree_model_1, train_data, type = "class")
train_acc_1 <- mean(train_preds_1 == train_data$TARGET)

train_preds_2 <- predict(tree_model_2, train_data, type = "class")
train_acc_2 <- mean(train_preds_2 == train_data$TARGET)

train_preds_3 <- predict(tree_model_3, train_data, type = "class")
train_acc_3 <- mean(train_preds_3 == train_data$TARGET)

# Evaluate the models (testing accuracy)
test_preds_1 <- predict(tree_model_1, test_data, type = "class")
test_acc_1 <- mean(test_preds_1 == test_data$TARGET)

test_preds_2 <- predict(tree_model_2, test_data, type = "class")
test_acc_2 <- mean(test_preds_2 == test_data$TARGET)

test_preds_3 <- predict(tree_model_3, test_data, type = "class")
test_acc_3 <- mean(test_preds_3 == test_data$TARGET)

# Print training and testing accuracy for each model
cat("Model 1: Training Accuracy =", train_acc_1, "Testing Accuracy =", test_acc_1, "\n")
cat("Model 2: Training Accuracy =", train_acc_2, "Testing Accuracy =", test_acc_2, "\n")
cat("Model 3: Training Accuracy =", train_acc_3, "Testing Accuracy =", test_acc_3, "\n")