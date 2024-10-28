#holding shell

#######################################
# Step 1
print("Covariance Computations")
#covMatrix <- cov(data["R"], data["E"])
covMatrix <- cov(data[, c("R", "E", "HR", "RA", "SOA")])
print(covMatrix)

print("Correlation Computations")
corMatrix <- cor(data[, c("R", "E", "HR", "RA", "SOA")])
print(corMatrix)
#note: the diagonals for both computations is each column crossed with itself and should be ignored.

#######################################
# Step 2
ggplot(data, aes(x = AB, y = H)) +
  geom_point() +
  labs(title = "At Bats vs Hits by Batters", x = "At Bats (AB)", y = "Hits by Batters (H)")

ggplot(data, aes(x = HA, y = BBA)) +
  geom_point() +
  labs(title = "Hits Allowed vs Walks Allowed", x = "Hits Allowed (HA)", y = "Walks Allowed (BBA)")

#######################################
# Step 3
#Grabbing Astros 2004-2013
filtered_data <- data %>% filter(yearID >= 2004 & yearID <= 2013)
astros_data <- filtered_data %>% filter(name == "Houston Astros")
row_count <- nrow(astros_data)
print(astros_data)
ggplot(astros_data, aes(x=TARGET)) + geom_bar(fill="steelblue") + 
  labs(title="Histogram of Target Values for the Houston Astros 2004-2013", x="Target", y="Frequency")+
  annotate("text", x=Inf, y=Inf, label=paste("Rows included:", row_count), hjust=1.1, vjust=2, size=4, color="red", fontface="bold")

#Grabbing Boston Red Sox 2004-2013
filtered_data <- data %>% filter(yearID >= 2004 & yearID <= 2013)
redsox_data <- filtered_data %>% filter(name == "Boston Red Sox")
row_count <- nrow(redsox_data)
print(redsox_data)
ggplot(redsox_data, aes(x=TARGET)) + geom_bar(fill="steelblue") + 
  labs(title="Histogram of Target Values for the Boston Red Sox 2004-2013" , x="Target", y="Frequency")+
  annotate("text", x=Inf, y=Inf, label=paste("Rows included:", row_count), hjust=1.1, vjust=2, size=4, color="red", fontface="bold")

#Grabbing New York Mets 2004-2013
filtered_data <- data %>% filter(yearID >= 2004 & yearID <= 2013)
mets_data <- filtered_data %>% filter(name == "New York Mets")
row_count <- nrow(mets_data)
print(mets_data)
ggplot(mets_data, aes(x=TARGET)) + geom_bar(fill="steelblue") + 
  labs(title="Histogram of Target Values for the New York Mets 2004-2013", x="Target", y="Frequency")+
  annotate("text", x=Inf, y=Inf, label=paste("Rows included:", row_count), hjust=1.1, vjust=2, size=4, color="red", fontface="bold")

#Grabbing Astros 2014-2023
filtered_data <- data %>% filter(yearID >= 2014 & yearID <= 2023)
astros_data <- filtered_data %>% filter(name == "Houston Astros")
row_count <- nrow(astros_data)
print(astros_data)
ggplot(astros_data, aes(x=TARGET)) + geom_bar(fill="steelblue") + 
  labs(title="Histogram of Target Values for the Houston Astros 2014-2023", x="Target", y="Frequency")+
  annotate("text", x=Inf, y=Inf, label=paste("Rows included:", row_count), hjust=1.1, vjust=2, size=4, color="red", fontface="bold")

#Grabbing Boston Red Sox 2014-2023
filtered_data <- data %>% filter(yearID >= 2014 & yearID <= 2023)
redsox_data <- filtered_data %>% filter(name == "Boston Red Sox")
row_count <- nrow(redsox_data)
print(redsox_data)
ggplot(redsox_data, aes(x=TARGET)) + geom_bar(fill="steelblue") + 
  labs(title="Histogram of Target Values for the Boston Red Sox 2014-2023", x="Target", y="Frequency")+
  annotate("text", x=Inf, y=Inf, label=paste("Rows included:", row_count), hjust=1.1, vjust=2, size=4, color="red", fontface="bold")

#Grabbing New York Mets 2014-2023
filtered_data <- data %>% filter(yearID >= 2014 & yearID <= 2023)
mets_data <- filtered_data %>% filter(name == "New York Mets")
row_count <- nrow(mets_data)
print(mets_data)
ggplot(mets_data, aes(x=TARGET)) + geom_bar(fill="steelblue") + 
  labs(title="Histogram of Target Values for the New York Mets 2014-2023", x="Target", y="Frequency")+
  annotate("text", x=Inf, y=Inf, label=paste("Rows included:", row_count), hjust=1.1, vjust=2, size=4, color="red", fontface="bold")

#######################################
# Step 4
# Create a box plot for the Walks by Batters attribute based on Target values
ggplot(data, aes(x = TARGET, y = BB, fill = TARGET)) +
  geom_boxplot() +
  labs(title = "Box Plot of Walks by Batters by Target Values",
       x = "TARGET",
       y = "BB (Walks by Batters)") +
  theme_minimal()

# Create a box plot for the Stolen Bases attribute based on Target values
ggplot(data, aes(x = TARGET, y = SB, fill = TARGET)) +
  geom_boxplot() +
  labs(title = "Box Plot of Stolen Bases by Target Values",
       x = "TARGET",
       y = "SB (Stolen Bases)") +
  theme_minimal()

#######################################
# Step 5
# Scatter plot for HBP vs SO
ggplot(data, aes(x = HBP, y = SO, color = TARGET)) +
  geom_point() +
  labs(title = "Scatter Plot of HBP vs SO by Target",
       x = "HBP (Hit By Pitch)",
       y = "SO (Strikeouts)") +
  theme_minimal()
# Scatter plot for CG vs SHO
ggplot(data, aes(x = CG, y = SHO, color = TARGET)) +
  geom_point() +
  labs(title = "Scatter Plot of CG vs SHO by Target",
       x = "CG (Complete Games)",
       y = "SHO (Shutouts)") +
  theme_minimal()
# Scatter plot for IPOuts vs DP
ggplot(data, aes(x = IPouts, y = DP, color = TARGET)) +
  geom_point() +
  labs(title = "Scatter Plot of IPOuts vs DP by Target",
       x = "IPOuts (Innings Pitched Outs)",
       y = "DP (Double Plays)") +
  theme_minimal()

#######################################
# Step 6
# Density plot for WP (Wins Percentage) grouped by Target
ggplot(data, aes(x = WP, fill = TARGET)) +
  geom_density(alpha = 0.6) +
  labs(title = "Density Plot of Wins Percentage by Target",
       x = "Wins Percentage",
       y = "Density") +
  theme_minimal() +
  scale_fill_manual(values = c("red", "blue", "green"))

# Density plot for E (Errors per Game) grouped by Target
ggplot(data, aes(x = E, fill = TARGET)) +
  geom_density(alpha = 0.6) +
  labs(title = "Density Plot of Errors per Game by Target",
       x = "Errors per Game",
       y = "Density") +
  theme_minimal() +
  scale_fill_manual(values = c("red", "blue", "green"))

#######################################
# Step 7
# Filter teams that won the World Series
ws_winners <- data %>%
  filter(WSWin == "Y")
# Count the occurrences of each Target class for each team that won the World Series
target_summary <- ws_winners %>%
  group_by(name) %>%
  summarise(
    High_Count = sum(TARGET == "HIGH"),
    Low_Count = sum(TARGET == "LOW"),
    Average_Count = sum(TARGET == "AVERAGE")
  )
print(target_summary)
# Histogram of Wins (W) for each team that won the World Series
ggplot(ws_winners, aes(x = W)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "black") +
  labs(title = "Histogram of Wins (W) for World Series Winning Teams",
       x = "Wins",
       y = "Frequency") +
  facet_wrap(~ name) +   # Create separate histograms for each team
  theme_minimal()

# Histogram of Losses (L) for each team that won the World Series
ggplot(ws_winners, aes(x = L)) +
  geom_histogram(binwidth = 5, fill = "tomato", color = "black") +
  labs(title = "Histogram of Losses (L) for World Series Winning Teams",
       x = "Losses",
       y = "Frequency") +
  facet_wrap(~ name) +   # Create separate histograms for each team
  theme_minimal()

#######################################
# Step 8
# Create the new dataset by transforming H, SO, SOA, SHO, and FP into z-scores
z_processed_data <- data %>%
  mutate(
    H_z = scale(H),
    SO_z = scale(SO),
    SOA_z = scale(SOA),
    SHO_z = scale(SHO),
    FP_z = scale(FP)
  )

# Fit a linear model to predict WP using the z-scored attributes
lm_model <- lm(WP ~ H_z + SO_z + SOA_z + SHO_z + FP_z, data = z_processed_data)
# Report the R-squared value
r_squared <- summary(lm_model)$r.squared
cat("R-squared value:", r_squared, "\n")
# Report the coefficients of the model
coefficients <- summary(lm_model)$coefficients
cat("Coefficients of the regression model:\n")
print(coefficients)