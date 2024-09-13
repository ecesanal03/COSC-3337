#install.packages('dpylr', repos='http://cran.us.r-project.org')
library(ggplot2)
library(dplyr)
data <- read.csv("Baseball_Databank_Teams_1871_2023_Modded.csv")

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
