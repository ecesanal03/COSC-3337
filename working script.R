#install.packages('httpgd', repos='http://cran.us.r-project.org')
library(ggplot2)
data <- read.csv("Baseball_Databank_Teams_1871_2023_Modded.csv")

print("Covariance Computations")
#covMatrix <- cov(data["R"], data["E"])
covMatrix <- cov(data[, c("R", "E", "HR", "RA", "SOA")])
print(covMatrix)

print("Correlation Computations")
corMatrix <- cor(data[, c("R", "E", "HR", "RA", "SOA")])
print(corMatrix)
#note: the diagonals for both computations is each column crossed with itself and should be ignored.

ggplot(data, aes(x = AB, y = H)) +
  geom_point() +
  labs(title = "At Bats vs Hits by Batters", x = "At Bats (AB)", y = "Hits by Batters (H)")

ggplot(data, aes(x = HA, y = BBA)) +
  geom_point() +
  labs(title = "Hits Allowed vs Walks Allowed", x = "Hits Allowed (HA)", y = "Walks Allowed (BBA)")