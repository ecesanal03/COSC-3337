library(ggplot2)
# Note - I did the first half of this project not knowing ggplot2 existed, oops?
# Everything should be right, though

dataset <- read.csv(file.choose())
# Question 1 - Covariance & Correlation Matrix
runsScored <- dataset[["R"]]
errorsPerGame <- dataset[["E"]]
homeruns <- dataset[["HR"]]
opponentRuns <- dataset[["RA"]]
# Places all the data into a combined list
data <- data.frame(errorsPerGame, runsScored, homeruns, opponentRuns)
# Prints the correlation & matrices (note: does not save them anywhere)
cov(data)
cor(data)

atBats <- dataset[["AB"]]
hitsByBatter <- dataset[["H"]]
walksAllowed <- dataset[["BBA"]]

# Scatter plots (Question 2). Not saved, yeehaw
par(mfrow = c(1,2))
plot(atBats, hitsByBatter, main = "Times at Bat VS Hits")
plot(hitsByBatter, walksAllowed, main = "Hits by Batter VS Times Walked")

# Question 3 - Lots of Barplots (exciting, I didn't save them tho)
houston_astros_old <- dataset[dataset$name == "Houston Astros" & dataset$yearID >= 2004 & dataset$yearID <= 2013,]
houston_astros_new <- dataset[dataset$name == "Houston Astros" & dataset$yearID >= 2014 & dataset$yearID <= 2023,]
la_dodgers_old <- dataset[dataset$name == "Los Angeles Dodgers" & dataset$yearID >= 2004 & dataset$yearID <= 2013,]
la_dodgers_new <- dataset[dataset$name == "Los Angeles Dodgers" & dataset$yearID >= 2014 & dataset$yearID <= 2023,]
pitt_pirates_old <- dataset[dataset$name == "Pittsburgh Pirates" & dataset$yearID >= 2004 & dataset$yearID <= 2013,]
pitt_pirates_new <- dataset[dataset$name == "Pittsburgh Pirates" & dataset$yearID >= 2014 & dataset$yearID <= 2023,]
par(mfrow = c(1,2))
ha_old_target <- houston_astros_old[["TARGET"]]
ha_new_target <- houston_astros_new[["TARGET"]]
barplot(table(ha_old_target), main = "Houston Astros 2004-2013 Target Frequency", xlab = "Target", ylab = "Frequency", names.arg = c("LOW", "AVERAGE"), col = "lightblue")
barplot(table(ha_new_target), main = "Houston Astros 2014-2023 Target Frequency", xlab = "Target", ylab = "Frequency", names.arg = c("AVERAGE", "HIGH"), col = "pink")
par(mfrow = c(1,2))
ld_old_target <- la_dodgers_old[["TARGET"]]
ld_new_target <- la_dodgers_new[["TARGET"]]
barplot(table(ld_old_target), main = "LA Dodgers 2004-2013 Target Frequency", xlab = "Target", ylab = "Frequency", names.arg = c("AVERAGE"), col = "lightblue")
barplot(table(ld_new_target), main = "LA Dodgers 2014-2023 Target Frequency", xlab = "Target", ylab = "Frequency", names.arg = c("AVERAGE", "HIGH"), col = "pink")
par(mfrow = c(1,2))
pi_old_target <- pitt_pirates_old[["TARGET"]]
pi_new_target <- pitt_pirates_new[["TARGET"]]
barplot(table(pi_old_target), main = "Pittsburgh Pirates 2004-2013 Target Frequency", xlab = "Target", ylab = "Frequency", names.arg = c("LOW", "AVERAGE"), col = "lightblue")
barplot(table(pi_new_target), main = "Pittsburgh Pirates 2014-2023 Target Frequency", xlab = "Target", ylab = "Frequency", names.arg = c("LOW", "AVERAGE"), col = "pink")

# Question 4 - Exciting Box Plots
# BB vs Target
ggplot(data = dataset, aes(x = TARGET, y = BB)) + labs(title = "Walks by Batter per Target Value", x = "Target", y = "Walks by Batter") + geom_boxplot(outlier.colour="darkred", outlier.shape=16, outlier.size = 1.5) + theme_minimal()
# SB vs Target
ggplot(data = dataset, aes(x = TARGET, y = SB)) + labs(title = "Stolen Bases per Target Value", x = "Target", y = "Stolen Bases") + geom_boxplot(outlier.colour="darkred", outlier.shape=16, outlier.size = 1.5) + theme_minimal()
# All Instances - Walks (maybe?)
ggplot(data = dataset, aes(x = factor(0), y = BB)) + labs(title = "Walks by Batter", x = "all instances", y = "Walks") + geom_boxplot(outlier.colour="darkred", outlier.shape=16, outlier.size = 1.5) + theme_minimal()
# All Instances - Stolen Bases
ggplot(data = dataset, aes(x = factor(0), y = SB)) + labs(title = "Stolen Bases", x = "all instances", y = "Bases Stolen") + geom_boxplot(outlier.colour="darkred", outlier.shape=16, outlier.size = 1.5) + theme_minimal()

# Question 5 - Supervised Scatter Plots
par(mfrow = c(1,1)) # this looks bad if you try to fit anymore plots on the page
# HBP vs SO
plot(dataset$HBP,dataset$SO,pch = 16,col = factor(dataset$TARGET), xlab = "Hits by Pitch", ylab = "Strikeouts", main = "Hits by Pitch VS Strikeouts sorted by Target")
legend(x="topright",legend = c("HIGH", "LOW", "AVERAGE"), col = unique(factor(dataset$TARGET)), pch = c(16,16,16))
# CG vs SHO
plot(dataset$CG,dataset$SHO,pch = 16,col = factor(dataset$TARGET), xlab = "Complete Games", ylab = "Shutouts", main = "Completed Games VS Shutouts sorted by Target")
legend(x="topleft",legend = c("HIGH", "LOW", "AVERAGE"), col = unique(factor(dataset$TARGET)), pch = c(16,16,16))
# IPouts vs DP
plot(dataset$IPouts,dataset$DP,pch = 16,col = factor(dataset$TARGET), xlab = "Innings Pitched Out", ylab = "Double Plays", main = "Innings Pitched Out VS Double Plays sorted by Target")
legend(x="topleft",legend = c("HIGH", "LOW", "AVERAGE"), col = unique(factor(dataset$TARGET)), pch = c(16,16,16))

# Question 6 - Density Plots
ggplot(data=dataset, aes(x=W, group=TARGET, fill=TARGET)) + labs(title = "Density of Wins based on Target", x = "Wins", y = "Density") + geom_density(adjust=1.5, alpha = 0.5) + theme_minimal() + scale_fill_manual(values = c("lightblue", "pink", "mediumpurple1"))
# Now the errors
ggplot(data=dataset, aes(x=E, group=TARGET, fill=TARGET)) + labs(title = "Density of Errors based on Target", x = "Errors", y = "Density") + geom_density(adjust=1.5, alpha = 0.5) + theme_minimal() + scale_fill_manual(values = c("lightblue", "pink", "mediumpurple1"))

# QUESTION 7 - Table & Histograms
won_world_series <- dataset[dataset$WSWin == "Y", c(2, 8, 35)]
high_sum <- sum(won_world_series$TARGET == "HIGH", na.rm = TRUE)
avg_sum <- sum(won_world_series$TARGET == "AVERAGE", na.rm = TRUE)
low_sum <- sum(won_world_series$TARGET == "LOW", na.rm = TRUE)

# Create table for Target
data= matrix(c(high_sum, avg_sum, low_sum), ncol=3, byrow=TRUE)
colnames(data) = c('HIGH','AVERAGE','LOW')
rownames(data) <- c('NUMBER OF WINS')
target_table <- as.table(data)
target_table # post the target table
won_world_series # post the table of teams that have won

# TO DO - Histograms for 7

# QUESTION 8 - TO DO
H <- dataset[["H"]]
SO <- dataset[["SO"]]
SOA <- dataset[["SOA"]]
SHO <- dataset[["SHO"]]
FP <- dataset[["FP"]]
WP <- dataset[["WP"]]

# time to do everything into a ZScore
H_zscore <- scale(H, center=TRUE, scale=TRUE)
SO_zscore <- scale(SO, center=TRUE, scale=TRUE)
SOA_zscore <- scale(SOA, center=TRUE, scale=TRUE)
SHO_zscore <- scale(SHO, center=TRUE, scale=TRUE)
FP_zscore <- scale(FP, center=TRUE, scale=TRUE)

zscore_data_frame <- data.frame(H_zscore, SO_zscore, SOA_zscore, SHO_zscore, FP_zscore, WP)

# all the data summarized
model <- lm(WP ~ H_zscore + SO_zscore + SOA_zscore + SHO_zscore + FP_zscore, data = zscore_data_frame)
summary(model)
# r-squared value
summary(model)$r.squared
# coefficients
summary(model)$coefficients

# QUESTION 9 - TO DO
library(tree)
