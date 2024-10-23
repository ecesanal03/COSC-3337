library(caret)
library(rpart)
library(rpart.plot)

data <- read.csv("heart-disease2.csv")

predictors <- data[, -ncol(data)] # All columns except the target
target <- data$target

#######################################
# Step 1a
train_control <- trainControl(method = "cv", number = 5)
max_depths <- c(3, 7, 11, 15)
models <- list()

for (depth in max_depths) {
  # Train model with specific max depth
  model <- train(target ~ ., data = data, method = "rpart", 
                 trControl = train_control, 
                 tuneGrid = data.frame(cp = 0), 
                 control = rpart.control(maxdepth = depth))
  models[[paste("depth", depth, sep = "_")]] <- model
}

for (depth in max_depths) {
  cat("\nDecision Tree for Depth:", depth, "\n")
  model <- models[[paste("depth", depth, sep = "_")]]
  
  # Print the decision tree structure
  print(model$finalModel)
  
  # Plot the decision tree
  rpart.plot(model$finalModel, main = paste("Decision Tree for Depth", depth))
}