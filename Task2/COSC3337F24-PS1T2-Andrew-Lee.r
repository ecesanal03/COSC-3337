#install.packages('kernlab', repos='http://cran.us.r-project.org')
library(caret)
library(e1071)
library(rpart)
library(rpart.plot)
library(kernlab)

# Load the dataset
data <- read.csv("heart-disease2.csv")

# Explore the dataset to check for missing values
summary(data)

# Ensure that target is a factor for classification (This is the important fix)
data$target <- factor(data$target, levels = c(0, 1), labels = c("NoDisease", "Disease"))

#######################################
# Step 1
# Set up 5-fold cross-validation
train_control <- trainControl(method = "cv", number = 5)

# Initialize a dataframe to store results
dt_results <- data.frame(Max_Depth = integer(), Accuracy = double(), Precision = double(), Recall = double())

# Loop through max_depth values and train models
for (depth in c(3, 7, 11, 15)) {
    # Train the decision tree model with the current max depth
    dt_model <- train(target ~ ., data = data, method = "rpart", 
                    trControl = train_control,
                    tuneGrid = data.frame(cp = 0), # No pruning, just depth control
                    control = rpart.control(maxdepth = depth))
  
    # Use the trained model to make predictions on the dataset
    predictions <- predict(dt_model, newdata = data)

    # Convert predictions to factors and make sure they have the same levels as the target
    predictions <- factor(predictions, levels = levels(data$target))

    # Now run the confusion matrix
    confusion <- confusionMatrix(predictions, data$target)
    accuracy <- confusion$overall['Accuracy']
    precision <- confusion$byClass['Pos Pred Value']
    recall <- confusion$byClass['Sensitivity']
  
    # Store the results in the dataframe
    dt_results <- rbind(dt_results, data.frame(Max_Depth = depth, Accuracy = accuracy, Precision = precision, Recall = recall))
    # Plot the decision tree
    cat("\nDecision Tree for max depth:", depth, "\n")
    rpart.plot(dt_model$finalModel)  # Print the tree structure using rpart.plot
}

# Display Decision Tree results
print(dt_results)


#######################################
# Step 2: SVM Model
# Initialize a dataframe to store results
svm_results <- data.frame(Kernel_Function = character(), Accuracy = double(), Precision = double(), Recall = double())

# Define the kernel types including the custom sigmoid kernel
kernels <- c("linear", "polynomial", "sigmoid", "sigmoid_custom")

# Set up 5-fold cross-validation
folds <- createFolds(data$target, k = 5)

# Loop over the kernels and perform 5-fold cross-validation
for (kernel in kernels) {
  
  # Initialize variables to store cumulative metrics
  total_accuracy <- 0
  total_precision <- 0
  total_recall <- 0
  
  # Loop over each fold
  for (fold in folds) {
    
    # Split the data into training and testing sets
    train_data <- data[-fold, ]
    test_data <- data[fold, ]
    
    # Train the SVM model using the specified kernel
    if (kernel == "sigmoid_custom") {
      # Custom sigmoid kernel with a different s (coef0)
      svm_model <- svm(target ~ ., data = train_data, kernel = "sigmoid", scale = TRUE, coef0 = -0.2)  # Adjust coef0 as needed
    } else {
      # Train the SVM model for other kernels
      svm_model <- svm(target ~ ., data = train_data, kernel = kernel, scale = TRUE)
    }
    
    # Make predictions on the test set
    predictions <- predict(svm_model, test_data)
    
    # Compute the confusion matrix
    confusion <- confusionMatrix(predictions, test_data$target)
    
    # Extract metrics for this fold
    accuracy <- confusion$overall['Accuracy']
    precision <- confusion$byClass['Pos Pred Value']
    recall <- confusion$byClass['Sensitivity']
    
    # Sum up the metrics for averaging later
    total_accuracy <- total_accuracy + accuracy
    total_precision <- total_precision + precision
    total_recall <- total_recall + recall
  }
  
  # Average the metrics across the 5 folds
  avg_accuracy <- total_accuracy / 5
  avg_precision <- total_precision / 5
  avg_recall <- total_recall / 5
  
  # Append the results to the dataframe
  svm_results <- rbind(svm_results, data.frame(Kernel_Function = kernel, Accuracy = avg_accuracy, Precision = avg_precision, Recall = avg_recall))
}

# Print the final results
print(svm_results)