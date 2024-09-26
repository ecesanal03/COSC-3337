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

#######################################
# Step 1
# Ensure that target is a factor for classification (This is the important fix)
data$target <- factor(data$target, levels = c(0, 1), labels = c("NoDisease", "Disease"))

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

# Define the tuneGrid with both C and sigma
tune_grid <- expand.grid(C = 1, sigma = 0.1)  # Adjust the sigma value as necessary

# Train SVM models with Radial Basis Function (RBF) kernel
svm_model <- train(target ~ ., data = data, method = "svmRadial",
                   trControl = train_control,
                   tuneGrid = tune_grid, # Include both sigma and C
                   preProcess = c("center", "scale"))

# Get cross-validation results
predictions <- predict(svm_model, data)

# Calculate accuracy, precision, and recall
confusion <- confusionMatrix(predictions, data$target)
accuracy <- confusion$overall['Accuracy']
precision <- confusion$byClass['Pos Pred Value']
recall <- confusion$byClass['Sensitivity']

# Store the results in the dataframe
svm_results <- rbind(svm_results, data.frame(Kernel_Function = "Radial", Accuracy = accuracy, Precision = precision, Recall = recall))

# Print the final results
print(svm_results)