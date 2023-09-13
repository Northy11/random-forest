
library(randomForest)

# Load the dataset
parkinsons = parkinsons

# Split the data using the holdout method
set.seed(123)
train_index <- sample(nrow(parkinsons),0.7 * nrow(parkinsons))
train_data <- parkinsons[train_index, ]
test_data <- parkinsons[-train_index, ]

# Build the random forest model using the training set
rf_model_holdout <- randomForest(as.factor(V18) ~ ., data = train_data)

# Evaluate the performance of the model using the testing set
predicted_holdout <- predict(rf_model_holdout, test_data)
table(predicted_holdout,test_data$V18)
# Calculate the accuracy of the model
accuracy_holdout <- sum(predicted_holdout == test_data$V18)/nrow(test_data)
accuracy_holdout

# Use cross-validation to evaluate the performance of the model
set.seed(123)
cv_model <- randomForest(as.factor(V18) ~ ., data = parkinsons, nfold = 5)
prediction_cv = predict(cv_model, parkinsons)
accuracy_cv = mean(prediction_cv == parkinsons$V18)
accuracy_cv
# Use bootstrapping to evaluate the performance of the model
set.seed(123)
boot_model <- randomForest(as.factor(V18) ~ ., data = parkinsons, ntree = 500, mtry = 3, bootstrap = TRUE)
pred_boot <- predict(boot_model,parkinsons)
accuracy_boot <- sum(pred_boot == parkinsons$V18)/nrow(parkinsons)
accuracy_boot
# Print the accuracies for each method
cat(paste0("Holdout accuracy: ", round(accuracy_holdout, 4), "\n"))
cat(paste0("Cross-validation accuracy: ", round(accuracy_cv, 4), "\n"))
cat(paste0("Bootstrapping accuracy: ", round(accuracy_boot, 4), "\n"))
