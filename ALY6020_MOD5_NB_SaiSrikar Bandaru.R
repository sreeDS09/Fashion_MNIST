#Import libraries
library(pillar)
library(glue)
library(tidyverse)
library(tidymodels)
library(caTools)
library(e1071)  # Required for the tune() function
library(readr)
library(caret)
library(rsample)






#Loading the train and test dataset using file.choose() function
train_dt <- read.csv(file.choose(),header = FALSE, stringsAsFactors = FALSE)
test_dt <- read.csv(file.choose(), header=FALSE, stringsAsFactors = FALSE)
head(train_dt)

str(train_dt)
str(test_dt)



set.seed(100)
#idx <- initial_split(train_dt, prop = 0.8, strata = "label")
mnist_train <- data.matrix(train_dt)
mnist_test <- data.matrix(test_dt)


#divide X and Y
train_x <- mnist_train[,-1]
test_x <- mnist_test[,-1]

train_y <- mnist_train[,1]
test_y <- mnist_test[,1]

### Convert into Arrays
train_x_ <- array(train_x, dim = dim(train_x))
test_x_ <- array(test_x, dim = dim(test_x))


# Train the Naive Bayes model
nb_model <- naive_bayes(train_x, train_y)

# Make predictions on test data
predictions <- predict(nb_model, test_x)

# Evaluate the model
accuracy <- sum(predictions == test_y) / length(test_y)

library(ipred)


## Hypertuning process
laplace_values <- c(0, 0.5, 1)  # Example values to try
best_accuracy <- 0
best_model <- NULL

for (value in laplace_values) {
nb_model <- naiveBayes(train_x, train_y, laplace = value)
predictions <- predict(nb_model, validation_x)
accuracy <- sum(predictions == validation_y) / length(validation_y)
  
#  if (accuracy > best_accuracy) {
#    best_accuracy <- accuracy
#    best_model <- nb_model
#  }
#}
#Evaluate best model on the test set
predictions <- predict(best_model, test_x)
accuracy <- sum(predictions == test_y) / length(test_y)
