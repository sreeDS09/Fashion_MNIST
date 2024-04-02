# Loading package
library(caTools)
library(randomForest)
library(readr)
library(keras)
library(caret)
#My Name is Sai Srikar
#In this assignment we will be working on Fashion Mnist dataset and evaluating via Random Forest and Gradient Boosting Techniques.


# Load the libraries, load the data sets, and setup Color Palette
#Since dataset is accessible on local systems we would let the end user load rather than load via localpath.
train_dt <- read.csv(file.choose(),header = FALSE)
test_dt <- read.csv(file.choose(), header=FALSE)

#print function lays out the output of the variables in ().
View(train_dt)
View(test_dt)

col_n <- c("F_Type", paste0("Pix", 1:784))

col_n

# Assign column names to the data
colnames(train_dt) <- col_n

# Assign column names to the data
colnames(test_dt) <- col_n


#Create target variable (diagnose) vectors
x_tar <- train_dt[,1]
y_tar <- test_dt[,1]



# Applying Random Forest algorithm for different set of trees and observe the results.
classifier_RF = randomForest(x_tar ~.,
                             data = train_dt[2:785] ,
                             ntree = 10)
classifier_RF
# Predicting the Test set results
y_pred = predict(classifier_RF, newdata = test[-5])


# Confusion Matrix
confusion_mtx = table(test[, 5], y_pred)
confusion_mtx
    

#Applying Gradient Booster technique to MNIST dataset

x_gtar <- train_dt[,1]
y_gtar <- test_dt[,1]

boost <- xgboost(data = x_gtar,nrounds = 1)
boost

# Predictions on Gradient boosting model
gb_pred <- predict(GBM,newdata = xgb_test)
pred.labels <- as.integer(gb_pred)

#Accuracy is calulated 
acc_cal <- sum(pred.labels == y_tar)/length(y_tar)
acc_cal
