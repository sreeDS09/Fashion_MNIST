install.packages("reticulate")
install.packages("tensorflow")
install.packages("glue")
install.packages('pillar')
library(pillar)
library(glue)
library(tidyverse)
library(tensorflow)
library(keras)
library(tidymodels)
library(caTools)
library(randomForest)
library(readr)
library(caret)
library(rsample)
# Install keras package for R
install_keras()

Sys.setenv(RETICULATE_PYTHON = "C:/ProgramData/Anaconda3/python.exe")



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
train_x_keras <- array(train_x, dim = dim(train_x))
test_x_keras <- array(test_x, dim = dim(test_x))


### We divide each pixel by 255 because of RGB maximum point is 255.
train_x_keras <- train_x_keras/255
test_x_keras <- test_x_keras/255


### One hot encoding
### target variable categorical without removing any class.
train_y_keras <- to_categorical(train_y)
test_y_keras <- to_categorical(test_y)

### Model Creation
mod <- keras_model_sequential()


### 3 layer neural network 
mod %>% 
  layer_dense(units = 16*2, activation = "relu", input_shape = ncol(train_x_keras), name = "hidden1") %>% 
  layer_dense(units = 16, activation = "relu", name = "hidden2") %>% 
  layer_dense(units = 10, activation = "softmax", name = "output")

mod %>% 
  compile(loss = "categorical_crossentropy", 
          optimizer = optimizer_adam(lr = 0.001), 
          metrics = c("accuracy"))

summary(mod)

mod %>% 
fit(train_x_keras, train_y_keras, epoch = 15, batch_size = 10)

pred <- model %>% predict(x= test_x_keras) %>% k_argmax()

ctg <- c("T-shirt", "Trouser", "Pullover", "Dress", "Coat", "Sandal", "Shirt", "Sneaker", "Bag", "Boot")



pred <- as.vector(pred)
# Convert pred to a factor
pred <- factor(pred, labels = ctg)


test_y <- factor (test_y, labels = ctg)



caret::confusionMatrix(as.factor(pred), as.factor(test_y))


confusion_mtx = table(test_y, pred)
confusion_mtx 