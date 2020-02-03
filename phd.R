library(readr)
library(glmnet)
library(MASS)
library(readxl)
library(plotmo)
library(MLmetrics)
library(lavaan)

data <- read_csv("data_010820.csv")[,c(-1,-12,-18)]

#Split the data
set.seed(123)
train = data[sample(nrow(data),0.75*nrow(data)),]
test = data[sample(nrow(data),0.25*nrow(data)),]
train_y=train[,18]
train_x=train[,-18]
test_y=test[,18]
test_x=test[,-18]



