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

#training
#glm
mod_glm = glm(Y~.,data=train, family = "binomial", maxit = 50)
#lasso
mod_lasso = glmnet(data.matrix(train_x), train_y, family = "binomial", alpha = 1)
#Backward AIC
mod_aic = stepAIC(lm(Y~.,data = train), direction = 'backward')

#test
#glm
#lasso
#Backward AIC
sum(as.matrix(as.numeric(predict(mod_aic,test)>=0.5)-test_y)==0)/length(as.matrix(as.numeric(predict(mod_aic,test)>=0.5)-test_y))



