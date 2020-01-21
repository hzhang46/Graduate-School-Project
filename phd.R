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

#lmod             working
mod_l = lm(Y~., data = train)
#glm              working
mod_glm = glm(Y~.,data=train, family = "binomial", maxit = 50)
#lasso        not working
mod_lasso = glmnet(data.matrix(train_x),as.factor(train_y) , family = "binomial", alpha = 1)
#Backward AIC     working
mod_aic = stepAIC(lm(Y~.,data = train), direction = 'backward')

#test accuracy

#lmod             0.844
sum(as.matrix(as.numeric(predict(mod_l,test)>=0.5)-test_y)==0)/length(as.matrix(as.numeric(predict(mod_l,test)>=0.5)-test_y))
#glm              0.797
sum(as.matrix(as.numeric(predict(mod_glm,test)>=0.5)-test_y)==0)/length(as.matrix(as.numeric(predict(mod_glm,test)>=0.5)-test_y))
#lasso

#Backward AIC     0.848
sum(as.matrix(as.numeric(predict(mod_aic,test)>=0.5)-test_y)==0)/length(as.matrix(as.numeric(predict(mod_aic,test)>=0.5)-test_y))



