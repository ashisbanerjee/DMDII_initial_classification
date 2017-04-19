# All library needed for the models###########################################################################
library(caret)
library(nnet)
library(NeuralNetTools)
library(e1071)
library(FactoMineR)
# Read in the data###########################################################################################
crash <- read.csv("DATA.csv")
# X_2 .. X_6, X_9, X_10 are categorical variables.
crash$X_2 <- as.factor(as.character(crash$X_2))
crash$X_3 <- as.factor(as.character(crash$X_3))
crash$X_4 <- as.factor(as.character(crash$X_4))
crash$X_5 <- as.factor(as.character(crash$X_5))
crash$X_6 <- as.factor(as.character(crash$X_6))
crash$X_9 <- as.factor(as.character(crash$X_9))
crash$X_10 <- as.factor(as.character(crash$X_10))
crash$Y <- as.factor(as.character(crash$Y))
# crash.2level is a 2 class classification dataset which constructed from the crash dataset
crash.2level <- crash
crash.2level$Y[crash.2level$Y == "C"] <- "B"
crash.2level$Y <- factor(crash.2level$Y)
# train is the training dataset and test is the testing dataset.
# To change to a 2 class classifier, please change the dataset from crash to crash.2level
training <- crash
#training <- crash.2level
test <- training[training$X_1 <= 1460,]
train <- training[training$X_1 > 1460,]
# Classification Tree model using caret package #############################################################
# traingControl is 10-fold cross validation
# classification metric is Accuracy
control <- trainControl(method = "cv", number = 10)
metric <- "Accuracy"
set.seed(7)
cart.start <- Sys.time()
# fit.cart is the model trained using train() function; the fomula is shown in the parameter space.
fit.cart <- train(Y~X_2+X_3+X_4+X_5+X_6+X_7+X_8+X_9+X_10+X_11, data=train, method="rpart", metric=metric, trControl=control)
cart.end <- Sys.time()
predictions <- predict(fit.cart, test)
confusionMatrix(predictions, test$Y)
cart.end - cart.start
# Artificial neural network using nnet package #############################################################
ann.start <- Sys.time()
# model.rev is trained using nnet() function; size is the number of hidden units
model.rev <- nnet(Y~X_2+X_3+X_4+X_5+X_6+X_7+X_8+X_9+X_10+X_11, lineout = F, size = 10, decay = 0.01, trace = F,
                  data = train)
ann.end <- Sys.time()
pre.ANN <- predict(model.rev, test, type = 'class')
ANNtable <- table(pre.ANN, test$Y)
ANNtable
print("Accuracy: ")
sum(diag(ANNtable))/sum(ANNtable)
ann.end - ann.start
# Garson's algorithm ########################################################################################
# Garson's algorithm is used to calculate the relative importance of the predictors
weight.rev <- data.frame(garson(model.rev,bar_plot=F))
garson(model.rev)
weight <- NULL
weight <- c(weight,sum(weight.rev$rel_imp[grep('X_2+',row.names(weight.rev))])/6)
weight <- c(weight,sum(weight.rev$rel_imp[grep('X_3+',row.names(weight.rev))])/2)
weight <- c(weight,sum(weight.rev$rel_imp[grep('X_4+',row.names(weight.rev))])/8)
weight <- c(weight,sum(weight.rev$rel_imp[grep('X_5+',row.names(weight.rev))])/11)
weight <- c(weight,sum(weight.rev$rel_imp[grep('X_6+',row.names(weight.rev))])/7)
weight <- c(weight,sum(weight.rev$rel_imp[grep('X_7+',row.names(weight.rev))]))
weight <- c(weight,sum(weight.rev$rel_imp[grep('X_8+',row.names(weight.rev))]))
weight <- c(weight,sum(weight.rev$rel_imp[grep('X_9+',row.names(weight.rev))])/15)
weight <- c(weight,sum(weight.rev$rel_imp[grep('X_10+',row.names(weight.rev))])/8)
weight <- c(weight,sum(weight.rev$rel_imp[grep('X_11+',row.names(weight.rev))]))
weight.names <- c("X_2",'X_3','X_4','X_5','X_6','X_7','X_8','X_9','X_10','X_11')
# weight contains the variable name and average relative importance
weight <- data.frame(weight,weight.names)
# Calculate the best number of hidden unit ###################################################################
acc <- NULL
for(n in 5:15){
  model.ori <- nnet(Y~X_2+X_3+X_4+X_5+X_6+X_7+X_8+X_9+X_10+X_11, lineout = F, size = n, decay = 0.01, trace = F,
                    data = train)
  pre.ANN <- predict(model.ori, test, type = 'class')
  ANNtable <- table(pre.ANN, test$Y)
  acc <- c(acc, sum(diag(ANNtable))/sum(ANNtable))
}
accdecay <- NULL
# Calculate the best value of decay #########################################################################
for(n in c(0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.10)){
  model.ori <- nnet(Y~X_2+X_3+X_4+X_5+X_6+X_7+X_8+X_9+X_10+X_11, lineout = F, size = 9, decay = n, trace = F,
                    data = train)
  pre.ANN <- predict(model.ori, test, type = 'class')
  ANNtable <- table(pre.ANN, test$Y)
  accdecay <- c(accdecay, sum(diag(ANNtable))/sum(ANNtable))
}
# Support Vector Machine using e1041 package
svm.startnew <- Sys.time()
modelnew.svm <-svm(Y~X_2+X_3+X_4+X_5+X_6+X_7+X_8+X_9+X_10+X_11,data = train)
svm.endnew <- Sys.time()
pre=predict(modelnew.svm, test, type='class')
SVMtable <- table(pre,test$Y)
SVMtable
print("Accuracy: ")
sum(diag(SVMtable))/sum(SVMtable)
# Multiple Factor Analysis using FactoMineR package ##########################################################
crashdata <- training
crashdata <- crashdata[1:2000,]
crashdata <- data.frame(crashdata$X_8,crashdata$X_7,crashdata$X_11, crashdata$X_9,crashdata$X_5,
                        crashdata$X_3,crashdata$X_2,crashdata$X_6,crashdata$X_4,crashdata$X_10)
res <- MFA(crashdata,group = c(2,1,3,4),type = c("c","c","n","n"),ncp = 4,name.group=c("Severity","Scale","Vehicle","Environment"))
