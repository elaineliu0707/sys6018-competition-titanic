#Computing ID: YL9QR
#SYS6018 - HW1

library(readr)
library(tidyverse)
library(ggplot2)
library(dplyr)

train = read_csv("train.csv")
test = read_csv("test.csv")
gender = read_csv("gender_submission.csv")

train$famsize = train$SibSp+train$Parch
train$sex = factor(train$Sex)
train$cabin = factor(train$Cabin)
train$embark = factor(train$Embarked)

# separate tickets with those contain letters and those contain only digits
train$tic = as.numeric(train$Ticket)

for (i in 1:891) {
  if (is.na(train$tic[i])){
   train$tictype[i]=1}
  else {
    train$tictype[i]=0}
  }

##################################################

# * REMOVE Cabin as it has too many missing values
train.lg1 <- glm(Survived ~ Pclass+sex+Age+Fare+embark+famsize+tictype, data=train, family = "binomial")
summary(train.lg1)
anova(train.lg1,test="Chisq")

# Pclass, sex,Age, famsize are significant in both general model and anova.REMOVE Fare, embark, and tictype. 

train.lg2 <- glm(Survived ~ Pclass+sex+Age+famsize, data=train, family = "binomial")
summary(train.lg2)
anova(train.lg2,test="Chisq")

sub <- sample(1:891,size=445)
training <- train[sub,]     # Select subset for cross-validation
validating <- train[-sub,]

probs <- as.vector(predict(train.lg2, type = "response"))
preds <- rep(0,445)  # Initialize prediction vector
preds[probs>0.5] <- 1 # p>0.5 -> 1
preds <- preds[1:445]
table(preds,training$Survived)
(158+64)/445
# 49.8% of prediction that are correct on training subset.

# predict
test$famsize = test$SibSp+test$Parch
test$sex = factor(test$Sex)
probs2<-as.vector(predict(train.lg2,newdata=(test), type="response"))
mypreds <- rep(0,nrow(test))  # Initialize prediction vector
mypreds[probs2>0.5] <- 1 # p>0.5 -> 1
pred_result = data.frame(test$PassengerId,mypreds)
write.table(pred_result, file = "YL9QR-titanic-mypredictions.csv", row.names=F, col.names=c("PassengerID","Survived"), sep=",")

mypred= read_csv("YL9QR-titanic-mypredictions.csv")
