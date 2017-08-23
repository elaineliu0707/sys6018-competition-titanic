#Name: Yingjie LIu
#Computing ID: YL9QR
#SYS6018 - HW1

library(readr)
library(tidyverse)
library(ggplot2)
library(dplyr)

train = read_csv("train.csv")
test = read_csv("test.csv")
gender = read_csv("gender_submission.csv")

#train$title = gsub('(.*, )|(\\..*)','',train$Name) #Run into error later in predicting regression. Will ask Prof. Error message:  invalid type (closure) for variable 'title'
train$famsize = train$SibSp+train$Parch

#train$title = factor(train$title)
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

# * REMOVE Cabin as it is related to Fare
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
(155+57)/445
# 47.6% of prediction that are correct on training subset.

# predict
#test$title = gsub('(.*, )|(\\..*)','',test$Name)
test$famsize = test$SibSp+test$Parch
#test$title = factor(test$title)
test$sex = factor(test$Sex)
probs2<-as.vector(predict(train.lg2,newdata=(test), type="response"))
mypreds <- rep(0,nrow(test))  # Initialize prediction vector
mypreds[probs2>0.5] <- 1 # p>0.5 -> 1
write.table(mypreds, file = "YL9QR-hw1-titanic-mypredictions.csv", row.names=F, col.names=c("Survived"), sep=",")

mypred= read_csv("YL9QR-hw1-titanic-mypredictions.csv")

#Compare
n=length(which(gender$Survived == mypred$Survived))
correct_rate = n/419
#correct_rate is 0.8782816
