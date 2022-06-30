#Location of dataset and reading from directory 
getwd()
setwd("C:\\Users\\oladi\\OneDrive\\Desktop\\Data analytics")
credit.card<- read.csv("approval.csv", header=T, na.strings=c(""), stringsAsFactors = T)
sapply(credit.card,function(x) sum(is.na(x)))
head(credit.card)
print(credit.card)
str(credit.card)

#Package installed 
install.packages("FSelector")
install.packages("rpart")
install.packages("caret", dependencies = TRUE)
install.packages("dplyr")
install.packages("rpart.plot")
install.packages("xlsx")
install.packages("data.tree")
install.packages("caTools")

#library installed 
library(caret)
library(FSelector)
library(dplyr)
library(caTools)
library(data.tree)
library(xlsx)
library(rpart)
library(rpart.plot)

#logistic Regression 
getwd()
credit.card2 <- read.csv("approval.csv", header=T, na.strings=c(""), stringsAsFactors = T)
sapply(credit.card2,function(x) sum(is.na(x)))

#statistical assumptions for this Data set 
#1. Ensure that the independent variable is choosen randomly 
#2. Ensure that this prediction is free from other variables like outliers that can easily affect the prediction
#3. Ensure that the outcome has two outcomes
#which is M and F
#4. To show linearity between both independent and dependent variable
#5. free of collinearity between both independent and dependent variable
#6 no missing variable in here 

#simple logistic regression model 
library(lessR)
Logit(CODE_GENDER ~ AMT_INCOME_TOTAL, data = credit.card2)

#cooks distance can be used to identify outliers that 
#that can easily influence the predictor variable

# 4. To show linearity between both independent and dependent variable
Logit(CODE_GENDER ~ AMT_INCOME_TOTAL + AMT_INCOME_TOTAL:log(AMT_INCOME_TOTAL)
      ,data = credit.card2, brief = TRUE)

#splitting of the data set 
set.seed(123)
sample = sample.split(credit.card3$CODE_GENDER, SplitRatio = .70)

train = subset(credit.card3,sample==TRUE)
test = subset(credit.card3,sample==FALSE)


#Multiple Logistic Model 
# The use of multiple logistic model can used to increase 
#the prediction accuracy
nrow(credit.card)
ncol(credit.card)
names(credit.card)
str(credit.card)
str(credit.card$JOB)

#Decision tree
getwd()
setwd("C:\\Users\\oladi\\OneDrive\\Desktop\\Data analytics")
credit.card3 <- read.csv("approval.csv", header=T, na.strings=c(""), stringsAsFactors = T)

sapply(credit.card3,function(x) sum(is.na(x)))
head(credit.card)
print(credit.card)
str(credit.card)

#This library creates the model 
install.packages("rpart.plot")

#This library draws the decision tree 
library(rpart)
library(rpart.plot)

#Running this code showed complicating figures,because of several independent variables that are not interconnected
#which made it difficult to interprete 
myresults <-rpart(CODE_GENDER ~., method = "class", data = credit.card3)### creating a decision tree

#Decided to choose one independent variable for my classification output instead
myresults <-rpart(CODE_GENDER ~ JOB,method = "class", data = credit.card3)

#splitting of the data set 
set.seed(123)
sample = sample.split(credit.card3$CODE_GENDER, SplitRatio = .70)

train = subset(credit.card3,sample==TRUE)
test = subset(credit.card3,sample==FALSE)

#Training the Decision Tree Classifier
myresults <-rpart(CODE_GENDER ~ JOB,data = train)

#Prediction
tree.CODE_GENDER.predicted <- predict(myresults, test,type="class")

#confusion matrix for model evaluation 
confusionMatrix(tree.CODE_GENDER.predicted, test$CODE_GENDER)

#Visualizing the decision tree 
rpart.plot(myresults, type = 1, fallen.leaves = F , cex = .9)#plot decision tree 

str(credit.card3)



#install packages that enable the use K-CROSS VALIDATION
#and use Linear Regression for this credit card. 
install.packages("ISLR")
install.packages("MASS")
library(ISLR)
library(MASS)

#Show the names on header 
names(credit.card)
head(credit.card)

#install the caret function that enables to run k fold validation 
install.packages("caret")
library(caret)

#Partition the data into two 
#random seed 
set.seed(1985)

#convert to an index seed 
index<-createDataPartition(credit.card$FLAG_WORK_PHONE,p=.8, list = FALSE, times = 1)

#create partition train_df and test_df
train_df <- credit.card[index,]
test_df <- credit.card[-index,]

#Re-label the Target Variable
str(train_df)
train_df$FLAG_WORK_PHONE[train_df$FLAG_WORK_PHONE==1] <-"YES"
train_df$FLAG_WORK_PHONE[train_df$FLAG_WORK_PHONE==0] <-"NO"

test_df$FLAG_WORK_PHONE[test_df$FLAG_WORK_PHONE==1] <-"YES"
test_df$FLAG_WORK_PHONE[test_df$FLAG_WORK_PHONE==0] <-"NO"

#convert outcome variable to a type factor 
train_df$FLAG_WORK_PHONE <- as.factor(train_df$FLAG_WORK_PHONE)
test_df$FLAG_WORK_PHONE  <- as.factor(test_df$FLAG_WORK_PHONE)

#specify the type of training method to be used
#and number of folds to specify 

ctrlspecs <- trainControl(method = "cv", number = 10,
                          savePredictions = "all",
                          classProbs = TRUE)

#set random seed 
set.seed(1985)

#specify the model to be used which is linear regression 
model1 <- train(FLAG_WORK_PHONE ~ NAME_EDUCATION_TYPE + NAME_FAMILY_STATUS,
                data=train_df,
                method="glm",family=binomial,
                trControl=ctrlspecs)
print(model1)


summary(model1)



#application of the model to test data to see how it performs
#in predictions 
predictions <- predict(model1, newdata = test_df)
print(predictions)


#create a confusion matrix 
confusionMatrix(data = predictions, test_df$FLAG_WORK_PHONE)

