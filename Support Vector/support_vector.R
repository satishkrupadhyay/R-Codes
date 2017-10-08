# R code for demostrating support vector machines in R.
# Data source: LendingClub.com

#Install Packages
install.packages('e1071')
install.packages("caTools")
#Include Packages to current session
library(e1071)
library(ggplot2)
library(caTools)

#Load Data
loans <- read.csv('loan_data.csv')

#Convert to categorical
loans$credit.policy <- factor(loans$credit.policy)
loans$inq.last.6mths <- factor(loans$inq.last.6mths)
loans$delinq.2yrs <- factor(loans$delinq.2yrs)
loans$pub.rec <- factor(loans$pub.rec)
loans$not.fully.paid <- factor(loans$not.fully.paid)

#Exploratory Data Analysis
#Bar plot
pl <- ggplot(loans,aes(x=factor(purpose))) 
pl <- pl + geom_bar(aes(fill=not.fully.paid),position = "dodge")
pl + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
#print(pl)


############################ BUILDING THE MODEL #############################################

#Spliting train and test data
set.seed(101)
spl = sample.split(loans$not.fully.paid, 0.7)
train = subset(loans, spl == TRUE)
test = subset(loans, spl == FALSE)

#Creating Model and Prediction
model <- svm(not.fully.paid ~ .,data=train)
summary(model)
predicted.values <- predict(model,test[1:13])
table(predicted.values,test$not.fully.paid)

#Tuning the model
tune.results <- tune(svm,train.x=not.fully.paid~., data=train,kernel='radial',
                     ranges=list(cost=c(1,10), gamma=c(0.1,1)))
model <- svm(not.fully.paid ~ .,data=train,cost=10,gamma = 0.1)
predicted.values <- predict(model,test[1:13])
table(predicted.values,test$not.fully.paid)
