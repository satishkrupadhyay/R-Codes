#Program to demostrate the use of Decision Tree method (machine learning)
#Data is taken from ISLR package, college dataframe

#Loading Library

library(ISLR)
library(ggplot2)
library(caTools)
library(rpart)
library(rpart.plot)
library(randomForest)

#Loading data in to datafame df 
df <- College

#Exploratory Data Analysis
#Private 
#print(ggplot(df, aes(Room.Board,Grad.Rate)) + geom_point(aes(color=Private), size=4, alpha=0.5)

#Full time undergraduate student
#print(ggplot(df,aes(F.Undergrad)) + geom_histogram(aes(fill=Private), color='black',bins=50, alpha=0.5) + theme_bw())

# Data cleaning

df['Cazenovia College','Grad.Rate'] <- 100
#subset(df, Grad.Rate >100)

# Train Test split
set.seed(101)

sample <-  sample.split(df$Private, SplitRatio = 0.70)
train <-  subset(df,sample==T)
test <- subset(df, sample == F)

########
#TRAIN MODEL
#####

tree <-  rpart(Private ~.,method = 'class', data = train)
tree.preds <- predict(tree,test)

tree.preds <-  as.data.frame(tree.preds)
joiner <- function(x){
  if (x >=0.5){
    return('Yes')
  }else{
    return('No')
  }
}

tree.preds$Private <- sapply(tree.preds$Yes,joiner)


############ Plotting the tree with rpart.plot ###############

#print(prp(tree))

##############################################################
#################### RANDOM FOREST ###########################
##############################################################

rf.model <- randomForest(Private ~., data = train, importance=TRUE)
print(rf.model$confusion)
print(rf.model$importance)
print(rf.preds <- predict(rf.model,test))
print(table(rf.preds,test$Private))
