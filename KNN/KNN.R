#install.packages("ISLR")
library(ISLR)
#print(head(iris))
####
##Scale the data
####
stand.features <- scale(iris[1:4])

###
final.data <- cbind(stand.features,iris[5])
###

#####TRAIN TEST SPLIT
####
set.seed(101)
library(caTools)

sample <-  sample.split(final.data$Species,SplitRatio = 0.7)

train <- subset(final.data,sample == T)
test <- subset(final.data, sample == F)

###########
#####   KNN
##########

library(class)

predicted.species <-  knn(train[1:4], test[1:4], train$Species,k=1)
print(predicted.species)

###Miss Classification
print(mean(test$Species != predicted.species))

#### CHOOSE A K VALUE
######
predicted.species <-NULL
error.rate <- NULL

for(i in 1:10){
  set.seed(101)
  predicted.species <- knn(train[1:4], test[1:4],train$Species, k=i)
  error.rate[i] <- mean(test$Species != predicted.species)
}

#######
##PLOT THIS OUT FOR ELBOW METHOD
#####

library(ggplot2)
k.values <- 1:10
error.df <- data.frame(error.rate,k.values)
######

pl <- ggplot(error.df,aes(x=k.values,y=error.rate)) + geom_point()
pl <- pl + geom_line(lty='dotted', color='red',size=2)
print(pl)

