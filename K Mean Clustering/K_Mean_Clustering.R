#K MEAN CLUSTERING EXAMPLE

#Iclude the required libraries.
library(ISLR)
library(cluster)

#Reading the data

df1 <- read.csv('winequality-red.csv',sep = ';')
df2 <- read.csv('winequality-white.csv', sep = ';')

#TO add labels to data set
df1$label <- sapply(df1$pH,function(x){'red'})
df2$label <- sapply(df2$pH,function(x){'white'})

#combine two dataset
wine <- rbind(df1,df2)


#######################################
##### Exploratory Data Analysis########
#######################################

#Histogram for suger in wines
library(ggplot2)

pl <- ggplot(wine,aes(residual.sugar)) + geom_histogram(aes(fill=label), color='black', bins = 50)
pl + scale_fill_manual(values = c('#ae4554','#faf7ea'))+ theme_bw()             

#Lets draw a scaterplot
pl <- ggplot(wine,aes(x=citric.acid,y=residual.sugar)) + geom_point(aes(color=label),alpha=0.2)
pl + scale_color_manual(values = c('#ae4554','#faf7ea')) +theme_dark()


##################################################
#Building Clustes
clus.data <- wine[,1:12]
wine.cluster <- kmeans(clus.data,2)
print(wine.cluster$centers)

#Lets evaluate the model
table(wine$label,wine.cluster$cluster)
