library(corrgram)
library(corrplot)
library(caTools)
library(Amelia)
library(ggplot2)
library(dplyr)

#### Getting the data

library(ISLR)

head(iris)

####
var(iris[,1])
var(iris[,2])

#### Making standardization

library(class)

standard.iris <- scale(iris[1:4])

## Binding both standard.iris and species 

final.iris <- cbind(standard.iris,iris[5])

View(final.iris)

## Testing variance, it should now show us 1 for every one 

var(standard.iris[,1])
var(standard.iris[,2])

### Making test and train

sample <- sample.split(final.iris,SplitRatio = 0.7)
train <- subset(final.iris,sample == TRUE)
test <- subset(final.iris,sample==FALSE)

#### KNN Model

model.species <- knn(train[1:4],test[1:4],train$Species,k=1)

print(model.species)

#### Mean error

meanerror <- mean(test$Species != model.species)

print(meanerror)

### Elbow method or graph to see the k values and when it stabilizes

model.species <- NULL
error.rate <- NULL

for (i in 1:10){
  model.species <- knn(train[1:4],test[1:4],train$Species,k=i)
  error.rate[i] <- mean(test$Species != model.species)
}

k.values <- 1:10

df <- data.frame(error.rate,k.values)

print(df)

ggplot(df,aes(k.values,error.rate)) + geom_point(position=position_jitter(w=1, h=0),aes(color=k.values),alpha=0.5) + geom_line(lty='dotted',color='red')

## although the error value starts high but as the k value increases the error rate goes down significantly
## also this data set is too small to really implement elbow method