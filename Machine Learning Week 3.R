## predicting with trees

data(iris)
library(ggplot2)
library(caret)
names(iris)
inTrain <- createDataPartition(y=iris$Species, p=0.7,list=FALSE)
training <- iris[inTrain,]
testing <- iris[-inTrain,]
dim(training); dim(testing)

qplot(Petal.Width,Sepal.Width,colour=Species,data=training)

modFit <- train(Species ~., method="rpart", data=training)
print(modFit$finalModel)

plot(modFit$finalModel, uniform = TRUE,
     main= "Classification Tree")
text(modFit$finalModel, use.n=TRUE, all=TRUE, cex=.8)

library(rattle)
library(rpart.plot)
fancyRpartPlot(modFit$finalModel)

predict(modFit,newdata=testing)

## bagging - resample cases and recalculate predictions

library(ElemStatLearn)
data(ozone,package = "ElemStatLearn")
ozone <- ozone[order(ozone$ozone),]
head(ozone)

ll <- matrix(NA,nrow=10,ncol=155)
for(i in 1:10){
  ss <- sample(1:dim(ozone)[1],replace=T)
  ozone0 <- ozone[ss,]; ozone0 <- ozone0[order(ozone0$ozone),]
  loess0 <- loess(temperature ~ ozone, data=ozone0, span=0.2)
  ll[i,] <- predict(loess0,newdata=data.frame(ozone 1:155))
}

plot(ozone$ozone,ozone$temperature, pch=19, cex=0.5)
for(i in 1:10) {lines(1:155, ll[i,], col="grey", lwd=2)}
lines(1:155,apply(ll,2,mean),col="red",lwd=2)

## for bagging in caret, you can use the train function and bagEarth, treebag, and bagFDA