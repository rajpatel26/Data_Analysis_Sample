LoadLibraries <- function (){library(ISLR)
  library(MASS)
  print("The libraries have been loaded.")
}

LoadLibraries
function(){
library(ISLR)
library(MASS)
print("The libraries have been loaded.")
}

LoadLibraries()

curve(x*sin(x^2),-3,3)

x <- seq(-3,3,0.01)
y <- x*sin(x^2)
plot(x,y,type="l")

#source("script_test.R")

cube <- function(x){
  return(x^3)
}

cube(3)

norm <- function(x,y=0) {
  squared.norm <- x^2+y^2
  return(sqrt(squared.norm))
}

norm(3,4)

for (i in 1:4)
  print(i^2)


attach(Caravan)
summary(Purchase)

standardized.X <- scale(Caravan[,-86])

var(Caravan[,1])
var(Caravan[,2])

var(standardized.X[,1])
var(standardized.X[,2])

library(class)
test <- 1:1000
train.X <- standardized.X[-test,]
test.X <- standardized.X[test,]
train.Y <- Purchase[-test]
test.Y <- Purchase[test]
set.seed(1)
knn.pred <- knn(train.X,test.X,train.Y,k=1)
mean(test.Y!=knn.pred)

mean(test.Y!="No")


table(knn.pred,test.Y)


knn.pred <- knn(train.X,test.X,train.Y,k=3)
table(knn.pred,test.Y)

knn.pred <- knn(train.X,test.X,train.Y,k=5)
table(knn.pred,test.Y)


glm.fit <- glm(Purchase~.,data=Caravan,family=binomial,subset=-test)
glm.probs <- predict(glm.fit,Caravan[test,],type="response")
glm.pred <- rep("No",1000)
glm.pred[glm.probs>.5] <- "Yes"
table(glm.pred,test.Y)

glm.pred <- rep("No",1000)
glm.pred[glm.probs>.25] <- "Yes"
table(glm.pred,test.Y)

#EXERCISES

#1
Power<- function (){
  print(2^3)
}

Power()


#2

Power2 = function (x,a){
  print(x^a)
}

Power2(3,8)


#Ans 3

Power2(10,3)

Power2(8,17)

Power2(131,3)

#Ans 4
Power3 <- function (x,a){
  result <- x^a 
  return(result)
}

#Ans 5

x <- c(1:10)
y <- Power3(x,2)
plot(x,y)

#Ans 6
x <- c(1:10)
y <- Power3(x,2)
plot(x,y)

PlotPower <- function(x,a){
  y <- Power3(x,a)
  plot(x,y)
}

#Ans 7

sum2 <- function(n){
  addition <- 0
  for (i in 1:n)
    addition <- addition + Power3(i,2)
  return(sum_add)
}
sum2(3)

#Ans 8
source("sum3.R")
sum3(3)s