library(tree)
library(ISLR)

data(Carseats)
names(Carseats)
attach(Carseats)
High <- ifelse(Sales<=8,"No","Yes")
Carseats <- data.frame(Carseats,High)
tree.carseats <- tree(High~CompPrice+Income+Advertising+Population+Price+ShelveLoc+Age+Education+Urban+US,Carseats)
tree.carseats <- tree(High~.-Sales,Carseats)

summary(tree.carseats)

plot(tree.carseats)
text(tree.carseats,pretty=0)
tree.carseats

set.seed(2)
train<-sample(1:nrow(Carseats), 200)

Carseats.test <- Carseats[-train,]

High.test <- High[-train]


tree.carseats <- tree(High~.-Sales, Carseats, subset = train)
tree.pred <- predict(tree.carseats, Carseats.test, type="class")
table(tree.pred,High.test)
(86+57)/200

set.seed(3)
cv.carseats <- cv.tree(tree.carseats, FUN=prune.misclass)
names(cv.carseats)
cv.carseats

par(mfrow=c(1,2))
plot(cv.carseats$size, cv.carseats$dev, type="b")
plot(cv.carseats$k, cv.carseats$dev, type="b")

prune.carseats <- prune.misclass(tree.carseats,best=9)
plot(prune.carseats)
text(prune.carseats,pretty=0)
tree.pred<- predict(prune.carseats, Carseats.test, type = "class")
table(tree.pred,High.test)
(94+60)/200

prune.carseats<-prune.misclass(tree.carseats, best=15)
plot(prune.carseats)
text(prune.carseats,pretty = 0)
tree.pred<-predict(prune.carseats, Carseats.test, type="class")
table(tree.pred,High.test)
(86+62)/200

library(MASS)
data(Boston)
fix(Boston)
names(Boston)

set.seed(1)
train<-sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston<- tree(medv ~ ., Boston, subset=train)
summary(tree.boston)

plot(tree.boston)
text(tree.boston,pretty=0)

cv.boston<-cv.tree(tree.boston)
plot(cv.boston$size, cv.boston$dev, type='b')

prune.boston <- prune.tree(tree.boston, best=5)
plot(prune.boston)
text(prune.boston,pretty=0)

yhat<-predict(tree.boston, newdata = Boston[-train,])
boston.test = Boston[-train,"medv"]
plot(yhat,boston.test)
abline(0,1)
mean((yhat - boston.test)^2)

#Exercises

#Exercise 1
set.seed(2)
train<-sample(1:nrow(Carseats),200)
Carseats.test<-Carseats[-train,]

#Exercise 2
tree.carseats<-tree(Sales~.,data=Carseats,subset=train)
summary(tree.carseats)
tree(Sales~.,Carseats,subset=train)
plot(tree.carseats)
text(tree.carseats,pretty=0)

yhat = predict(tree.carseats, newdata = Carseats.test)
yhat
carseats.test=Carseats[-train,"Sales"]
plot(yhat,carseats.test)
abline(0,1)
mean((yhat-carseats.test)^2)

#MSE obtained is 2.577326

#Exercise 3
set.seed(3)
cv.carseats <- cv.tree(tree.carseats, FUN=prune.misclass)
names(cv.carseats)
cv.carseats
par (mfrow =c(1 ,2))
plot(cv.carseats$size ,cv.carseats$dev ,type ="b")
plot(cv.carseats$k ,cv.carseats$dev ,type ="b")
prune.carseats<-prune.tree(tree.carseats,best=9)
plot(prune.carseats)
text(prune.carseats,pretty=0)

yhat<-predict(prune.carseats,newdata=Carseats[-train,])
carseats.test=Carseats[-train,"Sales"]
plot(yhat,carseats.test)
abline(0,1)
mean((yhat-carseats.test)^2)

#Pruning The Tree does not improves the test MSE