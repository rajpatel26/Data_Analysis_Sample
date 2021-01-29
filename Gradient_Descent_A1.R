# Let's import the library and fit our data
library(ISLR)
attach(Auto)
fix(Auto)
dim(Auto) # just to check if data is correctly loaded
names(Auto) # checking our variables

#Our data seems to have no NA values so it's good to use.

#Spliting our data into training and test sets
total <- nrow(Auto)
training <- total/2
test <- total-training

set.seed(2606) # to get the same set again
train <- sample(total,size=training) #random values for training

# Creating high variable
high <- ifelse(mpg>22.5,1,0)

# Creating dummy variables for origin since it is qualitative
# for an attribute taking K possible values we need K-1 dummy variables, so we create only 2 variables
D1 <- ifelse(origin==2,1,0) # If car origin is European
D2 <- ifelse(origin==3,1,0) # If car origin is Japanese

# Let's scale the attributes
# scaling only train to avoid data snooping
high1 <- high #unscaled

#scale horsepower
mean.hp <- mean(horsepower[train])
sd.hp <- sd(horsepower[train])
HP <- (horsepower-mean.hp)/sd.hp

#scale weight
mean.w <- mean(weight[train])
sd.w <- sd(weight[train])
W <- (weight-mean.w)/sd.w

#scale year
mean.y <- mean(year[train])
sd.y <- sd(year[train])
Y <- (year-mean.y)/sd.y

#Didn't scale origin because we already created dummy variables

#Defining p as no. of attributes
p <- 5

#Summing our scaled attributes as new train set
X_train <- matrix(nrow=training,ncol=p)
X_train[,1] <- HP[train]
X_train[,2] <- W[train]
X_train[,3] <- Y[train]
X_train[,4] <- D1[train]
X_train[,5] <- D2[train]
y_train <- high1[train] #unscaled train label mpg

#Summing our test set
X_test <- matrix(nrow=test,ncol=p)
X_test[,1] <- HP[-train]
X_test[,2] <- W[-train]
X_test[,3] <- Y[-train] 
X_test[,4] <- D1[-train]
X_test[,5] <- D2[-train]
y_test <- high1[-train] #unscaled test label mpg

#Randomly setting no. of epoch and learning rate

#total_epoch <- 5000 #should be marginaly greater than 1/learning rate 
#total_epoch <- 2000
total_epoch <- 1000

#learning_rate <- 0.001 #should be small for smooth results
#learning_rate <- 0.01
learning_rate <- 0.1


#Let's initialize beta
beta <- runif(p+1,-0.7,0.7)

#Let's define the sigmoid & sigmoid prime functions
sigmoid <- function(z){1/(1+exp(-z))}
sigmoid_prime <- function(z){exp(z)/(exp(z)+1)^2}

pass <- function(i){
  #forward pass
  A <- beta[p+1] #beta_0
  for (j in 1:p) {  A <- A + beta[j]*X_train[i,j] }
  P <- sigmoid(A)
  #backward pass
  d <- 2 * (y_train[i]-P) * sigmoid_prime(A)
  return(d)
}

#gradient descent
for(epoch in 1:total_epoch){
  for (i in 1:training) {
    d=pass(i)
    for (j in 1:p) {
      beta[j] <- beta[j] + (learning_rate/training) * d * X_train[i,j] #weight update
    }
    beta[p+1] <- beta[p+1] + (learning_rate/training) * d #weight update
  }
}

#Calculate test_mse & train_mse
test_mse <- 0
train_mse <- 0

#test_mse
for (i in 1:test) {
  A <- beta[p+1]
  for (j in 1:p) {
    A <- A + beta[j] * X_test[i,j]
  }
  P <- sigmoid(A)
  test_mse <- test_mse + ((y_test[i]-P)^2)
}
test_mse <- test_mse/test

#train_mse
for (i in 1:training) {
  A <- beta[p+1]
  for (j in 1:p) {
    A <- A + beta[j] * X_train[i,j]
  }
  P <- sigmoid(A)
  train_mse <- train_mse + ((y_train[i]-P)^2)
}
train_mse <- train_mse/training

print(test_mse)
print(train_mse)

