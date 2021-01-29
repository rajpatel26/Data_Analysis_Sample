#Task 1
#Decision Stump

# importing the library and fit our data
library(MASS)
attach(Boston)
fix(Boston)
dim(Boston) # just to check if data is correctly loaded
names(Boston) # checking our variables

#Spliting our data into training and test sets
total <- nrow(Boston)
training <- total/2
test <- total-training

set.seed(2606) #reusability
train <- sample(total,size=training) #random values for training

#Spliting into X_train using lstat and rm & y_train using medv 
X_train <- matrix(nrow=training,ncol=2)
X_train[,1] <- lstat[train]
X_train[,2] <- rm[train]
y_train <- medv[train]

#Spliting into X_test using lstat and rm & y_test using medv
X_test <- matrix(nrow=test,ncol=2)
X_test[,1] <- lstat[-train]
X_test[,2] <- rm[-train]
y_test <- medv[-train]

#defining the length of train and test samples
n_lstat_train <- n_rm_train <-length(X_train[,1])
n_lstat_test <- n_rm_test <- length(X_test[,1])

#Main DS function
DecisionStump <- function(y_train, n_lstat_test, n_lstat_train, n_rm_test, n_rm_train) {
  
  #threshold sequence for lstat
  s_lstat <- seq(1.8, 37.9, 0.1)
  best_s_lstat = Inf
  best_threshold_lstat = 0
  
  #threshold sequence for rm
  s_rm <- seq(3.6, 8.7, 0.1)
  best_s_rm = Inf
  best_threshold_rm = 0
  
  #lstat
  # Comparing lstat train values with threshold
  for (k in s_lstat) {
    index_lstat_train_less = c()
    index_lstat_train_more = c()
    for (i in 1:n_lstat_train){
      if (X_train[,1][i] < k){
        index_lstat_train_less <- c(index_lstat_train_less,i) #fetching index which are less than threshold
      }
      else {
        index_lstat_train_more <- c(index_lstat_train_more,i) #fetching index which are more than threshold
      }
    }
    
    # Fetching train labels     
    y_train_lstat_less <- y_train[index_lstat_train_less]
    y_train_lstat_more <- y_train[index_lstat_train_more]
    
    # Calculating mean
    mean_lstat_less <- mean(y_train_lstat_less)
    mean_lstat_more <- mean(y_train_lstat_more)
    
    # RSS for lstat
    RSS_lstat <- sum((y_train_lstat_less - mean_lstat_less)^2) + sum((y_train_lstat_more - mean_lstat_more)^2)
    
    # Best RSS & Best Attributes
    if (RSS_lstat < best_s_lstat ){
      best_s_lstat = RSS_lstat
      best_threshold_lstat = k
      best_lstat_mean_less = mean_lstat_less
      best_lstat_mean_more = mean_lstat_more
    }
   }
    
    #rm
    #Comparing rm train values with threshold
    for (k in s_rm) {
    index_rm_train_less = c()
    index_rm_train_more = c()
      for (i in 1:n_rm_train){
        if (X_train[,2][i] < k){
          index_rm_train_less <- c(index_rm_train_less,i)
        }
        else {
          index_rm_train_more <- c(index_rm_train_more,i)
        }
      }

    
      # Fetching train labels
      y_train_rm_less <- y_train[index_rm_train_less]
      y_train_rm_more <- y_train[index_rm_train_more]
  
      # Calculating mean
      mean_rm_less <- mean(y_train_rm_less)
      mean_rm_more <- mean(y_train_rm_more)
  
      # RSS for rm
      RSS_rm <- sum((y_train_rm_less - mean_rm_less)^2) + sum((y_train_rm_more - mean_rm_more)^2)
    
      # Best RSS & Best Attributes
      if (RSS_rm < best_s_rm ){
        best_s_rm = RSS_rm
        best_threshold_rm = k
        best_rm_mean_less = mean_rm_less
        best_rm_mean_more = mean_rm_more
    }
  }
  
    #Comparision between lstat and rm. Returns best values for attribute which has best RSS
    best_s <- ifelse (best_s_rm < best_s_lstat, best_threshold_rm, best_threshold_lstat)
    best_mean_less <- ifelse(best_s_rm < best_s_lstat, best_rm_mean_less,best_lstat_mean_less)
    best_mean_more <- ifelse(best_s_rm < best_s_lstat, best_rm_mean_more,best_lstat_mean_more)
    best_attr <- ifelse(best_s_rm < best_s_lstat, "rm", "lstat")
    returnArray= c(best_attr,best_s,best_mean_less,best_mean_more)
    
}

DS <- DecisionStump(y_train,n_lstat_train,n_lstat_test,n_rm_train,n_rm_test)
print(DS)

#if lstat or rm for train
lstat_or_rm<-0
S=as.numeric(DS[2])
if(DS[1]=="lstat"){
  lstat_or_rm<-X_train[,1]
}else if(DS[1]=="rm"){
  lstat_or_rm<-X_train[,2]
}
mean_greater_than=as.numeric(DS[4])
mean_less_than=as.numeric(DS[3])

#print(mean_greater_than)
#print(mean_less_than)

# TRAINING MSE
sumlessthanSquare <- sumgreaterthanSquare <- 0
for(i in 1:nrow(X_train)){
  if (lstat_or_rm[i] < S )
  {
    sumlessthanSquare <- sumlessthanSquare + (y_train[i]-mean_less_than)^2
  }
  if(lstat_or_rm[i] > S){
    sumgreaterthanSquare <-  sumgreaterthanSquare+(y_train[i]-mean_greater_than)^2
  }
}

final_train_rss=sumlessthanSquare + sumgreaterthanSquare

#if lstat or rm for test
lstat_or_rm<-0
S=as.numeric(DS[2])
if(DS[1]=="lstat"){
  lstat_or_rm<-X_test[,1]
}else if(DS[1]=="rm"){
  lstat_or_rm<-X_test[,2]
}
mean_greater_than=as.numeric(DS[4])
mean_less_than=as.numeric(DS[3])


# TEST MSE
sumlessthanSquare <- sumgreaterthanSquare <- 0
for(i in 1:nrow(X_train)){
  if (lstat_or_rm[i] < S )
  {
    sumlessthanSquare <- sumlessthanSquare + (y_test[i]-mean_less_than)^2
  }
  if(lstat_or_rm[i] > S){
    sumgreaterthanSquare <-  sumgreaterthanSquare+(y_test[i]-mean_greater_than)^2
  }
}

final_test_rss=sumlessthanSquare + sumgreaterthanSquare

print(c("TRAIN MSE : ",final_train_rss/length(X_train[,1])))
print(c("TEST MSE : " ,final_test_rss/length(X_test[,1])))



#Task 2 & Task 3
#Boosted Decision Stumps
B = 1000 #no. of trees
eta=0.01 #learning rate

#Boosted Decision Funtion
 BDS <- function(B,X_train,y_train,X_test,y_test,eta){
  fhat <- matrix(0,nrow=B,ncol=nrow(X_train),byrow=TRUE) #fhat
  different_mse <- matrix(nrow=B,ncol=1,byrow=TRUE) #For storing MSE's
  DS1 <- matrix(nrow=B,ncol=4,byrow=TRUE) #For storing 1000 instances of best attribute and it's values from DS function
  for(i in 1:B){
    DS1[i,]<- DecisionStump(y_train,n_lstat_train,n_lstat_test,n_rm_train,n_rm_test) 
    for(j in 1:nrow(X_train)){
      if(DS1[i,1]=="lstat"){
        if(X_train[,1][j] < as.numeric(DS1[i,2])){
          fhat[i,j] <- as.numeric(DS1[i,3])
        }else{
          fhat[i,j] <- as.numeric(DS1[i,4])
        }
      }
      else if(DS1[i,1]=="rm"){
        if(X_train[,2][j] < as.numeric(DS1[i,2])){
          fhat[i,j] <- as.numeric(DS1[i,3])
        }else{
          fhat[i,j] <- as.numeric(DS1[i,4])
        }
      }
      y_train[j] <- y_train[j]-eta*fhat[i,j]
    }
    sqd<-0
    for(each_val in 1:nrow(X_test)){
      p_rule = sum(eta*fhat[,each_val])
      sqd = sqd+(y_test[each_val]-(p_rule))^2
    }
    test_mse=sqd/nrow(X_test)
    different_mse[i,1]=test_mse

    print(c("TEST_MSE:",test_mse,"FOR B=",i))
  }
}
BDS(B,X_train,y_train,X_test,y_test,eta)

#Ploting
plot(1:B,different_mse,type='l',xlab="Number of trees",ylab="MSE Value",main = "MSE vs No. of Trees", col='blue')
