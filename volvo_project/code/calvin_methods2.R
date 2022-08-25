library(fpc)
library(dbscan)
library(tidyverse)
library(dplyr)
library(factoextra)
library(caret)
library(ROCR)
library(xtable)

classes <- seq(0,9)

labels <- c(1,0)

### training data ####
X_train <- read.csv("X_train.csv")
colnames(X_train)[2:11] <- classes 

### validation data ####
X_valid <- read.csv("X_valid.csv")
colnames(X_valid)[2:11] <- classes

small_valid <- read.csv("small_valid.csv")
colnames(small_valid)[2:11] <- classes 


#### DBSCAN Classwise #####

#choose training data
train_data <- X_train


#choose validation data
valid_data <- small_valid


# finding optimal classwise eps and minpts

eps <- seq(0.001,1.0,length.out = 5)
minpts <- c(10,20,30,40,50,100,200,300,500)
### Training subsets

# inliers and missclassified inliers
train_data <- subset(train_data,type == "x_in" | type == "x_miss")
train_data[4769:4868,] <- temp_1


# only inliers 
train_data <- subset(train_data, type =="x_in")

# inliers and outliers
train_data <- subset(train_data, type == "x_in" | type == "x_out")


auc_class <- numeric(length(10))
auc_index <- list(length(10))

for (i in 1:10){
  train <- subset(train_data, class == i-1)
  valid <- subset(valid_data, class == i-1)
  auc_matrix <- matrix(data = NA, nrow = length(eps), ncol = length(minpts))
  print(i/10)
  for (j in 1:length(eps)){
    for( k in 1:length(minpts)){
      set.seed(123)
      db <- dbscan(train[,2:11], eps = eps[j], minPts = minpts[k])
      pred <- predict(db, newdata = valid[,2:11], data = train[,2:11])
      pred[which(pred != 0)] <- 1
      
      conf <- confusionMatrix(as.factor(pred),as.factor(valid$inlier),positive = "0")
      
      #print(conf$table)
      
      #AUROC
      predi <- prediction(pred, valid$inlier)
      perf <- performance(predi,"auc")
      
      auc_matrix[j,k] <- perf@y.values[[1]][1]
     
    }
  }
  auc_class[i] <- max(auc_matrix)
  auc_index[[i]] <- which(auc_matrix == max(auc_matrix), arr.ind = TRUE)
}

### Optimal Eps and Minpts for each class ###

optimal <- list(length(10))

for (i in 1:10){
  opt <- auc_index[[i]]
  eps_minpts <- matrix(data = NA, nrow = nrow(opt), ncol = 2)
  
  for (j in 1:nrow(opt)){
    
    optimum <- opt[j,]
    
    eps_minpts[j,] <- c(eps[optimum[1]],minpts[optimum[2]])
    
  }
  optimal[[i]] <- eps_minpts
}


###### CODE FOR EVALUATION OF VALIDATON DATA ######

AUC <- numeric(10)

CONF_MAT <- list(length(10))

temp <- list(length(10))

TOTAL_CONF <- matrix(data = 0,2,2)

for (i in 1:10){
  train <- subset(train_data, class == i-1)
  test <- subset(valid_data, class == i-1)
  
  set.seed(123)
  db <- dbscan(train[,2:11], eps = optimal[[i]][1,1], minPts = optimal[[i]][1,2])
  #db <- dbscan(train[,2:11], eps = 0.068, minPts = 100)
  pred <- predict(db, newdata = test[,2:11], data = train[,2:11])
  pred[which(pred != 0)] <- 1
  
  temp [[i]]<- cbind(test$type,pred)
  
  conf <- confusionMatrix(as.factor(pred),as.factor(test$inlier))
  
  CONF_MAT[[i]] <- conf$table
  
  TOTAL_CONF[1,1] <- TOTAL_CONF[1,1] + conf$table[1,1]
  TOTAL_CONF[1,2] <- TOTAL_CONF[1,2] + conf$table[1,2]
  TOTAL_CONF[2,1] <- TOTAL_CONF[2,1] + conf$table[2,1]
  TOTAL_CONF[2,2] <- TOTAL_CONF[2,2] + conf$table[2,2]
  
  #AUROC
  predi <- prediction(pred, test$inlier)
  perf <- performance(predi,"auc")
  
  AUC[i] <- perf@y.values[[1]][1]
}

### CODE FOR BREAKING DOWN CLASSIFICATIONS ####
correct_miss <- numeric(length(10))
correct_ood <- numeric(length(10))

nr_miss <- numeric(length(10))
nr_ood <- numeric(length(10))


for (i in 1:10){
  
  class_pred <- temp[[i]]
  
  nr_miss[i] <- length(which(class_pred[,1] == 2))
  nr_ood[i] <- length(which(class_pred[,1] == 3))
  
  correct_miss[i] <- length(which(class_pred[,1] == 2 & class_pred[,2] == 0))
  correct_ood[i] <- length(which(class_pred[,1] == 3 & class_pred[,2] == 0))
  
}

acc_miss <- correct_miss/nr_miss
acc_ood <- correct_ood/nr_ood

tot_acc_miss <- sum(correct_miss)/sum(nr_miss)
tot_acc_ood <- sum(correct_ood)/sum(nr_ood)



##### CODE FOR FINAL TESTING ######

X_train <- read.csv("X_train.csv")
colnames(X_train)[2:11] <- classes 

X_test <- read.csv("X_test.csv")
colnames(X_test)[2:11] <- classes

small_test <- read.csv("small_test.csv")
colnames(small_test)[2:11] <- classes


# ALL, SMALL , LARGE 
full_train <- X_train

opt_all_large <- list.load("optimal_X_all_large.rdata")

opt_all_small <- list.load("optimal_all_small.rdata")



# IN_MISS, SMALL , LARGE
train_in_miss <- subset(X_train,type == "x_in" | type == "x_miss")

opt_inmiss_large <- list.load("optimal_in_miss_large.rdata")

opt_inmiss_small <- list.load("optimal_in_miss_small.rdata")


# IN , SMALL , LARGE
train_in <- subset(X_train,type == "x_in")

opt_in_large <- list.load("optimal_in_large.rdata")

opt_in_small <- list.load("optimal_in_small.rdata")



# choose training, testing and optimal

train_data <- train_in
test_data <- small_test
optimal <- opt_in_small


AUC_test <- numeric(10)

CONF_MAT_test <- list(length(10))

temp_test <- list(length(10))

TOTAL_CONF_test <- matrix(data = 0,2,2)

for (i in 1:10){
  train <- subset(train_data, class == i-1)
  test <- subset(test_data, class == i-1)
  
  set.seed(123)
  db <- dbscan(train[,2:11], eps = optimal[[i]][1,1], minPts = optimal[[i]][1,2])
  pred <- predict(db, newdata = test[,2:11], data = train[,2:11])
  pred[which(pred != 0)] <- 1
  
  conf <- confusionMatrix(as.factor(pred),as.factor(test$inlier))
  
  temp_test[[i]]<- cbind(test$type,pred)
  
  CONF_MAT_test[[i]] <- conf$table
  
  TOTAL_CONF_test[1,1] <- TOTAL_CONF_test[1,1] + conf$table[1,1]
  TOTAL_CONF_test[1,2] <- TOTAL_CONF_test[1,2] + conf$table[1,2]
  TOTAL_CONF_test[2,1] <- TOTAL_CONF_test[2,1] + conf$table[2,1]
  TOTAL_CONF_test[2,2] <- TOTAL_CONF_test[2,2] + conf$table[2,2]
  
  #AUROC
  predi <- prediction(pred, test$inlier)
  perf <- performance(predi,"auc")
  
  AUC_test[i] <- perf@y.values[[1]][1]
}


### CODE FOR BREAKING DOWN CLASSIFICATIONS ####
correct_miss <- numeric(length(10))
correct_ood <- numeric(length(10))

nr_miss <- numeric(length(10))
nr_ood <- numeric(length(10))


for (i in 1:10){
  
  class_pred <- temp_test[[i]]
  
  nr_miss[i] <- length(which(class_pred[,1] == 2))
  nr_ood[i] <- length(which(class_pred[,1] == 3))
  
  correct_miss[i] <- length(which(class_pred[,1] == 2 & class_pred[,2] == 0))
  correct_ood[i] <- length(which(class_pred[,1] == 3 & class_pred[,2] == 0))
  
}

acc_miss <- correct_miss/nr_miss
acc_ood <- correct_ood/nr_ood

tot_acc_miss <- sum(correct_miss)/sum(nr_miss)
tot_acc_ood <- sum(correct_ood)/sum(nr_ood)

nr_miss[11] <- sum(nr_miss)
nr_ood[11] <- sum(nr_ood)

acc_miss[11] <- sum(correct_miss)/nr_miss[11]
acc_ood[11] <- sum(correct_ood)/nr_ood[11]

correct_miss[11] <- sum(correct_miss)
correct_ood[11] <- sum(correct_ood)


stat <- rbind(nr_miss,correct_miss,acc_miss,nr_ood,correct_ood,acc_ood)
colnames(stat) <- c(0:9,"TOT")






