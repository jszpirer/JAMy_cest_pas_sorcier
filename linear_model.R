# D'abord on met tout ensemble pour avoir un tableau avec features+labels
total_training_features <- read.csv("tr_set_imputed.csv")
training_features <- total_training_features[,c(4, 5, 6, 7, 9, 10, 13, 15, 16, 20, 21, 33, 34, 35, 37, 40)]
summary(training_features)
training_labels <- read.csv("training_set_labels.csv", stringsAsFactors = T)
label_1 <- training_set_labels[,"h1n1_vaccine"]
label_2 <- training_set_labels[, "seasonal_vaccine"]
labels <- cbind(h1n1_vaccine=label_1, seasonal_vaccine=label_2)
total_train_set <- cbind(training_features, labels)

vaccine_idx <- sample(1:nrow(total_train_set))
half_split <- floor(nrow(total_train_set)/2)
train_data_set <- total_train_set[vaccine_idx[1:half_split],]
test_data <- total_train_set[vaccine_idx[(half_split+1):nrow(total_train_set)],]
target_idx <- ncol(train_data)
targets <- c(target_idx, target_idx-1)

# On peut lancer le modèle
model <- lm(cbind(h1n1_vaccine, seasonal_vaccine)~., data=train_data_set)

Y_pred <- predict(model,test_data[,-targets])
Y_pred
dim(Y_pred)
dim(test_data[,targets])
test_data[,targets]


#########################
#   Cross Validation    #
#########################



k = 10
accuracy_vec_h1n1 <- array(0,k)
accuracy_vec_seasonal <- array(0,k)
threshold <- 0.5

# 1. Shuffle the dataset randomly.
vaccine_idx <- sample(1:nrow(train_data_set))

# 2. Split the dataset into k groups
max <- ceiling(nrow(train_data_set)/k)
splits <- split(vaccine_idx, ceiling(seq_along(vaccine_idx)/max))

# 3. For each unique group:
for (i in 1:k){
  #3.1 Take the group as a hold out or test data set
  test_data <- train_data_set[splits[[i]],]
  
  #3.2 Take the remaining groups as a training data set
  train_data <- train_data_set[-splits[[i]],]
  print(paste("[INFO] - Training set size:",dim(train_data)[1],"- Testing set size",dim(test_data)[1]))
  
  #3.3 Fit a model on the training set and evaluate it on the test set
  model <- lm(cbind(h1n1_vaccine, seasonal_vaccine) ~ ., data=train_data)
  Y_pred <- predict(model,test_data[,-targets])
  Y_h1n1 <- test_data[,targets[2]]
  Y_seasonal <- test_data[,targets[1]]
  
  #3.4 Store the prediction of the tree (2 is to take only the P(Y="spam"|x))
  Y_hat <- ifelse(Y_pred > threshold,1,0) 
  # Need one confusion matrix for h1n1 and one for seasonal
  confusion_matrix_h1n1 <- table(Y_hat[,1],Y_h1n1)
  confusion_matrix_seasonal <- table(Y_hat[,2],Y_seasonal)
  
  #3.5 Retain the evaluation score and discard the model
  accuracy_vec_h1n1[i] = (confusion_matrix_h1n1[1,1]+confusion_matrix_h1n1[2,2])/sum(confusion_matrix_h1n1)
  misclassification_rate = 1 - accuracy_vec_h1n1[i]
  print(paste("[INFO] - Misclassification rate h1n1 -",i,"fold:",misclassification_rate))
  
  accuracy_vec_seasonal[i] = (confusion_matrix_seasonal[1,1]+confusion_matrix_seasonal[2,2])/sum(confusion_matrix_seasonal)
  misclassification_rate = 1 - accuracy_vec_seasonal[i]
  print(paste("[INFO] - Misclassification rate seasonal -",i,"fold:",misclassification_rate))
  
}

#4. Summarize the skill of the model using the sample of model evaluation scores
print(paste("[INFO] - Mean misclassification rate h1n1:",1-mean(accuracy_vec_h1n1)))
print(paste("[INFO] - Mean misclassification rate seasonal:",1-mean(accuracy_vec_seasonal)))



#############
#    ROC    #
#############

thresholds <- seq(0,0.99,0.05)
FPR <- c()
TPR <- c()

for(threshold in thresholds){
  Y_hat <- ifelse(Y_pred > threshold,1,0) 
  confusion_matrix <- table(Y_hat[,2],test_data[,targets[1]])
  
  if(dim(confusion_matrix)[1] < 2){ 
    if(rownames(confusion_matrix) == 0){
      confusion_matrix <- rbind(confusion_matrix,c(0,0))
      rownames(confusion_matrix)[2] <- 1
    }
    if(rownames(confusion_matrix) == 1){
      confusion_matrix <- rbind(c(0,0),confusion_matrix)
      rownames(confusion_matrix)[1] <- 0
    }
  }
  
  FP <- confusion_matrix[2,1]
  TP <- confusion_matrix[2,2]
  N_N <- sum(confusion_matrix[,1]) # Total number of 0's
  N_P <- sum(confusion_matrix[,2]) # Total number of 1's
  
  FPR <- c(FPR,FP/N_N)
  TPR <- c(TPR,TP/N_P)
}


plot(FPR,TPR)
lines(FPR,TPR,col="blue")
lines(thresholds,thresholds,lty=2)
title("ROC Curve for seasonal vaccine")
AUC <- sum(abs(diff(FPR)) * (head(TPR,-1)+tail(TPR,-1)))/2
AUC

thresholds <- seq(0,0.99,0.05)
FPR <- c()
TPR <- c()

for(threshold in thresholds){
  Y_hat <- ifelse(Y_pred > threshold,1,0) 
  confusion_matrix <- table(Y_hat[,1],test_data[,targets[2]])
  
  if(dim(confusion_matrix)[1] < 2){ 
    if(rownames(confusion_matrix) == 0){
      confusion_matrix <- rbind(confusion_matrix,c(0,0))
      rownames(confusion_matrix)[2] <- 1
    }
    if(rownames(confusion_matrix) == 1){
      confusion_matrix <- rbind(c(0,0),confusion_matrix)
      rownames(confusion_matrix)[1] <- 0
    }
  }
  
  FP <- confusion_matrix[2,1]
  TP <- confusion_matrix[2,2]
  N_N <- sum(confusion_matrix[,1]) # Total number of 0's
  N_P <- sum(confusion_matrix[,2]) # Total number of 1's
  
  FPR <- c(FPR,FP/N_N)
  TPR <- c(TPR,TP/N_P)
}
AUC <- sum(abs(diff(FPR)) * (head(TPR,-1)+tail(TPR,-1)))/2
AUC

plot(FPR,TPR)
lines(FPR,TPR,col="blue")
lines(thresholds,thresholds,lty=2)
title("ROC Curve for h1n1 vaccine")
