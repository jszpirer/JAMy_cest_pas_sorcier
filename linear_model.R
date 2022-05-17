# D'abord on met tout ensemble pour avoir un tableau avec features+labels
training_features <- read.csv("tr_set_preprocessed.csv")
summary(training_features)
training_labels <- read.csv("training_set_labels.csv", stringsAsFactors = T)
label_1 <- training_set_labels[,"h1n1_vaccine"]
label_2 <- training_set_labels[, "seasonal_vaccine"]
labels <- cbind(h1n1_vaccine=label_1, seasonal_vaccine=label_2)
total_train_set <- cbind(training_features, labels)

vaccine_idx <- sample(1:nrow(total_train_set))
half_split <- floor(nrow(total_train_set)/2)
train_data <- total_train_set[vaccine_idx[1:half_split],]
test_data <- total_train_set[vaccine_idx[(half_split+1):nrow(total_train_set)],]
target_idx <- ncol(train_data)
targets <- c(target_idx, target_idx-1)

# On peut lancer le mod�le
model <- lm(cbind(h1n1_vaccine, seasonal_vaccine)~., data=train_data)

Y_pred <- predict(model,test_data[,-targets])
Y_pred
dim(Y_pred)
dim(test_data[,targets])
test_data[,targets]

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