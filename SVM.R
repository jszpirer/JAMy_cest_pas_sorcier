#opening files
data <- read.csv("tr_set_preprocessed", stringsAsFactors = T)
dim(data)


training_set_labels <- read.csv("training_set_labels.csv", stringsAsFactors = T)
dim(training_set_labels)

#install.packages("e1071")
library("e1071")
y<- training_set_labels[,2]
length(y)
data <- data[,]
dim(data)

data_svm = svm(y ~ ., data = data, kernel = "linear", cost = 0.00001, scale = FALSE)

library(caret)
pred <- predict(data_svm,data)



# Calculs de ROC et AUC
thresholds <- seq(0,0.99,0.01)
FPR <- c()
TPR <- c()
Y_pred <- pred
Y <- y
for(threshold in thresholds){
  Y_hat <- ifelse(Y_pred > threshold,1,0) 
  confusion_matrix <- table(Y_hat,Y)
  
  if(dim(confusion_matrix)[1] < 2){ # Their is a possibility of having no spam/non spam elements in Y_hat -> we make the matrix the right size
    if(rownames(confusion_matrix) == 0){
      confusion_matrix <- rbind(confusion_matrix,c(0,0))
      rownames(confusion_matrix)[2] <- "1"
    }
    if(rownames(confusion_matrix) == 1){
      confusion_matrix <- rbind(c(0,0),confusion_matrix)
      rownames(confusion_matrix)[1] <- "0"
    }
  }
  
  FP <- confusion_matrix[2,1] # False positive
  TP <- confusion_matrix[2,2] # True positive
  N_N <- sum(confusion_matrix[,1]) # Total number of non vaccined
  N_P <- sum(confusion_matrix[,2]) # Total number of vaccined
  FPR <- c(FPR,FP/N_N)
  TPR <- c(TPR,TP/N_P)
}



FPR <- c(1, FPR, 0)
TPR <- c(1, TPR, 0)
plot(FPR,TPR, xlim=seq(0,1), ylim=seq(0,1))
lines(FPR,TPR,col="blue")
lines(thresholds,thresholds,lty=2)
title("ROC Curve")
AUC <- sum(abs(diff(FPR)) * (head(TPR,-1)+tail(TPR,-1)))/2
AUC                

