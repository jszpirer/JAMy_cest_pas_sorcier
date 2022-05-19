training_features <- read.csv("tr_set_imputed.csv")[, -c(1)]
summary(training_features)
training_labels <- read.csv("training_set_labels.csv", stringsAsFactors = T)
label_1 <- training_labels[,"h1n1_vaccine"]
label_2 <- training_labels[, "seasonal_vaccine"]
labels <- cbind(h1n1_vaccine=label_1, seasonal_vaccine=label_2)

testing_features <- read.csv("ts_set_imputed.csv")[, -c(1)]
feature_set <- rbind(training_features, testing_features)
feature_set <- feature_set[, -c(1)] # remove resp id
pca <- prcomp(feature_set)
summary(pca)

pca_feat_set <- pca$x[,1:54]

# install.packages("caret")
library("caret")

ss <- preProcess(as.data.frame(pca_feat_set), method=c("range"))
pca_feat_set <- predict(ss, as.data.frame(pca_feat_set))

#pca_tr_set <- pca_feat_set[1:26707,]
#pca_ts_set <- pca_feat_set[26708:53415,]
pca_tr_set <- pca_feat_set[1:13500,]
pca_ts_set <- pca_feat_set[13500:26707,]
#total_train_set <- cbind(pca_tr_set, label_1)
label_11<-label_1[1:13500]
label_22<-label_2[1:13500]

library("e1071")
data_svm1 = svm(label_11 ~ ., data = pca_tr_set, kernel = "polynomial",degree=3, gamma =0.5, cost = 1, scale = FALSE)
Y_pred1 <- predict(data_svm1,pca_ts_set)
ss <- preProcess(as.data.frame(Y_pred1), method=c("range"))
Y_pred1 <- predict(ss, as.data.frame(Y_pred1))

#second column
data_svm2 = svm(label_22 ~ ., data = pca_tr_set, kernel = "polynomial",degree=3, cost = 10, scale = FALSE)
Y_pred2 <- predict(data_svm2,pca_ts_set)
ss <- preProcess(as.data.frame(Y_pred2), method=c("range"))
Y_pred2 <- predict(ss, as.data.frame(Y_pred2))

final_pred <- cbind(testing_features[, c(1)], Y_pred1, Y_pred2)
write.csv(final_pred, "test_submission.csv", row.names = F)

# Calculs de ROC et AUC
thresholds <- seq(0,0.99,0.01)
FPR <- c()
TPR <- c()
Y_pred <- Y_pred1
Y <-label_1[13500:26707]
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

