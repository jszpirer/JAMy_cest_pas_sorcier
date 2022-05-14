library(randomForest)

tr_set_rf_imputed <- read.csv("tr_set_rf_imputed.csv")
training_set_labels <- read.csv("training_set_labels.csv", stringsAsFactors = T)


X <- tr_set_rf_imputed[,-c(1)]  # Remove respond_id of features
Y <- training_set_labels[,-c(1)]

N <- nrow(X) # Nb of training examples
n <- ncol(X) # Nb of input variables

shuff_idx <- sample(1:N)
half_split <- floor(N/2) # Splitting data for training / testing

X_tr <- X[shuff_idx[1:half_split],]
Y_tr <- Y[shuff_idx[1:half_split],]

X_ts <- X[shuff_idx[(half_split+1):N],]
Y_ts <- Y[shuff_idx[(half_split+1):N],]


n_trees <- 10
accuracy_vec <- matrix(0, n_trees, 4)

for (i in 10:n_trees) { # Structural loop to find the best number of trees
    print(paste("Model for", i, "trees"))
    # Model fit (using randomForest function)
    # 4 modèles pour tester l'efficacité d'évaluer les deux séparéments ou de manière groupée
    model_h1n1 <- randomForest(x=X_tr,
                               y=Y_tr[, c(1)], # as.factor so RF knows it's a classification problem
                               xtest=X_ts,
                               ytest=Y_ts[, c(1)],
                               ntree=i)
    
    model_season <- randomForest(x=X_tr,
                                 y=Y_tr[, c(2)], # as.factor so RF knows it's a classification problem
                                 xtest=X_ts,
                                 ytest=Y_ts[, c(2)],
                                 ntree=i)
    
    model_sum <- randomForest(x=X_tr,
                              y=Y_tr[, c(1)] + Y_tr[, c(2)], # as.factor so RF knows it's a classification problem
                              xtest=X_ts,
                              ytest=Y_ts[, c(1)] + Y_ts[, c(2)],
                              ntree=i)
    
    model_diff <- randomForest(x=X_tr,
                               y=Y_tr[, c(1)] - Y_tr[, c(2)], # as.factor so RF knows it's a classification problem
                               xtest=X_ts,
                               ytest=Y_ts[, c(2)] - Y_ts[, c(1)],
                               ntree=i)
    # Single prediction model 
    Y1_hat = model_h1n1$test$predicted
    Y2_hat = model_season$test$predicted
    
    # Mixed prediction model
    h1n1_hat = (model_sum$test$predicted - model_diff$test$predicted)/2
    seasonal_hat = (model_sum$test$predicted + model_diff$test$predicted)/2

}

par(mfrow=c(1,2))
plot(accuracy_vec[,1],main = "Number of trees influence on h1n1, mixed meth",xlab = "Nbr of trees",ylab = "Classification rate") 
plot(accuracy_vec[,2],main = "Number of trees influence on h1n1, classic",xlab = "Nbr of trees",ylab = "Classification rate") 
confusion_1
par(mfrow=c(1,2))

plot(accuracy_vec[,3],main = "Number of trees influence on seasonal, mixed",xlab = "Nbr of trees",ylab = "Classification rate") 
plot(accuracy_vec[,4],main = "Number of trees influence on seasonal, class",xlab = "Nbr of trees",ylab = "Classification rate")
confusion_2


# Calculs de ROC et AUC
thresholds <- seq(0,0.99,0.01)
FPR <- c()
TPR <- c()
Y_pred <- seasonal_hat
Y <- Y_ts[, c(1)]
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