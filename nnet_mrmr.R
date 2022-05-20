mrmr_tr_set <- read.csv("mrmr_tr.csv")
mrmr_ts_set <- read.csv("mrmr_ts.csv")
labels <- read.csv("training_set_labels.csv")
label_1 <- labels[,c(2)]
label_2 <- labels[,c(3)]


# Calculs de ROC et AUC
compute_AUC <- function(Y_pred, Y) {
    thresholds <- seq(0,0.99,0.01)
    FPR <- c()
    TPR <- c()
    for(threshold in thresholds){
        Y_hat <- ifelse(Y_pred > threshold,1,0) 
        confusion_matrix <- table(Y_hat,Y)
        
        if(dim(confusion_matrix)[1] < 2){ # Their is a possibility of having no spam/non spam elements in Y_hat -> we make the matrix the right size
            if(rownames(confusion_matrix)[1] == 0){
                confusion_matrix <- rbind(confusion_matrix,c(0,0))
                rownames(confusion_matrix)[2] <- "1"
            }
            if(rownames(confusion_matrix)[1] == 1){
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
    # plot(FPR,TPR, xlim=seq(0,1), ylim=seq(0,1))
    # lines(FPR,TPR,col="blue")
    # lines(thresholds,thresholds,lty=2)
    # title("ROC Curve")
    AUC <- sum(abs(diff(FPR)) * (head(TPR,-1)+tail(TPR,-1)))/2
    return(AUC)
}


auc_nnet_cross_valid <- function(X, Y, hidden_nodes=10,k=5,threshold=0.5, target=0){
    # Targe 0 for h1n1, 1 for seasonal
    accuracy_vec <- c()
    
    # 1. Shuffle the dataset randomly.
    shuffled_idx <- sample(1:nrow(X))
    half_split <- floor(nrow(X)/2)
    
    # 2. Split the dataset into k groups
    max <- ceiling(nrow(X)/k)
    splits <- split(shuffled_idx, ceiling(seq_along(shuffled_idx)/max))
    
    # 3. For each unique group:
    for (i in 1:k){
        #3.1 Take the group as a hold out or test data set
        X_test <- X[splits[[i]],]
        Y_test <- Y[splits[[i]]]
        
        #3.2 Take the remaining groups as a training data set
        X_train <- X[-splits[[i]],]
        Y_train <- Y[-splits[[i]]]
        # print(paste("[INFO] - Training set size:",dim(train_data)[1],"- Testing set size",dim(test_data)[1]))
        
        #3.3 Fit a model on the training set and evaluate it on the test set
        model <- nnet(x=X,
                      y=Y,
                      size=hidden_nodes,
                      skip=FALSE, maxit=1000,rang=1,MaxNWts=10000,trace=FALSE)
        print(paste("reached maxit ?",model$convergence))
        Y_pred<-predict(model,X_test)
        
        #3.4 Retain the evaluation score and discard the model
        accuracy_vec[i] <- compute_AUC(Y_pred, Y_test)
    }
    
    return(mean(accuracy_vec))
}

library(nnet)

k <- 5
#hidden_nodes_vec <- c(3,5,10,15,30)#,50,100) # Testing up to 100 hidden nodes, takes some time to train
# hidden_nodes_vec <- seq(95,115,2)
hidden_nodes_vec <- seq(10, 60, 10)#, 31, 33, 50)
#hidden_nodes_vec <- c(60)
results1 <- c()
results2 <- c()
for(hidden_nodes in hidden_nodes_vec){
    print(paste("[INFO] - Testing h=",hidden_nodes))
    results1 <- c(results1, auc_nnet_cross_valid(mrmr_tr_set, label_1, hidden_nodes=hidden_nodes,k=k))
    results2 <- c(results2, auc_nnet_cross_valid(mrmr_tr_set, label_2, hidden_nodes=hidden_nodes,k=k))
}
#mean(AUC) according to the nb of hidden nodes 
plot(hidden_nodes_vec, results)
lines(hidden_nodes_vec, results)


## Application to the test set

model <- nnet(x=mrmr_tr_set,
              y=label_1,
              size=60,
              skip=FALSE, maxit=1000,rang=0.7,MaxNWts=10000,trace=T)

h1n1_vaccine = predict(model, mrmr_ts_set)
summary(h1n1_vaccine)
summary(label_1)
model <- nnet(x=mrmr_tr_set,
              y=label_2,
              size=60,
              skip=FALSE, maxit=1000,rang=1,MaxNWts=10000,trace=FALSE)
seasonal_vaccine = predict(model, mrmr_ts_set)
summary(seasonal_vaccine)
summary(label_2)
submi <- cbind(testing_features[, "respondent_id"], h1n1_vaccine, seasonal_vaccine)
head(submi)
colnames(submi) <- c("respondent_id", "h1n1_vaccine", "seasonal_vaccine")
write.csv(submi, "submission_nnet.csv", row.names = F)
