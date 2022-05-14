#### Utilise pour le moment random forests mais va devenir nnet 
training_set_labels <- read.csv("training_set_labels.csv", stringsAsFactors = T)
dim(training_set_labels)

tr_set_rf_imputed <- read.csv("tr_set_rf_imputed.csv")
dim(tr_set_rf_imputed)

## Fonctions venant du TP
accuracyFromConfusionMatrix <- function(confusion_matrix){
    return((confusion_matrix[1,1]+confusion_matrix[2,2])/sum(confusion_matrix))
}


computeResults <- function(Y,Y_hat,explanatory_string="",verbose=TRUE){
    confusion_matrix <- table(Y_hat,Y)
    if(dim(confusion_matrix)[1] < 2){ # Their is a possibility of having no spam/non spam elements in Y_hat -> we make the matrix the right size
        if(rownames(confusion_matrix)[1] == 0){
            confusion_matrix <- rbind(confusion_matrix,c(0,0))
            rownames(confusion_matrix)[2] <- 1
        }
        else if(rownames(confusion_matrix)[1] == 1){
            confusion_matrix <- rbind(c(0,0),confusion_matrix)
            rownames(confusion_matrix)[1] <- 0
        }
    }
    accuracy <- accuracyFromConfusionMatrix(confusion_matrix)
    misclassification_rate <- 1 - accuracy
    
    if(verbose){
        print(paste("[INFO] - Confusion matrix",explanatory_string,":"))
        print(confusion_matrix)
        print(paste("[INFO] - Accuracy",explanatory_string,":",accuracy))
        print(paste("[INFO] - Misclassification rate",explanatory_string,":",misclassification_rate))
    }
    
    return(accuracy)
}


### Fin des fonctions venant directement tu TP


compute_fpr_tpr <- function(model, test_data, target_variable) {
    thresholds <- seq(0,0.99,0.05)
    FPR <- c()
    TPR <- c()
    Y_pred<-predict(model,test_data[,-target_variable])
    Y <- test_data[,target_variable]
    
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
    fpr_tpr <- rbind(FPR, TPR)
    return (fpr_tpr)
}


crossValidationInOutSample <- function(hidden_nodes=10,k=5,threshold=0.5, target=0){
    # Targe 0 for h1n1, 1 for seasonal
    accuracy_vec <- c()
    
    data_set <- tr_set_rf_imputed[, -c(1)]
    vaccine <- training_set_labels[,c(2+target)]
    
    data_set <- cbind(data_set, vaccine)
    
    # 1. Shuffle the dataset randomly.
    shuffled_idx <- sample(1:nrow(data_set))
    half_split <- floor(nrow(data_set)/2)
    target_variable <- ncol(data_set)
    target_name = colnames(data_set)[target_variable]
    
    # 2. Split the dataset into k groups
    max <- ceiling(nrow(data_set)/k)
    splits <- split(shuffled_idx, ceiling(seq_along(shuffled_idx)/max))
    
    # 3. For each unique group:
    for (i in 1:k){
        #3.1 Take the group as a hold out or test data set
        test_data <- data_set[splits[[i]],]
        
        #3.2 Take the remaining groups as a training data set
        train_data <- data_set[-splits[[i]],]
        print(paste("[INFO] - Training set size:",dim(train_data)[1],"- Testing set size",dim(test_data)[1]))
        
        #3.3 Fit a model on the training set and evaluate it on the test set
        model <- nnet(x=train_data[,-c(target_variable)],
                                 y=train_data[, c(target_variable)],
                                 size=hidden_nodes,
                                 skip=FALSE, maxit=3000,rang=0.2,MaxNWts=30000,trace=FALSE)
        
        Y_pred<-predict(model,test_data[,-target_variable])
        Y <- matrix(test_data[,target_variable])
        Y_hat <- Y_pred
        
        #3.4 Retain the evaluation score and discard the model
        # TODO : compute roc auc
        FPR_TPR = compute_fpr_tpr(model, test_data, target_variable)
        FPR <- FPR_TPR[1,]
        TPR <- FPR_TPR[2,]
        FPR <- c(1, FPR, 0)
        TPR <- c(1, TPR, 0)
        # print(FPR_TPR)
        # plot(FPR,TPR)
        # lines(FPR,TPR,col="blue")
        # lines(thresholds,thresholds,lty=2)
        # title("ROC Curve")

        AUC <- sum(abs(diff(FPR)) * (head(TPR,-1)+tail(TPR,-1)))/2
        accuracy_vec[i] <- AUC
    }
    
    return(mean(accuracy_vec))
}


k <- 4
# hidden_nodes_vec <- c(3,5,10,15,30,50,100) # Testing up to 100 hidden nodes, takes some time to train
hidden_nodes_vec <- seq(95,115,2)
# hidden_nodes_vec <- c(90)#, 31, 33, 50)

results <- c()
for(hidden_nodes in hidden_nodes_vec){
    print(paste("[INFO] - Testing h=",hidden_nodes))
    results <- c(results, crossValidationInOutSample(data_set, hidden_nodes=hidden_nodes,k=k))
}
# mean(AUC) according to the nb of hidden nodes 
plot(hidden_nodes_vec, results)
lines(hidden_nodes_vec, results)
