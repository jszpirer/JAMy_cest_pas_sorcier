# Training set features
training_set_features <- read.csv("training_set_features.csv", stringsAsFactors = T)
dim(training_set_features)

# Test set features
test_set_features <- read.csv("test_set_features.csv", stringsAsFactors = T)
dim(test_set_features)

# Training set labels
training_set_labels <- read.csv("training_set_labels.csv", stringsAsFactors = T)
dim(training_set_labels)

#####################################
###   Missing value imputation    ###
#####################################
## Data exploration

summary(training_set_features)

cat("Training set : ", dim(training_set_features)[1], "->", dim(na.omit(training_set_features))[1], "\n")
cat("Test set     : ", dim(test_set_features)[1], "->", dim(na.omit(test_set_features))[1], "\n")
cat("Training labs: ", dim(training_set_labels)[1], "->", dim(na.omit(training_set_labels))[1])


a <- sapply(training_set_features, function(x) sum(is.na(x)))
print(a[order(a, decreasing=T)])

cat("Training set : ", dim(training_set_features[,-c(16)])[1], "->", dim(na.omit(training_set_features[,-c(16)]))[1], "\n")
cat("Test set     : ", dim(test_set_features[,-c(16)])[1], "->", dim(na.omit(test_set_features[,-c(16)]))[1], "\n")
cat("Training labs: ", dim(training_set_labels)[1], "->", dim(na.omit(training_set_labels))[1])

library(fastDummies)
training_set_features <- dummy_cols(training_set_features, 
                                    select_columns = "health_insurance",
                                    remove_selected_columns = T)

## Change NAs to 0
replace_na_with_0<-function(vec) {
    vec[is.na(vec)]<-0
    vec
}

training_set_features[,"health_insurance_0"] <- replace_na_with_0(training_set_features[,"health_insurance_0"])
training_set_features[,"health_insurance_1"] <- replace_na_with_0(training_set_features[,"health_insurance_1"])


### Managing categorical variables
sapply(training_set_features[1,],class)

dim(training_set_features)
factor_variables<-which(sapply(training_set_features[1,],class)=="factor")
factor_variables
factor_col_names <- colnames(factor_variables)
data_factor<-training_set_features[,factor_variables]
dim(data_factor)

# First try : complete oh encoding for factors

tr_set_complete_oh <- dummy_cols(training_set_features,
                                 select_columns = factor_col_names,
                                 ignore_na = T,
                                 remove_selected_columns = T)
dim(tr_set_complete_oh)
# 115 features, seems ok for now but might have drawbacks
# -> mal fait, on a besoin d'utiliser split=',' sur les household_incomes
# Attention, peut pas utiliser split=',' sur census_msa

# Second try : use integer encoding on age_group and education 
# Todo

### Missing values imputation

# select which training set encoding to use
tr_set_encoded <- tr_set_complete_oh

## First try : replace with mean or frequent value

# install.packages("DescTools")
library("DescTools") # For Mode function

replace_na_with_mean_value<-function(vec) {
    mean_vec <- mean(as.numeric(vec), na.rm=TRUE)
    vec[is.na(vec)]<-mean_vec
    vec
}

replace_na_with_mode<-function(vec) {
    mode_vec <- Mode(vec, na.rm = TRUE)
    vec[is.na(vec)]<-mode_vec
    vec
}

tr_set_noNA_mode<-data.frame(apply(tr_set_encoded, MARGIN=2, replace_na_with_mode))
summary(tr_set_noNA_mode)


## Second try : replace using RF rfImpute method

# tr_set_rf_imputed <- rfImpute(training_set_labels[, c(2)] ~ ., tr_set_encoded)
# tr_set_rf_imputed <- rfImpute(tr_set_encoded, merge(tr_set_encoded, training_set_labels, by.y = "respondent_id"))

tr_set_rf_imputed <- read.csv("tr_set_rf_imputed.csv")

install.packages("caret")
library("caret")

# normalizing data
ss <- preProcess(as.data.frame(tr_set_rf_imputed), method=c("range"))
gfg <- predict(ss, as.data.frame(tr_set_rf_imputed))
gfg


#### Random forests

library(randomForest)

N <- nrow(X) # Nb of training examples
n <- ncol(X) # Nb of input variables

shuff_idx <- sample(1:N)
half_split <- floor(N/2) # Splitting data for training / testing

X <- tr_set_rf_imputed[,-c(1)]  # Remove respond_id of features
Y <- training_set_labels[,-c(1)] # Work on h1n1 vaccine

X_tr <- X[shuff_idx[1:half_split],]
Y_tr <- Y[shuff_idx[1:half_split],]

X_ts <- X[shuff_idx[(half_split+1):N],]
Y_ts <- Y[shuff_idx[(half_split+1):N],]

n_trees <- 10
accuracy_vec <- matrix(0, n_trees, 4)
threshold = 0.9  # Arbitrary

for (i in 10:n_trees) {
    print(paste("Model for", i, "trees"))
    # Model fit (using randomForest function)
    
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
    # 
    Y1_hat = model_h1n1$test$predicted
    # Y1_hat <- ifelse(Y1_hat > threshold, 1, 0) 
    # 
    # confusion_1 <- table(Y1_hat, Y_ts[,c(1)])
    h1n1_hat = (model_sum$test$predicted - model_diff$test$predicted)/2
    # h1n1_hat <- ifelse(h1n1_hat > threshold, 1, 0) 
    # confusion_h1n1 <- table(h1n1_hat, Y_ts[,c(1)])
    # 
    Y2_hat = model_season$test$predicted
    # Y2_hat <- ifelse(Y2_hat > threshold, 1, 0) 
    # 
    # confusion_2 <- table(Y2_hat, Y_ts[,c(2)])
    seasonal_hat = (model_sum$test$predicted + model_diff$test$predicted)/2
    # seasonal_hat <- ifelse(seasonal_hat > threshold, 1, 0) 
    # confusion_seasonal <- table(seasonal_hat, Y_ts[,c(2)])
    # 
    # # Accuracy of h1n1 
    # accuracy_vec[i, 1] = (confusion_h1n1[1,1] + confusion_h1n1[2,2]) / sum(confusion_h1n1)
    # accuracy_vec[i, 2] = (confusion_1[1,1] + confusion_1[2,2]) / sum(confusion_1)
    # # Accuracy of seasonal
    # accuracy_vec[i, 3] = (confusion_seasonal[1,1] + confusion_seasonal[2,2]) / sum(confusion_seasonal)
    # accuracy_vec[i, 4] = (confusion_2[1,1] + confusion_2[2,2]) / sum(confusion_2)
    # 
}

par(mfrow=c(1,2))
plot(accuracy_vec[,1],main = "Number of trees influence on h1n1, mixed meth",xlab = "Nbr of trees",ylab = "Classification rate") 
plot(accuracy_vec[,2],main = "Number of trees influence on h1n1, classic",xlab = "Nbr of trees",ylab = "Classification rate") 
confusion_1
par(mfrow=c(1,2))

plot(accuracy_vec[,3],main = "Number of trees influence on seasonal, mixed",xlab = "Nbr of trees",ylab = "Classification rate") 
plot(accuracy_vec[,4],main = "Number of trees influence on seasonal, class",xlab = "Nbr of trees",ylab = "Classification rate")
confusion_2

thresholds <- seq(0,0.99,0.05)
FPR <- c()
TPR <- c()
Y_pred <- Y1_hat
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

plot(FPR,TPR)
lines(FPR,TPR,col="blue")
lines(thresholds,thresholds,lty=2)
title("ROC Curve")
AUC <- sum(abs(diff(FPR)) * (head(TPR,-1)+tail(TPR,-1)))/2
AUC

