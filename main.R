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
# data_preprocessed<-training_set_features[,-factor_variables]
# head(data_preprocessed)
# dim(data_preprocessed)

# First try : complete oh encoding for factors

tr_set_complete_oh <- dummy_cols(training_set_features,
                                 select_columns = factor_col_names,
                                 ignore_na = T,
                                 remove_selected_columns = T)
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

tr_set_rf_imputed <- rfImpute(training_set_labels[, c(2)] ~ ., tr_set_encoded)
# tr_set_rf_imputed <- rfImpute(tr_set_encoded, cbind(tr_set_encoded, training_set_labels))

## Third try : replace using NMF (non negative matrix factorization)
# For every column with missing value
# We select columns containing no missing values

# install.packages("NMFN")
library(NMFN)

tr_set_remove_NA<- na.omit(tr_set_useful)
N <- nrow(tr_set_remove_NA)
n <- ncol(tr_set_remove_NA)



# k fold validation of our NMF model

CV_folds <- 10
size_CV <-floor(N/CV_folds)
CV_err<-numeric(CV_folds)

for (i in 1:CV_folds) {
    idx_ts<-(((i-1)*size_CV+1):(i*size_CV))  ### idx_ts represents the indices of the test set the i-th fold
    X_ts<-X[idx_ts,]  
    Y_ts<-Y[idx_ts]  
    
    idx_tr<-setdiff(1:N,idx_ts) ### idx_tr represents  indices of the training sefor the i-th fold
    X_tr<-X[idx_tr,]
    Y_tr<-Y[idx_tr]                          
    
    DS<-cbind(X_tr,imdb_score=Y_tr)
    
    # Model fit (using lm function)
    model<- lm(imdb_score~.,DS)
    
    # Model prediction 
    Y_hat_ts<- predict(model,X_ts)
    
    # Cross validation error = Mean Squared Error
    CV_err[i]<-mean((Y_hat_ts-Y_ts)^2)
}


print(paste("CV error=",round(mean(CV_err),digits=4), " ; std dev=",round(sd(CV_err),digits=4)))

CV_err_lm_single_model <- CV_err
