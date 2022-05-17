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

# Nombre de missing values dans l'ordre
a <- sapply(training_set_features, function(x) sum(is.na(x)))
print(a[order(a, decreasing=T)])

# Quid si on regarde pas les health_insurance ?
cat("Training set : ", dim(training_set_features[,-c(16)])[1], "->", dim(na.omit(training_set_features[,-c(16)]))[1], "\n")
cat("Test set     : ", dim(test_set_features[,-c(16)])[1], "->", dim(na.omit(test_set_features[,-c(16)]))[1], "\n")
cat("Training labs: ", dim(training_set_labels)[1], "->", dim(na.omit(training_set_labels))[1])

# On change health insurance pour ajouter la donnée "na" comme étant une information
library(fastDummies)
training_set_features <- dummy_cols(training_set_features, 
                                    select_columns = "health_insurance",
                                    remove_selected_columns = T)

## Change NAs to 0 for the new health_insurance one hot encoded columns
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


###############
##### Missing values imputation
###############
# Mtn que les données ont une forme exploitable on peut gérer les missing values
# select which training set encoding to use (si on en a fait plusieurs)
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
tr_set_rf_imputed <- read.csv("tr_set_rf_imputed.csv")


### Normalize 
# install.packages("caret")
library("caret")

ss <- preProcess(as.data.frame(tr_set_rf_imputed), method=c("range"))
tr_set_rf_imputed <- predict(ss, as.data.frame(tr_set_rf_imputed))
summary(tr_set_rf_imputed)

write.csv(tr_set_rf_imputed, "tr_set_rf_imputed.csv")

