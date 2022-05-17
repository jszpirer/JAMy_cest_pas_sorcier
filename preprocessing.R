# Training set features
training_set_features <- read.csv("training_set_features.csv", stringsAsFactors = T, na.strings = c("NA", ""))
dim(training_set_features)

# Test set features
test_set_features <- read.csv("test_set_features.csv", stringsAsFactors = T, na.strings = c("NA", ""))
dim(test_set_features)

# Training set labels
training_set_labels <- read.csv("training_set_labels.csv", stringsAsFactors = T, na.strings = c("NA", ""))
dim(training_set_labels)

#####################################
###   Missing value imputation    ###
#####################################
## Data exploration

# summary(training_set_features)

cat("Training set : ", dim(training_set_features)[1], "->", dim(na.omit(training_set_features))[1], "\n")
cat("Test set     : ", dim(test_set_features)[1], "->", dim(na.omit(test_set_features))[1], "\n")
cat("Training labs: ", dim(training_set_labels)[1], "->", dim(na.omit(training_set_labels))[1])

# Nombre de missing values dans l'ordre
a <- sapply(training_set_features, function(x) sum(is.na(x)))
print(a[order(a, decreasing=T)])
a <- sapply(test_set_features, function(x) sum(is.na(x)))
print(a[order(a, decreasing=T)])


# Quid si on regarde pas les health_insurance ?
useful_nas <- which(sapply(test_set_features, function(x) sum(is.na(x))) > 10000)

cat("Training set : ",
    dim(training_set_features[,-useful_nas])[1], "->",
    dim(na.omit(training_set_features[,-useful_nas]))[1], "\n")
cat("Test set     : ", dim(test_set_features[,-useful_nas])[1], "->",
    dim(na.omit(test_set_features[,-useful_nas]))[1], "\n")
cat("Training labs: ", dim(training_set_labels)[1], "->", dim(na.omit(training_set_labels))[1])

### On va gérer ça en même temps que les categorical variables en faisant un one hot encoding avec NA comme possibilité


# Apd mtn on gere les missing values pour le tr et ts en un seul ds
#### On constate que certaines catégories auraient pu être encodées par des nombres 
# car il y a une progression logique$

# Pour pouvoir les reséparer plus tard
tr_indexes <- training_set_features[, "respondent_id"]
ts_indexes <- test_set_features[, "respondent_id"]

features_set <- rbind(training_set_features, test_set_features)

levels(features_set[, "age_group"])
levels(features_set[, "age_group"]) <- 0:4
features_set[, "age_group"] <- as.numeric(features_set[, "age_group"])
features_set[, "age_group"] <- (features_set[, "age_group"] - 1)/4

levels(features_set[, "education"])
levels(features_set[, "education"]) <- 0:3
features_set[, "education"] <- as.numeric(features_set[, "education"])
features_set[, "education"] <- (features_set[, "education"] - 1)/3

levels(features_set[, "income_poverty"]) <- c(1, 2, 0)
features_set[, "income_poverty"] <- as.numeric(features_set[, "income_poverty"])
features_set[, "income_poverty"] <- (features_set[, "income_poverty"] - 1)/2

levels(features_set[, "census_msa"]) <- c(1, 2, 0)
features_set[, "census_msa"] <- as.numeric(features_set[, "census_msa"])
features_set[, "census_msa"] <- (features_set[, "census_msa"] - 1)/2



#####################################################################
########### Missing values of numerical variables
# Using a nmf model to predict NAs 
# based on https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8510447/
# and page 3 of https://arxiv.org/pdf/2001.00563.pdf
library(NMFN)

# We work only on the columns of numeric type
numeric_variables_idx<-which(sapply(features_set[1,],class)!="factor")
numeric_variables_idx <- numeric_variables_idx[-c(1, 16)] # Remove resp ID and health insur

summary(features_set[, numeric_variables_idx])
# We need to normalize this data to use efficient nNMF

library("caret")

ss <- preProcess(as.data.frame(features_set[, numeric_variables_idx]), method=c("range"))
features_set[,numeric_variables_idx] <- predict(ss, as.data.frame(features_set[, numeric_variables_idx]))
summary(features_set[, numeric_variables_idx])


# Define the function that will compute nmf
nfm_mult_upd <- function(R, K, missing_idx, maxit=800, eps=2.2204e-16) {
    # Using weighted multiplicative rule Zhu 2016
    # init random W and H
    print(paste("[INFO] : NMF with k=", K))
    R <- as.matrix(R)
    I <- dim(R)[1]
    J <- dim(R)[2]
    M <- matrix(1, nrow = dim(R)[1], ncol = dim(R)[2])
    M[missing_idx] <- 0
    X <- R # Store original R
    R <- R*M
    W <- matrix(runif(I*K), nrow = I, ncol = K)
    H <- matrix(runif(K*J), nrow = K, ncol = J)
    
    n <- 0
    d1 <- 1000
    d2 <- 1000
    while(n < maxit && !(d1 < eps && d2 < eps)) {
        if (n %% 100 == 0) {
            print(paste("[INFO] : iter", n, " Relative error is :", distance2(X, W%*%H)/distance2(X, R*0)))
        }
        newH <- H* (t(W) %*% R) / (t(W) %*% W %*% H)
        newW <- W*(R %*% t(newH)) / ((W %*% newH) %*% t(newH))
        
        d1 <- distance2(newH, H)
        d2 <- distance2(newW, W)
        
        H <- newH
        W <- newW
        n <- n+1
    }
    
    Res <- W%*%H
    #Res[missing_idx] <- X[missing_idx]
    nmf <- list("res"=Res, "dst"=distance2(R, Res)/distance2(R, 0))
    return(nmf)
}


# 1: initialize by defining N and replace NAs by mean
N <- 14

replace_na_with_mean_value<-function(vec) {
    mean_vec <- mean(as.numeric(vec), na.rm=TRUE)
    vec[is.na(vec)]<-mean_vec
    vec
}

X<-data.frame(apply(features_set[, numeric_variables_idx], MARGIN=2, replace_na_with_mean_value))
miss_idx = which(is.na(features_set[,numeric_variables_idx]), arr.ind = T)
non_miss_idx <- which(!is.na(features_set[,numeric_variables_idx]), arr.ind = T)

# X <- log(X+1, base = 10)

# 2: nNMF for k=k1 to K=kN with masking missing values
library(Matrix) # For rankMatrix 
rX <- rankMatrix(X)
k1 = floor(max(abs(rX - N/2), 1))
kN = min(abs(rX + N/2), dim(X)[1], dim(X)[2])

X_hat = array(0, dim = c(kN-k1+1, nrow(X), ncol(X)))
dim(X_hat)
for (K in k1:kN) {
    # compute NMF
    nmf <- nfm_mult_upd(X, K, missing_idx = miss_idx, maxit=800)
    print(paste("Computed nmf, final dist is", nmf$dst))
    X_hat[K-k1+1,,] <- nmf$res
}


# 3: weighted reconstruction
d = array(0, dim = c(kN-k1+1))
for (K in 1:(kN-k1+1)) {
    # Reconstruction error based on non missing values
    d[K] <- sum(abs(X_hat[K,,][non_miss_idx] - X[non_miss_idx]))/nrow(non_miss_idx)
}

X_hat_f <- matrix(0, nrow = nrow(X), ncol = ncol(X))
denum <- 0
for (K in 1:(kN-k1+1)) {
    # Reconstruction matrix
    X_hat_f <- X_hat_f + exp(-d[K])*X_hat[K,,]
    denum <- denum + exp(-d[K])
}
X_hat_f <- X_hat_f / sum(exp(-d))

print(paste("[INFO] : Relative error is :", distance2(X, X_hat_f)/distance2(X, X*0)))
print(paste("[INFO] : Relative error on missing data mean imputed is :",
            distance2(X[miss_idx], X_hat_f[miss_idx])/distance2(X[miss_idx], X[miss_idx]*0)))
head(X_hat_f[miss_idx])
head(X[miss_idx])

X[miss_idx] <- X_hat_f[miss_idx]
features_set[, numeric_variables_idx] <- X

####################################################################

# install.packages("missForest")
library(missForest)

X2 <- features_set[, numeric_variables_idx]
results <- missForest(X2,
           maxiter=5,
           variablewise=T,
           verbose=T)


##################################################################
### Managing categorical variables
#sapply(training_set_features[1,],class)


# First try : complete oh encoding for factors
# On considère pour toutes les features de type factor que missing value est une info, on va impute que les values num
# tr_set_complete_oh <- dummy_cols(training_set_features,
#                                  select_columns = factor_col_names,
#                                  ignore_na = F,
#                                  remove_selected_columns = T)
# 
# ts_set_complete_oh <- dummy_cols(test_set_features,
#                                  select_columns = factor_col_names,
#                                  ignore_na = F,
#                                  remove_selected_columns = T)
# 
# dim(ts_set_complete_oh) 
# 115 features, seems ok for now but might have drawbacks
# -> mal fait, on a besoin d'utiliser split=',' sur les household_incomes
# Attention, peut pas utiliser split=',' sur census_msa

# On change les trois indices avec bcp de na pour ajouter la donnée "na" comme étant une information
useful_nas <- which(sapply(test_set_features, function(x) sum(is.na(x))) > 10000)

library(fastDummies)
features_set <- dummy_cols(features_set, 
                           select_columns = names(useful_nas),
                           remove_selected_columns = T)


dim(features_set)
# note : du coup on a déjà géré les factors pour empl industry et empl status


## Change NAs to 0 for the new health_insurance one hot encoded columns
replace_na_with_0<-function(vec) {
    vec[is.na(vec)]<-0
    vec
}

useful_nas_feat_idx <- c(grep("health_insurance_*", colnames(features_set)))
useful_nas_feat_idx <- c(useful_nas_feat_idx, grep("employment_industry*", colnames(features_set)))
useful_nas_feat_idx <- c(useful_nas_feat_idx, grep("employment_occup*", colnames(features_set)))

features_set[,useful_nas_feat_idx] <- replace_na_with_0(features_set[,useful_nas_feat_idx])


###### O-H encoding of remaining categorical variables

dim(features_set)
factor_variables<-which(sapply(features_set[1,],class)=="factor")
factor_variables
factor_col_names <- colnames(factor_variables)

features_set_int_encoded <- dummy_cols(features_set,
                                       select_columns = factor_col_names,
                                       ignore_na = F,
                                       remove_selected_columns = T)


features_set_int_encoded <- replace_na_with_0(features_set_int_encoded)

tr_set_enc <- features_set_int_encoded[tr_indexes,]
ts_set_enc <- features_set_int_encoded[ts_indexes,]


summary(features_set_int_encoded)

dim(tr_set_enc)
dim(ts_set_enc)

write.csv(tr_set_enc, "tr_set_imputed.csv")
write.csv(ts_set_enc, "ts_set_imputed.csv")

### Normalize -> not needed anymore since done above
# install.packages("caret")
# library("caret")
# 
# ss <- preProcess(as.data.frame(tr_set_rf_imputed), method=c("range"))
# tr_set_rf_imputed <- predict(ss, as.data.frame(tr_set_rf_imputed))
# summary(tr_set_rf_imputed)
# 
# 
# write.csv(tr_set_rf_imputed, "tr_set_rf_imputed.csv")


####################### OLD VERSION ############################################
##############
##### Missing values imputation
###############
# Mtn que les données ont une forme exploitable on peut gérer les missing values
# select which training set encoding to use (si on en a fait plusieurs)
# tr_set_encoded <- tr_set_int_encoding
# ts_set_encoded <- ts_set_int_encoding

## First try : replace with mean or frequent value

# install.packages("DescTools")
# library("DescTools") # For Mode function
# 
# 
# 
# replace_na_with_mode<-function(vec) {
#     mode_vec <- Mode(vec, na.rm = TRUE)
#     vec[is.na(vec)]<-mode_vec
#     vec
# }
# 
# tr_set_noNA_mode<-data.frame(apply(tr_set_encoded, MARGIN=2, replace_na_with_mode))
# summary(tr_set_noNA_mode)
# 
# 
# ## Second try : replace using RF rfImpute method
# library(DescTools)
# 
# # tr_set_rf_imputed <- rfImpute(training_set_labels[, c(2)] ~ ., tr_set_encoded)
# 
# 
# which(apply(is.na(ts_set_int_encoding), 2, sum)==0)
# ts_set_rf_imputed <- rfImpute(test[, c(2)] ~ ., ts_set_encoded)
# tr_set_rf_imputed <- read.csv("tr_set_rf_imputed.csv")


### Normalize 
# install.packages("caret")
# library("caret")
# 
# ss <- preProcess(as.data.frame(tr_set_rf_imputed), method=c("range"))
# tr_set_rf_imputed <- predict(ss, as.data.frame(tr_set_rf_imputed))
# summary(tr_set_rf_imputed)
# 
# 
# write.csv(tr_set_rf_imputed, "tr_set_rf_imputed.csv")
