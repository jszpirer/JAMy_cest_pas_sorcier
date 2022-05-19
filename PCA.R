#opening files
data_preprocessed <- read.csv("tr_set_imputed.csv", stringsAsFactors = T)
data_preprocessed <- data_preprocessed[, -c(1,2)]  # Remove respondant id and automatic added column
dim(data_preprocessed)


training_set_labels <- read.csv("training_set_labels.csv", stringsAsFactors = T)
dim(training_set_labels)

#setting parameters
CV_folds <- 10
N<-nrow(data_preprocessed)
n<-ncol(data_preprocessed)

size_CV <-floor(N/CV_folds)

CV_err<-matrix(0,nrow=n,ncol=CV_folds)

Y<- training_set_labels[,2]

# Compute PCA 
X_pca<-data.frame(prcomp(data_preprocessed,retx=T)$x)

for (i in 1:CV_folds) {
    print(paste("Fold number", i))
  idx_ts<-(((i-1)*size_CV+1):(i*size_CV))  ### idx_ts represents the indices of the test set the i-th fold
  X_ts<-X_pca[idx_ts,]
  Y_ts<-Y[idx_ts]
  
  idx_tr<-setdiff(1:N,idx_ts) ### idx_tr represents  indices of the training sefor the i-th fold
  X_tr<-X_pca[idx_tr,]
  Y_tr<-Y[idx_tr]
  
  for (nb_components in 1:n) {
    # Create a dataset including only the first nb_components principal components
    DS<-cbind(X_tr[,1:nb_components,drop=F],vacc_status=Y_tr)
    
    # Model fit (using lm function)
    model<- lm(vacc_status~.,DS)
    
    # Model predict
    Y_hat_ts<- predict(model,X_ts[,1:nb_components,drop=F])
    
    CV_err[nb_components,i]<-mean((Y_hat_ts-Y_ts)^2)
  }
}  

CV_error_mean_2 <- round(apply(CV_err,1,mean),digits=4)


features_to_keep_2<-c()
features_to_throw_2<-c()
for (i in 2:n){
  a<-CV_error_mean_2 [i-1]-CV_error_mean_2[i]
  if (a > 0.0005){
    features_to_keep_2<- c(features_to_keep_2, i)
  }
  else{
    features_to_throw_2<-c(features_to_throw_2, i)
  }
}

colnames(data_preprocessed[,features_to_keep_2])
# This indexes are the ones of the features from the dataset after removing 2 cols
features_to_keep_2 <- features_to_keep_2 + 2
tr_set <- read.csv("tr_set_imputed.csv", stringsAsFactors = T)

colnames(tr_set[, features_to_keep_2])
#select only the interresting columns for the output 2 and save it
data_preprocessed_2<-subset(tr_set, select = c(2, features_to_keep_2))
dim(data_preprocessed_2)
colnames(data_preprocessed_2)
write.csv(data_preprocessed_2, "tr_set_preprocessed_2.csv")

ts_set <- read.csv("ts_set_imputed.csv", stringsAsFactors = T)
ts_set_selected<-subset(ts_set, select = c(2, features_to_keep_2))

write.csv(ts_set_selected, "ts_set_preprocessed_2.csv")




Y<- training_set_labels[,3]

# Compute PCA 
X_pca<-data.frame(prcomp(data_preprocessed,retx=T)$x)

for (i in 1:CV_folds) {
  
  idx_ts<-(((i-1)*size_CV+1):(i*size_CV))  ### idx_ts represents the indices of the test set the i-th fold
  X_ts<-X_pca[idx_ts,]
  Y_ts<-Y[idx_ts]
  
  idx_tr<-setdiff(1:N,idx_ts) ### idx_tr represents  indices of the training sefor the i-th fold
  X_tr<-X_pca[idx_tr,]
  Y_tr<-Y[idx_tr]
  
  for (nb_components in 1:n) {
    # Create a dataset including only the first nb_components principal components
    DS<-cbind(X_tr[,1:nb_components,drop=F],imdb_score=Y_tr)
    
    # Model fit (using lm function)
    model<- lm(imdb_score~.,DS)
    
    # Model predict
    Y_hat_ts<- predict(model,X_ts[,1:nb_components,drop=F])
    #find the error
    CV_err[nb_components,i]<-mean((Y_hat_ts-Y_ts)^2)
  }
}  
CV_error_mean_3 <- round(apply(CV_err,1,mean),digits=4)
CV_error_mean_3
features_to_keep_3<-c(1)
features_to_throw_3<-c()
for (i in 2:n){
  a<-CV_error_mean_3 [i-1]-CV_error_mean_3[i]
  print(paste(a))
  if (a > 0.0005){
    features_to_keep_3<- c(features_to_keep_3, i)
  }
  else{
    features_to_throw_3<-c(features_to_throw_3, i)
  }
}

features_to_throw_3

#select only the interresting columns for the output 3 and save it
data_preprocessed_3<-subset(data_preprocessed, select = -c(features_to_throw_3))
dim(data_preprocessed_3)
S
write.csv(data_preprocessed_3, "tr_set_preprocessed_3")

features_to_keep<-c(1)
features_to_throw<-c()
for (i in 2:n){
  a<-CV_error_mean_3 [i-1]-CV_error_mean_3[i]
  b<-CV_error_mean_2 [i-1]-CV_error_mean_2[i]
  if ((0.5*a+0.5*b) > 0.0005){
    features_to_keep<- c(features_to_keep, i)
  }
  else{
    features_to_throw<-c(features_to_throw, i)
  }
}

features_to_throw

#select only the interresting columns for the output 3 and save it
data_preprocessed_tot<-subset(data_preprocessed, select = -c(features_to_throw))
dim(data_preprocessed_tot)

write.csv(data_preprocessed_tot, "tr_set_preprocessed.csv")
