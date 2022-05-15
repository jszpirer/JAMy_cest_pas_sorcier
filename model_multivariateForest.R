#install.packages("MultivariateRandomForest")
training_features <- read.csv("tr_set_rf_imputed.csv")
training_labels <- read.csv("training_set_labels.csv", stringsAsFactors = T)
label_1 <- training_set_labels[,"h1n1_vaccine"]
label_2 <- training_set_labels[, "seasonal_vaccine"]
labels <- cbind(h1n1_vaccine=label_1, seasonal_vaccine=label_2)
total_train_set <- cbind(training_features, labels)
# Il faut un set de test pour random forest donc on coupe en deux pour le moment

vaccine_idx <- sample(1:nrow(total_train_set))
half_split <- floor(nrow(total_train_set)/2)
train_data <- total_train_set[vaccine_idx[1:half_split],]
test_data <- total_train_set[vaccine_idx[(half_split+1):nrow(total_train_set)],]

#On peut commencer la multivariate random forest
library(MultivariateRandomForest)
#Input and Output Feature Matrix of random data (created using runif)
n_tree=2
m_feature=5
min_leaf=5
#Prediction size is 10 x 5, where 10 is the number
#of testing samples and 5 is the number of output features

target_idx <- ncol(train_data)
targets <- c(target_idx, target_idx-1)
trainX <- as.matrix(train_data[,-targets])
trainY <- as.matrix(train_data[,targets])
testX <- as.matrix(test_data[,-targets])


Prediction <- build_forest_predict(trainX, trainY, n_tree, m_feature, min_leaf,testX)
Prediction
# On met un threshold sur la prédiction pour avoir des 0 et des 1
testY <- ifelse(Prediction > 0.5, 1, 0)
confusion_matrix <- table(testY,test_data[,targets])
confusion_matrix
