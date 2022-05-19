training_features <- read.csv("tr_set_imputed.csv")[, -c(1)]
summary(training_features)
training_set_labels <- read.csv("training_set_labels.csv", stringsAsFactors = T)
label_1 <- training_set_labels[,"h1n1_vaccine"]
label_2 <- training_set_labels[, "seasonal_vaccine"]
labels <- cbind(h1n1_vaccine=label_1, seasonal_vaccine=label_2)

testing_features <- read.csv("ts_set_imputed.csv")[, -c(1)]
feature_set <- rbind(training_features, testing_features)
feature_set <- feature_set[, -c(1)] # remove resp id
pca <- prcomp(feature_set)
summary(pca)

pca_feat_set <- pca$x[,1:54]
summary(pca_feat_set)

# install.packages("caret")
library("caret")

ss <- preProcess(as.data.frame(pca_feat_set), method=c("range"))
pca_feat_set <- predict(ss, as.data.frame(pca_feat_set))


pca_tr_set <- pca_feat_set[1:26707,]
pca_ts_set <- pca_feat_set[26708:53415,]
total_train_set_h1n1 <- cbind(pca_tr_set, label_1)
total_train_set_seasonal <- cbind(pca_tr_set, label_2)

##### H1N1 PART #####
vaccine_idx <- sample(1:nrow(total_train_set_h1n1))
half_split <- floor(nrow(total_train_set_h1n1)/2)
train_data_set <- total_train_set_h1n1[vaccine_idx[1:half_split],]
summary(train_data_set)
test_data <- total_train_set_h1n1[vaccine_idx[(half_split+1):nrow(total_train_set)],]
target_idx <- ncol(train_data_set)


library(neuralnet)
n <- names(pca_tr_set) # Toujours les mêmes car on prend les 54 variables post-PCA
f <- as.formula(paste("label_1~", paste(n, collapse = " + ")))
nn <- neuralnet(f, data = train_data_set,
                hidden = c(4, 2),
                linear.output = T)
nn$response

# Plotting the graph
plot(nn)

# Il faut mettre les résultats à la même échelle pour les comparer

pr.nn <- compute(nn,test_data[,-target_idx])
pr.nn_ <- pr.nn$net.result*(max(train_data_set$label_1)-min(train_data_set$label_1))+min(train_data_set$label_1)
test.r <- (test_data$label_1)*(max(train_data_set$label_1)-min(train_data_set$label_1))+min(train_data_set$label_1)
MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_data) #Mean square error


##### SEASONAL PART #####
vaccine_idx <- sample(1:nrow(total_train_set_seasonal))
half_split <- floor(nrow(total_train_set_seasonal)/2)
train_data_set <- total_train_set_seasonal[vaccine_idx[1:half_split],]
summary(train_data_set)
test_data <- total_train_set_seasonal[vaccine_idx[(half_split+1):nrow(total_train_set)],]
target_idx <- ncol(train_data_set)


library(neuralnet)
n <- names(pca_tr_set) # Toujours les mêmes car on prend les 54 variables post-PCA
f <- as.formula(paste("label_2~", paste(n, collapse = " + ")))
f
nn <- neuralnet(f, data = train_data_set,
                hidden = c(4, 2),
                linear.output = T)
nn$response

# Plotting the graph
plot(nn)

pr.nn <- compute(nn,test_data[,-target_idx])
pr.nn_ <- pr.nn$net.result*(max(train_data_set$label_2)-min(train_data_set$label_2))+min(train_data_set$label_2)
test.r <- (test_data$label_2)*(max(train_data_set$label_2)-min(train_data_set$label_2))+min(train_data_set$label_2)
MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_data) #Mean square error