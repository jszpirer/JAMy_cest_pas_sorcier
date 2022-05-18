training_features <- read.csv("tr_set_imputed.csv")[, -c(1)]
summary(training_features)
training_labels <- read.csv("training_set_labels.csv", stringsAsFactors = T)
label_1 <- training_set_labels[,"h1n1_vaccine"]
label_2 <- training_set_labels[, "seasonal_vaccine"]
labels <- cbind(h1n1_vaccine=label_1, seasonal_vaccine=label_2)

testing_features <- read.csv("ts_set_imputed.csv")[, -c(1)]
feature_set <- rbind(training_features, testing_features)
feature_set <- feature_set[, -c(1)] # remove resp id
pca <- prcomp(feature_set)
summary(pca)

pca_feat_set <- pca$x[,1:54]

# install.packages("caret")
library("caret")

ss <- preProcess(as.data.frame(pca_feat_set), method=c("range"))
pca_feat_set <- predict(ss, as.data.frame(pca_feat_set))


pca_tr_set <- pca_feat_set[1:26707,]
pca_ts_set <- pca_feat_set[26708:53415,]
total_train_set <- cbind(pca_tr_set, label_1)
model <- lm(label_1 ~ ., data=total_train_set)


Y_pred <- predict(model,pca_ts_set)
summary(Y_pred)
ss <- preProcess(as.data.frame(Y_pred), method=c("range"))
Y_pred1 <- predict(ss, as.data.frame(Y_pred))
summary(Y_pred1)

total_train_set <- cbind(pca_tr_set, label_2)
model <- lm(label_2 ~ ., data=total_train_set)
Y_pred <- predict(model,pca_ts_set)
summary(Y_pred)
ss <- preProcess(as.data.frame(Y_pred), method=c("range"))
Y_pred2 <- predict(ss, as.data.frame(Y_pred))
summary(Y_pred2)

final_pred <- cbind(testing_features[, c(1)], Y_pred1, Y_pred2)
write.csv(final_pred, "test_submission.csv", row.names = F)

vaccine_idx <- sample(1:nrow(total_train_set))
half_split <- floor(nrow(total_train_set)/2)
train_data_set <- total_train_set[vaccine_idx[1:half_split],]
test_data <- total_train_set[vaccine_idx[(half_split+1):nrow(total_train_set)],]
target_idx <- ncol(train_data)
targets <- c(target_idx, target_idx-1)

# On peut lancer le mod?le
model <- lm(cbind(h1n1_vaccine, seasonal_vaccine)~., data=train_data_set)

Y_pred <- predict(model,test_data[,-targets])
Y_pred
dim(Y_pred)
dim(test_data[,targets])
test_data[,targets]