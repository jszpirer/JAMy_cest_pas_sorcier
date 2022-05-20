
#install.packages(praznik)
library(praznik)

test_set <- read.csv("ts_set_imputed.csv")[, -c(1)]


X <- read.csv("tr_set_imputed.csv")[, -c(1)]
Y <- read.csv("training_set_labels.csv")
summary(Y)

results <- MRMR(X[, -c(1)], Y[, c(2)], 40, positive = T) # arbitrary number of cols to keep for h1n1
results$selection
results$score

results <- MRMR(X[, -c(1)], Y[, c(3)], 40, positive = T) # arbitrary number of cols to keep for seasonal
results$selection
results$score

features_to_keep = c(3,6,7,10,11,12,13,14,15,16,17,19,20,22,28,29,30,32,36,38,41,45,46,47,55,56,66,67,70,72,77,80,87,91,94)
features_to_keep

mrmr_tr_set <- X[, features_to_keep]
mrmr_tr_set
mrmr_ts_set <- test_set[, features_to_keep]

write.csv(mrmr_tr_set, "mrmr_tr.csv", row.names = F)
write.csv(mrmr_ts_set, "mrmr_ts.csv", row.names = F)
