
#install.packages(praznik)
library(praznik)

test_set <- read.csv("ts_set_imputed.csv")[, -c(1)]

X <- read.csv("tr_set_imputed.csv")[, -c(1)]
Y <- read.csv("training_set_labels.csv")

results <- MRMR(X[, -c(1)], Y[, c(2)], 40, positive = T) # arbitrary number of cols to keep
results$selection
results$score

results <- MRMR(X[, -c(1)], Y[, c(3)], 40, positive = T) # arbitrary number of cols to keep
results$selection
length(results$score)

mrmr_tr_set <- X[, results$selection]
mrmr_ts_set <- test_set[, results$selection]

write.csv(mrmr_tr_set, "mrmr_tr.csv", row.names = F)
write.csv(mrmr_ts_set, "mrmr_ts.csv", row.names = F)