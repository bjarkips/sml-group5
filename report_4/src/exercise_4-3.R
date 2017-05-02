require(randomForest)
set.seed(123)

M_train_dep_2.df = data.frame(M_train_dep_2)
rf = randomForest(X1~., M_train_dep_2.df)

pred = predict(rf, data.frame(M_train_dep_2))
sum(round(pred) == M_test_dep_2[,1]) / length(M_test_dep_2[,1])


cat("Performing cross-validation\n")
M_xval = list()
M_xval_prenorm_rf = list()
M_xval_postnorm = list()
prenorm_res_rf = double(10)
postnorm_res = double(10)

for (i in 1:10) {
  # Split matrix into 10 parts
  M_xval[[i]] = M_shuffled[((i-1)*nrow(M)/10+1):(i*nrow(M)/10),]
  M_xval_prenorm_rf[[i]] = M_reduced_prenorm[((i-1)*nrow(M)/10+1):(i*nrow(M)/10),]
  M_xval_postnorm[[i]] = M_reduced_postnorm[((i-1)*nrow(M)/10+1):(i*nrow(M)/10),]
}

for (i in 1:10) {
  # Recombine 9 parts for training and keep 1 for testing
  M_xval_test = as.data.frame(M_xval[[i]])
  
  M_xval_train = as.data.frame(do.call(rbind, M_xval[-i])) # Bind all matrixes in list except the ith
  
  class_xval_rf = randomForest(V1 ~ ., data = M_xval_train)
  
  pred_rf<-predict(class_xval_rf, M_xval_test)
  
  true_class_xval_test_rf = factor(M_xval_test$V1, levels(pred_rf))
  success_xval_rf = sum(true_class_xval_test == pred_rf)/length(pred_rf)
  print(success_xval_rf)
  cat("Success", i, ":", success_xval_rf, "\t")
  prenorm_res_rf[i] = success_xval
  
}
#aa

hist(prenorm_res, xlim = c(.35, .5), main = 'Cross Validation', xlab = 'Accuracy')
abline(v = mean(prenorm_res),
       col = "royalblue",
       lwd = 2)
abline(v = mean(prenorm_res)+ sd(prenorm_res), col = 'red', lwd = 2)
abline(v = mean(prenorm_res)- sd(prenorm_res), col = 'red', lwd = 2)
legend(x = "topright", # location of legend within plot area
       c("Mean", "Std. Dev."),
       col = c("royalblue", "red"),
       lwd = c(2, 2))