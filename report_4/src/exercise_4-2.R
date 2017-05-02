library(rpart)
library(caret)

M_test_dep_2_frame<-as.data.frame(M_test_dep_2)
M_train_dep_2_frame<-as.data.frame(M_train_dep_2)
M_reduced_2_frame<-as.data.frame((M_reduced_2))
r_tree<-rpart(V1 ~ ., data = M_test_dep_2_frame, method = "class")
rpart.plot::prp(r_tree, type = 3, fallen.leaves = T)
pred<-predict(r_tree, M_train_dep_2_frame, type = "class")
xtab<-table(M_train_dep_2_frame$V1, pred)
confusionMatrix(xtab)



cat("Performing cross-validation\n")
M_xval = list()
M_xval_prenorm = list()
M_xval_postnorm = list()
prenorm_res = double(10)
postnorm_res = double(10)
for (i in 1:10) {
  # Split matrix into 10 parts
  M_xval[[i]] = M_shuffled[((i-1)*nrow(M)/10+1):(i*nrow(M)/10),]
  M_xval_prenorm[[i]] = M_reduced_prenorm[((i-1)*nrow(M)/10+1):(i*nrow(M)/10),]
  M_xval_postnorm[[i]] = M_reduced_postnorm[((i-1)*nrow(M)/10+1):(i*nrow(M)/10),]
}

for (i in 1:10) {
  # Recombine 9 parts for training and keep 1 for testing
  M_xval_test = as.data.frame(M_xval[[i]])
  
  M_xval_train = as.data.frame(do.call(rbind, M_xval[-i])) # Bind all matrixes in list except the ith
  
  class_xval = rpart(V1 ~ ., data = M_xval_train, method = "class")
  
  pred<-predict(r_tree, M_xval_test, type = "class")
  
  true_class_xval_test = factor(M_xval_test$V1, levels(pred))
  success_xval = sum(true_class_xval_test == pred)/length(pred)
  print(success_xval)
  cat("Success", i, ":", success_xval, "\t")
  prenorm_res[i] = success_xval
}