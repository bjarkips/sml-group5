library(class)

M_prenorm = (M - min(M)) / (max(M) - min(M))
M_reduced = M_PCA[['x']][,1:7]
M_PCA_prenorm = prcomp(M_prenorm, retx = TRUE)
summ = summary(M_PCA_prenorm)
imp = summ[['importance']]
M_reduced_prenorm = M_PCA_prenorm[['x']][,1:7]
M_reduced_postnorm = (M_reduced - min(M_reduced)) / (max(M_reduced) - min(M_reduced))


cat("Performing cross-validation\n")
M_xval = list()
M_xval_prenorm = list()
M_xval_postnorm = list()
for (i in 1:10) {
  # Split matrix into 10 parts
  M_xval[[i]] = M_shuffled[((i-1)*nrow(M)/10+1):(i*nrow(M)/10),]
  M_xval_prenorm[[i]] = M_reduced_prenorm[((i-1)*nrow(M)/10+1):(i*nrow(M)/10),]
  M_xval_postnorm[[i]] = M_reduced_postnorm[((i-1)*nrow(M)/10+1):(i*nrow(M)/10),]
}
for (i in 1:10) {
  # Recombine 9 parts for training and keep 1 for testing
  M_xval_test_prenorm = M_xval_prenorm[[i]]
  M_xval_test_postnorm = M_xval_postnorm[[i]]
  M_xval_train = do.call(rbind, M_xval[-i]) # Bind all matrixes in list except the ith
  true_class_xval = M_xval_train[,1]
  M_xval_train_prenorm = do.call(rbind, M_xval_prenorm[-i])
  M_xval_train_postnorm = do.call(rbind, M_xval_postnorm[-i])
  class_xval_postnorm = knn(M_xval_train_postnorm, M_xval_test_postnorm, true_class_xval, 50)
  class_xval_prenorm = knn(M_xval_train_prenorm, M_xval_test_prenorm, true_class_xval, 50)
  true_class_xval = factor(true_class_xval, levels(class_xval_postnorm))
  success_xval_postnorm = sum(true_class_xval == class_xval_postnorm)/length(class_xval_postnorm)
  success_xval_prenorm = sum(true_class_xval == class_xval_prenorm)/length(class_xval_prenorm)
  cat("Prenorm", i, ":", success_xval_prenorm, "\t")
  cat("Postnorm", i, ":", success_xval_postnorm, "\n")
}