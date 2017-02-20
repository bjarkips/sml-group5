# Solution to exercise 1.4.4
# Created 20.02.2017
#---------------------------

# This scripts performs the same functions as required for part 1.4.1
# but sources loadImage2.r for alternate image preprocessing

source('~/SML/sml-group5/report_1/loadImage2.r')

sigma <- c(250)
print("Performing kNN classification using gaussian blur preprocessing with varying sigmas")
print("k is set to 50 and DPI to 300")
for (sigma_it in sigma) {
  cat("Loading data using sigma =", sigma_it, "\n")
  # Read data
  M = loadSinglePersonsData(300, 'group5', 1, '/home/bjarkips/SML/trunk/2017/', sigma_it)
  
  # Shuffle data
  set.seed(123)
  M_shuffled <- M[sample(nrow(M)),]
  
  # Split data into training and test sets (50/50)
  M_train <- M_shuffled[1:(nrow(M)/2),]
  M_test <- M_shuffled[(nrow(M)/2+1):nrow(M),]
  true_class <- M_shuffled[,1]
  true_class_train <- true_class[1:(nrow(M)/2)]
  true_class_test <- true_class[(nrow(M)/2+1):nrow(M)]
  
  # Perform kNN classification
  # Train and test on training set
  class_train = knn(M_train, M_train, true_class_train, 50)
  true_class_train <- factor(true_class_train, levels(class_train))
  
  # Train on training set and test on test set
  tic <- proc.time()
  class_test = knn(M_train, M_test, true_class_train, 50)
  toc <- proc.time() - tic
  print(toc)
  true_class_test <- factor(true_class_test, levels(class_test))
  
  # Show results
  success_train <- sum(true_class_train == class_train)/length(class_train)
  cat("Training set:", success_train, "\n")
  success_test <- sum(true_class_test == class_test)/length(class_test)
  cat("Test set:", success_test, "\n\n")
  
  # Perform cross-validation
  cat("Performing cross-validation", "\n")
  M_xval <- list()
  for (i in 1:10) {
    # Split matrix into 10 parts
    M_xval[[i]] <- M_shuffled[((i-1)*nrow(M)/10+1):(i*nrow(M)/10),]
  }
  for (i in 1:10) {
    # Recombine 9 parts for training and keep 1 for testing
    M_xval_test <- M_xval[[i]]
    M_xval_train <- do.call(rbind, M_xval[-i])
    true_class_xval <- M_xval_train[,1]
    class_xval = knn(M_xval_train, M_xval_test, true_class_xval, 50)
    true_class_xval <- factor(true_class_xval, levels(class_xval))
    success_xval <- sum(true_class_xval == class_xval)/length(class_xval)
    cat("Result", i, ":", success_xval, "\n")
  }
}
