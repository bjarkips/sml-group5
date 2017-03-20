# Solution to exercises 1.4.1-1.4.3
# Created 17.02.2017
#----------------------------------

source('~/SML/trunk/Basefolder/loadImage.R')

# Iterate through values for k and DPI
DPI <- c(100, 200, 300)
k <- c(1, 10, 25, 50, 100)

for (DPI_it in DPI) {
  for (k_it in k) {
    # Read data
    if(!exists("M")) {
      M = loadSinglePersonsData(DPI_it, 'group5', 1, '/home/bjarkips/SML/trunk/2017/')
    }
    
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
    class_train = knn(M_train, M_train, true_class_train, k_it)
    true_class_train <- factor(true_class_train, levels(class_train))
    
    # Train on training set and test on test set
    tic <- proc.time()
    class_test = knn(M_train, M_test, true_class_train, k_it)
    toc <- proc.time() - tic
    cat("k =", k_it, ", ")
    cat("DPI =", DPI_it, "\n")
    print(toc)
    true_class_test <- factor(true_class_test, levels(class_test))
    
    # Show results
    success_train <- sum(true_class_train == class_train)/length(class_train)
    cat("Training set:", success_train, "\n")
    success_test <- sum(true_class_test == class_test)/length(class_test)
    cat("Test set:", success_test, "\n\n")
  }
  
  # Perform cross-validation
  cat("Performing cross-validation for k = 50 and DPI =", DPI_it, "\n")
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
    class_xval = knn(M_xval_train, M_xval_test, true_class_xval, k_it)
    true_test = M_xval_test[,1]
    true_test = factor(true_test, levels(class_xval))
    #true_class_xval <- factor(true_class_xval, levels(class_xval))
    #success_xval <- sum(true_class_xval == class_xval)/length(class_xval)
    success_xval = sum(true_test == class_xval)/length(class_xval)
    cat("Result", i, ":", success_xval, "\n")
  }
}