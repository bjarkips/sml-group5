# Solution to exercise 1.4.1
# Created 17.02.2017
#----------------------------

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
}