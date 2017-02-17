# Solution to exercise 1.4.1
# Created 17.02.2017
#----------------------------

# Read data
if(!exists("M")) {
  M = loadSinglePersonsData(100, 'group5', 1, '/home/bjarkips/SML/trunk/2017/')
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
class_train = knn(M_train, M_train, true_class_test, k = 50)
true_class_train <- factor(true_class_train, levels(class_train))

# Train on training set and test on test set
class_test = knn(M_train, M_test, true_class_train, k = 50)
true_class_test <- factor(true_class_test, levels(class_test))

# Show results
success_train <- sum(true_class_train == class_train)/length(class_train)
print(success_train)
success_test <- sum(true_class_test == class_test)/length(class_test)
print(success_test)