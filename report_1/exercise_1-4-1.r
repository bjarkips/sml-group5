# Read data
if(!exists("M")) {
  M = loadSinglePersonsData(100, 'group5', 1, '/home/bjarkips/SML/trunk/2017/')
}

# Shuffle data
set.seed(123)
M_shuffled <- M[sample(nrow(M)),]
M_train <- M_shuffled[1:(nrow(M)/2),]
M_test <- M_shuffled[(nrow(M)/2+1):nrow(M),]
true_class <- M_shuffled[,1]
true_class_train <- true_class[1:(nrow(M)/2)]
true_class_test <- true_class[(nrow(M)/2+1):nrow(M)]

class_train = knn(M_train, M_train, true_class_test, k = 50)
class_test = knn(M_train, M_test, true_class_train, k = 50)
true_class_train <- factor(true_class_train, levels(class_train))
true_class_test <- factor(true_class_test, levels(class_test))
result_train <- true_class_train == class_train
print(table(result_train))
result_test <- true_class_test == class_test
print(table(result_test))


