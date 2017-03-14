# Solution to exercise 2.1
# Created 09.03.2017
#----------------------------------

library(class)

k = 10
bp = 7
set.seed(123)
M_shuffled <- M[sample(nrow(M)),]

#if(!exists("M_PCA")) {
  M_PCA = prcomp(M_shuffled, retx = TRUE)
#}
#M_PCA = princomp(M_shuffled, retx = TRUE)
summ = summary(M_PCA)
imp = summ[['importance']]
plotrange = 1:10
plot(imp['Proportion of Variance', plotrange], main = 'PCA', xlab = '', ylab = 'Proportion of Variance', type = 'o', xaxt = 'n')
axis(1, at = plotrange, labels = colnames(imp)[plotrange])
breakpoints = c(3,7,15,44)
#for (bp in breakpoints) {
  cat("Using", bp, "principal components.\n")
  M_reduced = M_PCA[['x']][,1:bp]
  
  # Split data into training ad test sets (50/50)
  M_train <- M_reduced[1:(nrow(M)/2),]
  M_test <- M_reduced[(nrow(M)/2+1):nrow(M),]
  true_class <- M_shuffled[,1]
  true_class_train <- true_class[1:(nrow(M)/2)]
  true_class_test <- true_class[(nrow(M)/2+1):nrow(M)]
  
  # Train on training set and test on test set
  class_test = knn(M_train, M_test, true_class_train, k)
  #system.time(replicate(100, knn(M_train, M_test, true_class_train, k)))
  cat("k =", k, "\n")
  true_class_test <- factor(true_class_test, levels(class_test))
  
  # Show results
  success_test <- sum(true_class_test == class_test)/length(class_test)
  cat("Test set:", success_test, "\n\n")
#}
