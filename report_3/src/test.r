# Solution to exercise 2.1
# Created 09.03.2017
#----------------------------------

source('~/workspace/SML/trunk/Basefolder/loadImage.R')
library(class)


M_big_2 = list()
for (i in 1:2) {
  M_big_2[[i]] = loadSinglePersonsData(100, 'group5', i, '/home/bjarkips/workspace/SML/trunk/2017/')
}
M = do.call(rbind, M_big_2)
#k = 50
#bp = 15
set.seed(123)
M_shuffled <- M[sample(nrow(M)),]

#if(!exists("M_PCA")) {
M_PCA = prcomp(M_shuffled[,2:ncol(M_shuffled)], retx = TRUE)
#}
#M_PCA = princomp(M_shuffled, retx = TRUE)
summ = summary(M_PCA)
imp = summ[['importance']]
plotrange = 1:10
plot(imp['Proportion of Variance', plotrange], main = 'PCA', xlab = '', ylab = 'Proportion of Variance', type = 'o', xaxt = 'n')
axis(1, at = plotrange, labels = colnames(imp)[plotrange])
#breakpoints = c(3,7,15,44)
#breakpoints = c(7,16,27,60)
#breakpoints = c(13, 23, 36, 77)
breakpoints = c(20, 31, 45, 89)
ks = c(10, 200)
#for (bp in breakpoints) {
# for(k in ks) {
#bp = 13
k = 50
cat("Using", "principal components.\n")
M_reduced = M_PCA[['x']][,1:13]

# Split data into training and test sets (50/50)
M_train <- M_reduced[1:(nrow(M)/2),]
M_test <- M_reduced[(nrow(M)/2+1):nrow(M),]
true_class <- M_shuffled[,1]
true_class_train <- true_class[1:(nrow(M)/2)]
true_class_test <- true_class[(nrow(M)/2+1):nrow(M)]

M_2 = M
# Person-independent
M_train_ind_unshuffled_2 = M_2[1:(nrow(M_2)/2),]
M_test_ind_unshuffled_2 = M_2[(nrow(M_2)/2+1):nrow(M_2),]
M_train_ind_2 = M_train_ind_unshuffled_2[sample(nrow(M_train_ind_unshuffled_2)),]
M_test_ind_2 = M_test_ind_unshuffled_2[sample(nrow(M_train_ind_unshuffled_2)),]
true_class_train_ind_2 = M_train_ind_2[,1]
true_class_test_ind_2 = M_test_ind_2[,1]

# Train on training set and test on test set
tic = proc.time()
class_test = knn(M_train_ind_2[,2:ncol(M_train_ind_2)], M_test_ind_2[,2:ncol(M_test_ind_2)], true_class_train_ind_2, k)

#tic = proc.time()
#system.time(replicate(100, knn(M_train, M_test, true_class_train, k)))
#replicate(10, knn(M_train, M_test, true_class_train, k))
toc <- proc.time() - tic
print(toc)
cat("k =", k, "\n")
true_class_test_ind_2 <- factor(true_class_test_ind_2, levels(class_test))

# Show results
success_test <- sum(true_class_test_ind_2 == class_test)/length(class_test)
cat("Test set:", success_test, "\n\n")
#}
#}
