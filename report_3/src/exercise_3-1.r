# Solution to exercise 3.1
# Created 22.03.2017
#---------------------------------

source('~/workspace/SML/trunk/Basefolder/loadImage.R')
source('./src/utilityFunctions.r')
# This is used for catching multiple function outputs (via list[])
source("https://raw.githubusercontent.com/ggrothendieck/gsubfn/master/R/list.R")

graphics.off()
set.seed(123)

if (!exists("M_2")) { # _2 stands for 2-person data
  
  # Load 2-person data
  M_big_2 = list()
  for (i in 1:2) {
    M_big_2[[i]] = loadSinglePersonsData(100, 'group5', i, '/home/bjarkips/workspace/SML/trunk/2017/')
  }
  M_2 = do.call(rbind, M_big_2)

  # Shuffle
  M_shuffled_2 = M_2[sample(nrow(M_2)),]
  
  # Reduce dimensionality to 90% of total variance
  "M_PCA_2 = prcomp(M_shuffled_2[,2:ncol(M_shuffled_2)], retx = TRUE)
  summ = summary(M_PCA_2)
  imp = summ[['importance']]
  cumulative_prop = imp['Cumulative Proportion',]
  num_pc_2 = findBreakpoint(cumulative_prop)
  M_reduced_2 = M_PCA_2[['x']][,1:num_pc_2]
  "
  # Split into training and test sets
  # Person-dependent
  M_train_dep_2 = M_shuffled_2[1:(nrow(M_shuffled_2)/2),]
  M_test_dep_2 = M_shuffled_2[(nrow(M_shuffled_2)/2+1):nrow(M_shuffled_2),]
  true_class_test_dep_2 = M_test_dep_2[,1]
  
  # Person-independent
  M_train_ind_unshuffled_2 = M_2[1:(nrow(M_2)/2),]
  M_test_ind_unshuffled_2 = M_2[(nrow(M_2)/2+1):nrow(M_2),]
  M_train_ind_2 = M_train_ind_unshuffled_2[sample(nrow(M_train_ind_unshuffled_2)),]
  M_test_ind_2 = M_test_ind_unshuffled_2[sample(nrow(M_train_ind_unshuffled_2)),]
  true_class_train_ind_2 = M_train_ind_2[,1]
  true_class_test_ind_2 = M_test_ind_2[,1]
}

"clusters = kmeans(M_reduced_2, 10)
dev.new()
plot ( M_reduced_2, col = clusters$cluster, type = 'p')
dev.new()
plot(clusters$centers, col = 1:10)
dev.new()
scatterplot3d(M_reduced_2[,1], M_reduced_2[,2], M_reduced_2[,3], color = clusters$cluster)
dev.new()
scatterplot3d(clusters$centers[,1], clusters$centers[,2], clusters$centers[,3], color = 1:10)
"

#means_k = 200
means_ks = c(200,100,50,25)
nearest_neighbour_k = 1
result_dep_2 = numeric()
result_ind_2 = numeric()
for (means_k in means_ks) {
  list[M_train_dep_clustered_2, true_class_train_dep_2] = clustering(M_train_dep_2, means_k)
  class_dep_2 = knn(M_train_dep_clustered_2, M_test_dep_2[,2:ncol(M_test_dep_2)], true_class_train_dep_2, nearest_neighbour_k)
  true_class_test_dep_2 = factor(true_class_test_dep_2, levels(class_dep_2))
  accuracy_dep_2 = sum(true_class_test_dep_2 == class_dep_2)/length(class_dep_2)
  result_dep_2 = c(result_dep_2, accuracy_dep_2)
  
  list[M_train_ind_clustered_2, true_class_train_ind_2] = clustering(M_train_ind_2, means_k)
  class_ind_2 = knn(M_train_ind_clustered_2, M_test_ind_2[,2:ncol(M_test_ind_2)], true_class_train_ind_2, nearest_neighbour_k)
  true_class_test_ind_2 = factor(true_class_test_ind_2, levels(class_ind_2))
  accuracy_ind_2 = sum(true_class_test_ind_2 == class_ind_2)/length(class_ind_2)
  result_ind_2 = c(result_ind_2, accuracy_ind_2)
  
  cat('\nUsing', means_k, 'clusters.\n')
  cat('Person-dependent accuracy:\t',accuracy_dep_2)
  cat('\nPerson-independent accuracy:\t', accuracy_ind_2)
  cat('\n')
}

x_labels = expression()
for (means_k in means_ks) {
  x_labels = c(x_labels, bquote(2^{.(log2(means_k/400))}))
}

plotmaty = matrix(c(result_dep_2, result_ind_2), ncol = 2)

matplot(log2(means_ks/400), plotmaty, type = 'o', pch = 'o', ylab = 'Accuracy', xlab = 'Clustering ratio', ylim = c(0,1), main = 'Two-person data, k=1', xaxt = 'n')
axis(1, at = log2(means_ks/400), labels = x_labels)
legend('right', legend = c('Person-dependent', 'Person-independent'), col = 1:2, pch = 'o')

#plot(log2(means_ks/400), result_dep_2, type = 'o', xaxt = 'n', xlab = 'Data split')
#axis(1, at = log2(means_ks/400), labels = x_labels)
#dev.new()
#plot(log2(means_ks/400), result_ind_2, type = 'o', xaxt = 'n', xlab = 'Data split')
#axis(1, at = log2(means_ks/400), labels = x_labels)