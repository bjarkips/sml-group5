source('~/workspace/SML/trunk/Basefolder/loadImage.R')
source('./utilityFunctions.r')
source("https://raw.githubusercontent.com/ggrothendieck/gsubfn/master/R/list.R")
if (!exists("M_c")) { # _c stands for complete data set
  group = c('group0', 'group1', 'group2', 'group3', 'group4', 'group5', 'group6', 'group7', 'group9', 'group10', 'group11', 'group12')
  memberCnt = c(3, 3, 3, 4, 3, 3, 3, 2, 1, 1, 2, 1)
  groupNr = 0
  it = 0
  M_big = list()
  for (group_it in group) {
    groupNr = groupNr + 1
    for (memberNr in 1:memberCnt[groupNr]) {
      it = it + 1
      M_big[[it]] = loadSinglePersonsData(100, group_it, memberNr, '~/workspace/SML/trunk/2017/')
    }
  }
  M_c = do.call(rbind, M_big)
  
  # Shuffle
  M_shuffled_c = M_c[sample(nrow(M_c)),]
  
  # Reduce dimensionality to 90% of total variance
  "M_PCA_c = prcomp(M_shuffled_c[,2:ncol(M_shuffled_c)], retx = TRUE)
  summ = summary(M_PCA_c)
  imp = summ[['importance']]
  cumulative_prop = imp['Cumulative Proportion',]
  num_pc_c = findBreakpoint(cumulative_prop)
  M_reduced_c = M_PCA_c[['x']][,1:num_pc_c]
  "
  # Split into training and test sets
  # Person-dependent
  M_train_dep_c = M_shuffled_c[1:(nrow(M_shuffled_c)/2),]
  M_test_dep_c = M_shuffled_c[(nrow(M_shuffled_c)/2+1):nrow(M_shuffled_c),]
  true_class_test_dep_c = M_test_dep_c[,1]
  
  # Person-independent
  M_train_ind_unshuffled_c = M_c[1:(nrow(M_c)/2),]
  M_test_ind_unshuffled_c = M_c[(nrow(M_c)/2+1):nrow(M_c),]
  M_train_ind_c = M_train_ind_unshuffled_c[sample(nrow(M_train_ind_unshuffled_c)),]
  M_test_ind_c = M_test_ind_unshuffled_c[sample(nrow(M_train_ind_unshuffled_c)),]
  true_class_train_ind_c = M_train_ind_c[,1]
  true_class_test_ind_c = M_test_ind_c[,1]
}

"clusters = kmeans(M_reduced_c, 10)
dev.new()
plot ( M_reduced_c, col = clusters$cluster, type = 'p')
dev.new()
plot(clusters$centers, col = 1:10)
dev.new()
scatterplot3d(M_reduced_c[,1], M_reduced_c[,2], M_reduced_c[,3], color = clusters$cluster)
dev.new()
scatterplot3d(clusters$centers[,1], clusters$centers[,2], clusters$centers[,3], color = 1:10)
"

means_k = 200
nearest_neighbour_k = 50

list[M_train_dep_clustered_c, true_class_train_dep_c] = clustering(M_train_dep_c, means_k)
class_dep_c = knn(M_train_dep_clustered_c, M_test_dep_c[,2:ncol(M_test_dep_c)], true_class_train_dep_c, nearest_neighbour_k)
true_class_test_dep_c = factor(true_class_test_dep_c, levels(class_dep_c))
accuracy_dep_c = sum(true_class_test_dep_c == class_dep_c)/length(class_dep_c)

list[M_train_ind_clustered_c, true_class_train_ind_c] = clustering(M_train_ind_c, means_k)
class_ind_c = knn(M_train_ind_clustered_c, M_test_ind_c[,2:ncol(M_test_ind_c)], true_class_train_ind_c, nearest_neighbour_k)
#class_ind_c = knn(M_train_ind_c, M_test_ind_c, true_class_train_ind_c, nearest_neighbour_k)
true_class_test_ind_c = factor(true_class_test_ind_c, levels(class_ind_c))
accuracy_ind_c = sum(true_class_test_ind_c == class_ind_c)/length(class_ind_c)
