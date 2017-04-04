# Solution to exercise 3.3
# Created 30.03.2017
#---------------------------------

source('~/workspace/SML/trunk/Basefolder/loadImage.R')
source('./src/utilityFunctions.r')
# This is used for catching multiple function outputs (via list[])
#source("https://raw.githubusercontent.com/ggrothendieck/gsubfn/master/R/list.R")
graphics.off()
#set.seed(123)

if (!exists('class_dep_list_2')) {
class_dep_list_2 = list()
class_dep_list_inner_2 = list()
  for (k in 1:13) {
    for (l in 1:k) {
      class_dep_list_inner_2[[l]] = knn(M_train_dep_2[,-1], M_test_dep_2[,-1], M_train_dep_2[,1], k, l)
    }
    class_dep_list_2[[k]] = do.call(rbind, class_dep_list_inner_2)
    #class_dep_list_2[[k]] = matrix(do.call(rbind, class_dep_list_inner_2))
  }
}

precision = numeric(91)
recall = numeric(91)
F1 = numeric(13)
index = 0
for (k in 1:13) {
  f1 = 0
  for (l in 1:k) {
    unclass = sum(is.na(class_dep_list_2[[k]][l,]))
    true_pos = sum(true_class_test_dep_2 == class_dep_list_2[[k]][l,] - 1, na.rm = TRUE)
    false_pos = sum(true_class_test_dep_2 != class_dep_list_2[[k]][l,] - 1, na.rm = TRUE)
    p = true_pos / (true_pos + false_pos)
    r = true_pos / length(true_class_test_dep_2)
    f = 2 * (p * r) / (p + r)
    
    index = index + 1
    precision[index] = p
    recall[index] = r
    f1 = max(f1, f)
  }
  F1[k] = f1
}

plot(1, type = 'n', xlab = 'Recall', ylab = 'Precision', pch = 'o', xlim = c(.2,1), ylim = c(.8,1))
index = 0
for (k in 1:13) {
  range = (index+1):(index+k)
  index = index + k
  lines(recall[range], precision[range], type = 'o', col = k)
}
legend('bottomleft', legend = paste('k = ', 1:13), col = 1:13, pch = 'o')

#plot(recall, precision, xlab = 'Recall', ylab = 'Precision', pch = 'o')
#legend('bottomright', legend = paste('k = ', 1:13), col = 1:13)
#dev.new()
#graphics.off()
#plot(1:13, F1, type = 'o', xlab = 'k', ylab = 'F1-Score', xaxt = 'n')
#axis(1, at = 1:13)
