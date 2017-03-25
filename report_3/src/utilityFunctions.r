# Complementary functions to exercise_3-1.r
# Created 22.03.2017
#------------------------------------------

# Given a vector of cumulative proportions of variance and a breakpoint percentage, return 
# the number of principal components corresponding to the cumulative variance breakpoint
findBreakpoint = function(cumulative_prop, breakpoint){
  if(missing(breakpoint)) {
    breakpoint = .9
  }
  index = 0;
  for (current_prop in cumulative_prop) {
    index = index + 1;
    if (current_prop > breakpoint) {
      return(index)
    }
  }
  stop("Breakpoint index not found. Make sure that cumulative_var is a vector of cumulative variances and 0 <= breakpoint < 1")
}

# Perform k-means clustering preprocessing for kNN
clustering = function(id, means_k){
  cipher_cluster = c()
  label_cluster = c()
  for( i in 0:9) {
    clusterData = kmeans(id[ id[,1] == i, -1 ], means_k)
    cipher_cluster[[i + 1]] = clusterData$centers
    label_cluster[[i + 1]] = c(1:means_k)*0 + i
  }
  train_lab = factor(unlist(label_cluster))
  train_dat = cipher_cluster[[1]]
  for( i in 2:10) {
    train_dat = rbind(train_dat,cipher_cluster[[i]])
  }
  return(list(train_dat, train_lab))
}