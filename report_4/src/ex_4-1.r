# Solution to exercise 4.1
# Created 06.04.2017
#---------------------------------

source('./src/utilityFunctions.r')
#library(party)
#library(matrixStats)

ent_pre = entropy(M_reduced_2)
#print(ent_pre)

if(!exists('M_norm_2')) {
  M_norm_2 = (M_reduced_2[,2:ncol(M_reduced_2)] - min(M_reduced_2[,2:ncol(M_reduced_2)])) / (max(M_reduced_2[,2:ncol(M_reduced_2)]) - min(M_reduced_2[,2:ncol(M_reduced_2)]))
  #M_norm_2 = (M_reduced_2[,2:ncol(M_reduced_2)] - mean(M_reduced_2[,2:ncol(M_reduced_2)])) / sd(M_reduced_2[,2:ncol(M_reduced_2)])
}

#ctree(class ~ ., M_norm_2)#data.frame(cbind(M_reduced_2[,1], M_norm_2)))
dev.new()
par(mfrow = c(2, 3))
for (pc in 1:5) {
  pcstr = paste("PC", pc, sep='')
  M_norm_sort_2 = as.matrix( data.table(M_norm_2, key=pcstr) ) # Sort matrix along current PC
  maxIG = 0
  ent_post = numeric(nrow(M_norm_2))
  for (i in 1:nrow(M_norm_2)) {
    threshold = M_norm_sort_2[i,pc]
    M_split_1 = cbind(M_reduced_2[M_norm_2[,pc] >= threshold,1], M_norm_2[ M_norm_2[,pc] >= threshold ,])
    M_split_2 = cbind(M_reduced_2[M_norm_2[,pc] < threshold,1], M_norm_2[ M_norm_2[,pc] < threshold ,])
    s1 = length(M_split_1)
    s2 = length(M_split_2)
    if ( s1 == 0 | s2 == 0 ) {
      ent_post = ent_pre
      next
    }
    #cat("i is now ", i, "\n")
    ent_post[i] = (entropy(M_split_1)*s1 + entropy(M_split_2)*s2) / (s1+s2)
    #ent_post[i] = entropyBin1(M_split_1, M_split_2)
    IG = ent_pre - ent_post[i]
    oldMax = maxIG
    maxIG = max(IG, maxIG)
    if( maxIG != oldMax ) best = threshold
    
    #M_split_1 = M_norm_2[ (M_reduced_2[,pc] == 1) ,]
    #M_split_2 = M_norm_2[ (M_reduced_2[,pc] != 1) ,]
  }
  plot(ent_pre-ent_post, ylab = 'Information Gain', type = 'l')
  cat("Max. information gain for PC", pc, ": ", maxIG, '\n', sep = '')
}