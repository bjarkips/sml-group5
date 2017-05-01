# Solution to exercise 4.1
# Created 06.04.2017
#---------------------------------

source('./src/utilityFunctions.r')

ent_pre = entropy(M_reduced_2)
print(ent_pre)

if(!exists('M_norm_2')) {
  M_norm_2 = (M_reduced_2[,2:ncol(M_reduced_2)] - min(M_reduced_2[,2:ncol(M_reduced_2)])) / (max(M_reduced_2[,2:ncol(M_reduced_2)]) - min(M_reduced_2[,2:ncol(M_reduced_2)]))
  #M_norm_2 = (M_reduced_2[,2:ncol(M_reduced_2)] - mean(M_reduced_2[,2:ncol(M_reduced_2)])) / sd(M_reduced_2[,2:ncol(M_reduced_2)])
}

M_split = list() #matrix(, nrow = nrow(M_norm_2), ncol = 5)
dec = numeric(10)
for (i in 1:11) {
  dec[i] = (i-1)/10.0
}
qua = quantile(M_norm_2, dec) # Find quantiles 0, 0.1, 0.2, ..., 1 for thresholding
M_class_list = list()
for (pc in 1:5) {
  M_class_list[[pc]] = numeric(nrow(M_norm_2))
  for(i in 1:10) {
    M_split[[i]] = ((M_norm_2[,pc] > qua[i]) & (M_norm_2[,pc] < qua[i+1]))*(i-1)
    M_class_list[[pc]] = M_class_list[[pc]] + M_split[[i]]
  }
}
M_class = do.call(cbind, M_class_list)
ent_post = numeric(5)
for (pc in 1:5) {
  ent_post[pc] = entropy(cbind(M_reduced_2[,1], M_class[,pc]))
}