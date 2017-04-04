# Solution to exercise 3.1
# Created 28.03.2017
#---------------------------------

source('~/workspace/SML/trunk/Basefolder/loadImage.R')
source('./src/utilityFunctions.r')
# This is used for catching multiple function outputs (via list[])
source("https://raw.githubusercontent.com/ggrothendieck/gsubfn/master/R/list.R")
graphics.off()
set.seed(123)

if(!exists('M_s')) {  # _s stands for single-person data
  
  # Load and shuffle data
  M_s = loadSinglePersonsData(100, 'group5', 1, '/home/bjarkips/workspace/SML/trunk/2017/')
  M_shuffled_s = M_s[sample(nrow(M_s)),]
  
  # Reduce dimensionality to 90% of total variance
  M_PCA_s = prcomp(M_shuffled_s[,2:ncol(M_shuffled_s)], retx = TRUE)
  summ = summary(M_PCA_s)
  imp = summ[['importance']]
  cumulative_prop = imp['Cumulative Proportion',]
  num_pc_s = findBreakpoint(cumulative_prop)
  M_reduced_s = M_PCA_s[['x']][,1:num_pc_s]
}

M_downsampled_2 = downsample(M_shuffled_2, 5)
hc_downsampled_2 = hclust(dist(M_downsampled_2[,2:ncol(M_downsampled_2)]))
plot(hc_downsampled_2, labels = M_downsampled_2[,1], xlab = 'Distance', sub = '', main = 'Random Downsampling')

list[M_clustered_2, cluster_labels_2] = clustering(M_shuffled_2, 5)
hc_clustered_2 = hclust(dist(M_clustered_2))
#dev.new()
plot(hc_clustered_2, labels = cluster_labels_2, xlab = 'Distance', sub = '', main = 'K-means Clustering')
