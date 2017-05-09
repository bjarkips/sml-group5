source('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')

id = M_reduced_2.df

lev <- levels(factor(id$V1))
nnTrainingClass <- matrix(nrow = length(id$V1), ncol = 10, data =  0)
for(i in 1:length(id$V1)) {
  matchList <-  match(lev,toString(id$V1[i]))
  matchList[is.na(matchList)] <- 0
  nnTrainingClass[i,] <- matchList   
}
trainingClass <- as.data.frame(nnTrainingClass)

nn = mlp(M_reduced_2[,-1], y = trainingClass)
plot.nnet(nn)