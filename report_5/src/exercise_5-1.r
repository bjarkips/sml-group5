source('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')

require(RSNNS)

set.seed(123)

M_train_dep_2 = M_reduced_2[1:(nrow(M_reduced_2)/2),]
M_test_dep_2 = M_reduced_2[(nrow(M_reduced_2)/2+1):nrow(M_reduced_2),]

id = data.frame(M_train_dep_2) #M_reduced_2.df
lev <- levels(factor(id$V1))
nnTrainingClass <- matrix(nrow = length(id$V1), ncol = 10, data =  0)
for(i in 1:length(id$V1)) {
  matchList <-  match(lev,toString(id$V1[i]))
  matchList[is.na(matchList)] <- 0
  nnTrainingClass[i,] <- matchList   
}
trainingClass <- as.data.frame(nnTrainingClass)

id2 = data.frame(M_test_dep_2) #M_reduced_2.df
lev <- levels(factor(id2$V1))
nnTestClass <- matrix(nrow = length(id2$V1), ncol = 10, data =  0)
for(i in 1:length(id2$V1)) {
  matchList2 <-  match(lev,toString(id2$V1[i]))
  matchList2[is.na(matchList2)] <- 0
  nnTestClass[i,] <- matchList2  
}
testClass <- as.data.frame(nnTestClass)

nn = mlp(M_train_dep_2[,-1], y = trainingClass, size = c(1), maxit = 1000)
#plot.nnet(nn)

#dev.new()
plotIterativeError(nn)

#dev.new()
pred = predict(nn, M_test_dep_2[,-1])
predClass = c()
for (i in 1:nrow(pred)) {
  predClass = c(predClass, which.max(pred[i,])-1)
}
accuracy_dep_2 = sum(predClass == M_test_dep_2[,1])/length(M_test_dep_2[,1])
#plotRegressionError(pred[,2], )