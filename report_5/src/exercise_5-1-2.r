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