require(randomForest)
set.seed(123)

M_train_dep_2.df = data.frame(M_train_dep_2)
rf = randomForest(X1~., M_train_dep_2.df)

pred = predict(rf, data.frame(M_test_dep_2))
sum(round(pred) == M_test_dep_2[,1]) / length(M_test_dep_2[,1])