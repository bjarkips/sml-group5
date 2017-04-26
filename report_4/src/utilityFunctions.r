# Complementary functions to exercise 4
# Created 06.04.2017
#------------------------------------------

entropy = function(M) {
  p = numeric(10)
  sum = 0
  for(i in 1:10) {
    p[i] = length( M[ M[,1] == i-1]) / length(M)
    sum = sum - p[i]*log2(p[i])
  }
  return(sum)
}