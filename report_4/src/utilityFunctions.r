# Complementary functions to exercise 4
# Created 06.04.2017
#------------------------------------------

entropy = function(M) {
  p = numeric(10)
  #p = .1
  sum = 0
  if(length(M))
  for(i in 1:10) {
    p[i] = length( M[ M[,1] == i-1 ]) / length(M)
    if (p[i] == 0) next
    #if(is.na(p[i])) print("Eureka!")
    sum = sum - p[i]*log2(p[i])
  }
  return(sum)
}

#entropyBin1 = function(M1, M2) {
#  p = numeric(10)
#}