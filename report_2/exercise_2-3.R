source('C:/Users/mikeh/Documents/Tec/8Semestre(Dinamarca)/Statistical Machine Learning/sml-group5/report_2/loadImage.R')
cipherNum = 1

imageSize <- sqrt(ncol(M_PCA[['rotation']]))
imageM <- matrix( M_PCA[['rotation']][,3],nrow = imageSize,ncol = imageSize,byrow = FALSE)

#imageM <- matrix( M[cipherNum,2:ncol(M)],nrow = imageSize,ncol = imageSize,byrow = FALSE)

imageM <- rotate(imageM, -90) # rotate is a function to rotate the image
image( imageM )

imageReconstr = matrix(double(324), nrow = imageSize, ncol = imageSize, byrow = FALSE)
reconstr = matrix()

for (i in 1:324) {
  reconstr = reconstr + M_PCA[['rotation']][,i] %*% M_PCA[['x']][cipherNum,i]
  #imageM = matrix( M_PCA[['rotation']][1,2:325], nrow = imageSize, ncol = imageSize, byrow = FALSE)
  
  #imageReconstr = imageReconstr + imageM * M_PCA[['x']][1, i]
}
reconstr<-M_PCA[['rotation']][,1:324] %*% M_PCA[['x']][cipherNum,1:324]
print(M_PCA[['x']][cipherNum,1])
print(M_PCA$x[4000,1])

imageReconstr = matrix(reconstr, nrow = imageSize, ncol = imageSize, byrow = FALSE)
imageReconstr = rotate(imageReconstr, -90)
#image(imageReconstr)

=======
#source('C:/Users/mikeh/Documents/Tec/8Semestre(Dinamarca)/Statistical Machine Learning/sml-group5/report_2/loadImage.R')
source('~/workspace/SML/trunk/Basefolder/loadImage.R')
cipherNum = 1
cipherNum = 1

imageSize <- sqrt(ncol(M_PCA[['rotation']]))
imageM <- matrix( M_PCA[['rotation']][,1],nrow = imageSize,ncol = imageSize,byrow = FALSE)

#imageM <- matrix( M[cipherNum,2:ncol(M)],nrow = imageSize,ncol = imageSize,byrow = FALSE)

imageM <- rotate(imageM, -90) # rotate is a function to rotate the image
image( imageM )

imageReconstr = matrix(double(324), nrow = imageSize, ncol = imageSize, byrow = FALSE)
reconstr = matrix()

for (i in 1:324) {
  reconstr = reconstr + M_PCA[['rotation']][,i] %*% M_PCA[['x']][cipherNum,i]
  #imageM = matrix( M_PCA[['rotation']][1,2:325], nrow = imageSize, ncol = imageSize, byrow = FALSE)
  
  #imageReconstr = imageReconstr + imageM * M_PCA[['x']][1, i]
}
reconstr<-M_PCA[['rotation']][,1:324] %*% M_PCA[['x']][cipherNum,1:324]
print(M_PCA[['x']][cipherNum,1])
print(M_PCA$x[4000,1])

imageReconstr = matrix(reconstr, nrow = imageSize, ncol = imageSize, byrow = FALSE)
imageReconstr = rotate(imageReconstr, -90)
#image(imageReconstr)

>>>>>>> c718c532f1c6b704d0c15560b5a2ff136e39de79
cipherNum = cipherNum + 1