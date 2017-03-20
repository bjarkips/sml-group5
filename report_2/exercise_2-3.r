source('~/SML/trunk/Basefolder/loadImage.R')

cipherNum = 1

imageSize <- sqrt(ncol(M_PCA[['rotation']]))
imageM <- matrix( M_PCA[['rotation']][,1],nrow = imageSize,ncol = imageSize,byrow = FALSE)

#imageM <- matrix( M[cipherNum,2:ncol(M)],nrow = imageSize,ncol = imageSize,byrow = FALSE)

imageM <- rotate(imageM, -90) # rotate is a function to rotate the image
image( imageM )

imageReconstr = matrix(double(324), nrow = imageSize, ncol = imageSize, byrow = FALSE)
reconstr = double(324)

for (i in 1:200) {
  reconstr = reconstr + M_PCA[['rotation']][,i] * M_PCA[['x']][cipherNum,i]
  #imageM = matrix( M_PCA[['rotation']][1,2:325], nrow = imageSize, ncol = imageSize, byrow = FALSE)
  
  #imageReconstr = imageReconstr + imageM * M_PCA[['x']][1, i]
}
imageReconstr = matrix(reconstr, nrow = imageSize, ncol = imageSize, byrow = FALSE)
imageReconstr = rotate(imageReconstr, -90)
#image(imageReconstr)

cipherNum = cipherNum + 1