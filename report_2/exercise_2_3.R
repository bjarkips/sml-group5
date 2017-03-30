source('C:/Users/mikeh/Documents/Tec/8Semestre(Dinamarca)/Statistical Machine Learning/sml-group5/report_2/loadImage.R')

#Get the size of each image
imageSize<-sqrt(ncol(M)-1);


#Create the matrix with the image of one Cipher
imageM <- matrix( M[3605,2:ncol(M)],nrow = imageSize,ncol = imageSize,byrow = FALSE)

imageM <- rotate(imageM, -90) 

#view the image of one cipher
image( imageM )

#see one eigenvector
image(rotate(matrix(M_PCA$rotation[1:nrow(M_PCA$rotation),10],nrow = imageSize,ncol = imageSize,byrow = FALSE), -90))



#getting the reconstruction of the images
trunc <- M_PCA$x[1,1:10] %*% t(M_PCA$rotation[,1:10])
trunc <- scale(trunc, center = -1 * M_PCA$center, scale=FALSE)
#19/80
#30/90
#43/95
#plot the image of the reconstruction with pca
image(rotate(matrix(trunc, nrow = imageSize,ncol = imageSize,byrow = FALSE), -90))