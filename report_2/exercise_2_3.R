#Get the size of each image
imageSize<-sqrt(ncol(M)-1);

#Create the matrix with the image of one Cipher
imageM <- matrix( M[600,2:ncol(M)],nrow = imageSize,ncol = imageSize,byrow = FALSE)

#imageM <- rotate(imageM) 

#view the image of one cipher
image( imageM )

#see one eigenvector
image(matrix(M_PCA$x[250:nrow(M_PCA$x),1]))

#getting the reconstruction of the images
trunc <- M_PCA$x[600,1:nrow(M_PCA$rotation)] %*% t(M_PCA$rotation[,1:nrow(M_PCA$rotation)])
trunc <- scale(trunc, center = -1 * M_PCA$center, scale=FALSE)

#plot the image of the reconstruction with pca
image(trunc)