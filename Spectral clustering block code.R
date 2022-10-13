########### Installing libraries ###########################
library(factoextra)
library(readxl)
library(clusterSim)
library("writexl")
######################### 1/ For Normal ####################################################
################### Uploading data set #################################################
x <- data.frame(read_excel("D:/Grad school/Lab/clustring/data/Normal env weights.xlsx"))
x1<-x[1:2]

####################################################################################

############## spectral clustering #########
### Distance= Euclidean, Closest neighbors K=20, Number of clusters n=2, Inputs= BT and E

spectral_clustering <- function(mat, # matrix of data points
                                nn = 20, # the k nearest neighbors to consider
                                n_eig = 2) # m number of eignenvectors to keep
{
  mutual_knn_graph <- function(mat, nn = 20)
  {
    D <- as.matrix( dist(mat) ) # matrix of euclidean distances between data points in X
    #D <- as.matrix( dist(mat)* cov(mat[,1],mat[,2]) ) # matrix of cov euclidean distances between data points in X
    # intialize the knn matrix
    knn_mat <- matrix(0,
                      nrow = nrow(mat),
                      ncol = nrow(mat))
    
    # find the 10 nearest neighbors for each point
    for (i in 1: nrow(mat)) {
      neighbor_index <- order(D[i,])[2:(nn + 1)]
      knn_mat[i,][neighbor_index] <- 1 
    }
    
    # Now we note that i,j are neighbors iff K[i,j] = 1 or K[j,i] = 1 
    knn_mat <- knn_mat + t(knn_mat) # find mutual knn
    
    knn_mat[ knn_mat == 2 ] = 1
    
    return(knn_mat)
  }
  
  graph_laplacian <- function(W, normalized = TRUE)
  {
    stopifnot(nrow(W) == ncol(W)) 
    
    g = colSums(W) # degrees of vertices
    n = nrow(W)
    
    if(normalized)
    {
      D_half = diag(1 / sqrt(g) )
      return( diag(n) - D_half %*% W %*% D_half )
    }
    else
    {
      return( diag(g) - W )
    }
  }
  
  W = mutual_knn_graph(mat) # 1. matrix of similarities
  L = graph_laplacian(W) # 2. compute graph laplacian
  ei = eigen(L, symmetric = TRUE) # 3. Compute the eigenvectors and values of L
  n = nrow(L)
  return(ei$vectors[,(n - n_eig):(n - 1)]) # return the eigenvectors of the n_eig smallest eigenvalues
  
}

# do spectral clustering procedure
X_sc <- spectral_clustering(x1)
#X_sc
# run kmeans on the 2 eigenvectors
set.seed(123)
X_sc_kmeans <- kmeans(X_sc, 2)
clusters <- X_sc_kmeans$cluster
############## Adding the clusters variable to original dataset ##############
newdata <- data.frame(x,clusters)
#head(newdata)

################saving new dataset with clusters###################"
write_xlsx(newdata,"D:\\Grad school\\Lab\\clustring\\data\\Normal class.xlsx")
######################################################################


######################### 2/ For Hot ####################################################
################### Uploading data set #################################################
x <- data.frame(read_excel("D:/Grad school/Lab/clustring/data/Hot env weights.xlsx"))


####################################################################################

############## spectral clustering #########
### Distance= Euclidean, Closest neighbors K=10, Number of clusters n=3, Input= BT, E, N & C

spectral_clustering <- function(mat, # matrix of data points
                                nn = 10, # the k nearest neighbors to consider
                                n_eig = 2) # m number of eignenvectors to keep
{
  mutual_knn_graph <- function(mat, nn = 10)
  {
    D <- as.matrix( dist(mat) ) # matrix of euclidean distances between data points in X
    #D <- as.matrix( dist(mat)* cov(mat[,1],mat[,2]) ) # matrix of cov euclidean distances between data points in X
    # intialize the knn matrix
    knn_mat <- matrix(0,
                      nrow = nrow(mat),
                      ncol = nrow(mat))
    
    # find the 10 nearest neighbors for each point
    for (i in 1: nrow(mat)) {
      neighbor_index <- order(D[i,])[2:(nn + 1)]
      knn_mat[i,][neighbor_index] <- 1 
    }
    
    # Now we note that i,j are neighbors iff K[i,j] = 1 or K[j,i] = 1 
    knn_mat <- knn_mat + t(knn_mat) # find mutual knn
    
    knn_mat[ knn_mat == 2 ] = 1
    
    return(knn_mat)
  }
  
  graph_laplacian <- function(W, normalized = TRUE)
  {
    stopifnot(nrow(W) == ncol(W)) 
    
    g = colSums(W) # degrees of vertices
    n = nrow(W)
    
    if(normalized)
    {
      D_half = diag(1 / sqrt(g) )
      return( diag(n) - D_half %*% W %*% D_half )
    }
    else
    {
      return( diag(g) - W )
    }
  }
  
  W = mutual_knn_graph(mat) # 1. matrix of similarities
  L = graph_laplacian(W) # 2. compute graph laplacian
  ei = eigen(L, symmetric = TRUE) # 3. Compute the eigenvectors and values of L
  n = nrow(L)
  return(ei$vectors[,(n - n_eig):(n - 1)]) # return the eigenvectors of the n_eig smallest eigenvalues
  
}

# do spectral clustering procedure
X_sc <- spectral_clustering(x)
#X_sc
# run kmeans on the 2 eigenvectors
set.seed(123)
X_sc_kmeans <- kmeans(X_sc, 3)
clusters <- X_sc_kmeans$cluster
############## Adding the clusters variable to original dataset ##############
newdata <- data.frame(x,clusters)
#head(newdata)

################saving new dataset with clusters###################"
write_xlsx(newdata,"D:\\Grad school\\Lab\\clustring\\data\\Hot class.xlsx")
######################################################################



######################### 3/ For Cold ####################################################
################### Uploading data set #################################################
x <- data.frame(read_excel("D:/Grad school/Lab/clustring/data/Cold env weights.xlsx"))


####################################################################################

############## spectral clustering #########
### Distance= Euclidean, Closest neighbors K=10, Number of clusters n=3, Input= BT, E, N & C

spectral_clustering <- function(mat, # matrix of data points
                                nn = 10, # the k nearest neighbors to consider
                                n_eig = 2) # m number of eignenvectors to keep
{
  mutual_knn_graph <- function(mat, nn = 10)
  {
    D <- as.matrix( dist(mat) ) # matrix of euclidean distances between data points in X
    #D <- as.matrix( dist(mat)* cov(mat[,1],mat[,2]) ) # matrix of cov euclidean distances between data points in X
    # intialize the knn matrix
    knn_mat <- matrix(0,
                      nrow = nrow(mat),
                      ncol = nrow(mat))
    
    # find the 10 nearest neighbors for each point
    for (i in 1: nrow(mat)) {
      neighbor_index <- order(D[i,])[2:(nn + 1)]
      knn_mat[i,][neighbor_index] <- 1 
    }
    
    # Now we note that i,j are neighbors iff K[i,j] = 1 or K[j,i] = 1 
    knn_mat <- knn_mat + t(knn_mat) # find mutual knn
    
    knn_mat[ knn_mat == 2 ] = 1
    
    return(knn_mat)
  }
  
  graph_laplacian <- function(W, normalized = TRUE)
  {
    stopifnot(nrow(W) == ncol(W)) 
    
    g = colSums(W) # degrees of vertices
    n = nrow(W)
    
    if(normalized)
    {
      D_half = diag(1 / sqrt(g) )
      return( diag(n) - D_half %*% W %*% D_half )
    }
    else
    {
      return( diag(g) - W )
    }
  }
  
  W = mutual_knn_graph(mat) # 1. matrix of similarities
  L = graph_laplacian(W) # 2. compute graph laplacian
  ei = eigen(L, symmetric = TRUE) # 3. Compute the eigenvectors and values of L
  n = nrow(L)
  return(ei$vectors[,(n - n_eig):(n - 1)]) # return the eigenvectors of the n_eig smallest eigenvalues
  
}

# do spectral clustering procedure
X_sc <- spectral_clustering(x)
#X_sc
# run kmeans on the 2 eigenvectors
set.seed(123)
X_sc_kmeans <- kmeans(X_sc, 3)
clusters <- X_sc_kmeans$cluster
############## Adding the clusters variable to original dataset ##############
newdata <- data.frame(x,clusters)
#head(newdata)

################saving new dataset with clusters###################"
write_xlsx(newdata,"D:\\Grad school\\Lab\\clustring\\data\\Cold class.xlsx")
######################################################################

######################### 4/ For Exercise ####################################################
################### Uploading data set #################################################
x <- data.frame(read_excel("D:/Grad school/Lab/clustring/data/Exercise env weights.xlsx"))
x1<-x[1:2]

####################################################################################

############## spectral clustering #########
### Distance= Covariance Euclidean, Closest neighbors K=15, Number of clusters n=, Input= BT & E

spectral_clustering <- function(mat, # matrix of data points
                                nn = 15, # the k nearest neighbors to consider
                                n_eig = 2) # m number of eignenvectors to keep
{
  mutual_knn_graph <- function(mat, nn = 15)
  {
    #D <- as.matrix( dist(mat) ) # matrix of euclidean distances between data points in X
    D <- as.matrix( dist(mat)* cov(mat[,1],mat[,2]) ) # matrix of cov euclidean distances between data points in X
    # intialize the knn matrix
    knn_mat <- matrix(0,
                      nrow = nrow(mat),
                      ncol = nrow(mat))
    
    # find the 10 nearest neighbors for each point
    for (i in 1: nrow(mat)) {
      neighbor_index <- order(D[i,])[2:(nn + 1)]
      knn_mat[i,][neighbor_index] <- 1 
    }
    
    # Now we note that i,j are neighbors iff K[i,j] = 1 or K[j,i] = 1 
    knn_mat <- knn_mat + t(knn_mat) # find mutual knn
    
    knn_mat[ knn_mat == 2 ] = 1
    
    return(knn_mat)
  }
  
  graph_laplacian <- function(W, normalized = TRUE)
  {
    stopifnot(nrow(W) == ncol(W)) 
    
    g = colSums(W) # degrees of vertices
    n = nrow(W)
    
    if(normalized)
    {
      D_half = diag(1 / sqrt(g) )
      return( diag(n) - D_half %*% W %*% D_half )
    }
    else
    {
      return( diag(g) - W )
    }
  }
  
  W = mutual_knn_graph(mat) # 1. matrix of similarities
  L = graph_laplacian(W) # 2. compute graph laplacian
  ei = eigen(L, symmetric = TRUE) # 3. Compute the eigenvectors and values of L
  n = nrow(L)
  return(ei$vectors[,(n - n_eig):(n - 1)]) # return the eigenvectors of the n_eig smallest eigenvalues
  
}

# do spectral clustering procedure
X_sc <- spectral_clustering(x1)
#X_sc
# run kmeans on the 2 eigenvectors
set.seed(123)
X_sc_kmeans <- kmeans(X_sc, 2)
clusters <- X_sc_kmeans$cluster
############## Adding the clusters variable to original dataset ##############
newdata <- data.frame(x,clusters)
#head(newdata)

################saving new dataset with clusters###################"
write_xlsx(newdata,"D:\\Grad school\\Lab\\clustring\\data\\Exercise class.xlsx")
######################################################################







################ Tracing pair variables based on clusters ###############


library(e1071) 
library(ggplot2)


###################
qplot(newdata$BT, newdata$E, colour =factor(newdata$clusters),data = newdata)+ ggtitle("") +
  xlab("BT") + ylab("Eyes temperature")+labs(colour = "clusters")+
  scale_color_hue(labels = c("Cluster 1", "Cluster 2", "Cluster 3"))
qplot(newdata$BT, newdata$N, colour =factor(newdata$clusters),data = newdata)+ ggtitle("") +
  xlab("BT") + ylab("Nose temperature")+labs(colour = "clusters")+
  scale_color_hue(labels = c("Cluster 1", "Cluster 2", "Cluster 3"))
qplot(newdata$BT, newdata$C, colour =factor(newdata$clusters),data = newdata)+ ggtitle("") +
  xlab("BT") + ylab("Cheek temperature")+labs(colour = "clusters")+
  scale_color_hue(labels = c("Cluster 1", "Cluster 2", "Cluster 3"))
qplot(newdata$E, newdata$N, colour =factor(newdata$clusters),data = newdata)+ ggtitle("") +
  xlab("Eyes temperature") + ylab("Nose temperature")+labs(colour = "clusters")+
  scale_color_hue(labels = c("Cluster 1", "Cluster 2", "Cluster 3"))
qplot(newdata$E, newdata$C, colour =factor(newdata$clusters),data = newdata)+ ggtitle("") +
  xlab("Eyes temperature") + ylab("Cheek temperature")+labs(colour = "clusters")+
  scale_color_hue(labels = c("Cluster 1", "Cluster 2", "Cluster 3"))
qplot(newdata$N, newdata$C, colour =factor(newdata$clusters),data = newdata)+ ggtitle("") +
  xlab("Nose temperature") + ylab("Cheek temperature")+labs(colour = "clusters")+
  scale_color_hue(labels = c("Cluster 1", "Cluster 2", "Cluster 3"))
#####################


##############Tracing 3D graph############################################
library(rgl)
library(magick)
colors3d <- c("steelblue2", "firebrick3", "springgreen2")
newdata$color <- colors3d[ as.numeric( as.factor(newdata$clusters) ) ]

# Static chart
plot3d( newdata$E, newdata$N, newdata$C, col = newdata$color, type = "s", radius = .08 )

# We can indicate the axis and the rotation velocity
play3d( spin3d( axis = c(0, 0, 1), rpm = 5), duration = 15 )

# Save like gif
movie3d(
  movie="3dAnimatedScatterplot exercise cov dist 15", 
  spin3d( axis = c(0, 0, 1), rpm = 7),
  duration = 20, 
  dir = "D:/Grad school/Lab/clustring/data/",
  type = "gif", 
  clean = TRUE
)
###########################################################################
