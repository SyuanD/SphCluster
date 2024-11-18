#These functions are used to conduct the spherical clustering
#including spherical k-means method and spherical k-pc method.

# Load the necessary package for skmeans
library(skmeans)

######################################
#SKMeans
######################################

# Define the function to handle K-means for k=1 and k>1
Skmeans_func <- function(data, k, ...) {

  # K-means when k=1
  skmeans1 <- function(data) {
    # Get dimensions of the data
    n <- nrow(data)
    d <- ncol(data)

    # Compute the sum of the data and normalize
    y <- colSums(data)
    y1 <- y / sqrt(sum(y^2))

    # Center is the normalized sum of data
    center <- y1

    # Compute Cosine Disimalrity
    dot_product<-center*data
    magnitude_A <- sqrt(sum(center^2))
    magnitude_B <- sqrt(rowSums(data^2))

    # Calculate cosine dissimilarity
    dissimilarity <- sum(1 - dot_product / (magnitude_A * magnitude_B))

    return(list(prototypes = matrix(center, nrow = 1), cluster = rep(1, n), value = dissimilarity))
  }

  # Choose between skmeans1 and skmeans based on the value of k
  if (k == 1) {
    result <- skmeans1(data)
  } else {
    # Use the standard skmeans function for k > 1
    result <- skmeans(data, k, ...)
  }

  return(result)
}

clusterKM <- function(data, k = 2, tol = 1e-5, nrep = 100, startFromMeans = FALSE) {
  maxval <- 0
  best_centroids <- NULL

  for (i in 1:nrep) {
    res <-Skmeans_func(data,k)
    if (res$value > maxval) {
      maxval <- res$value
      best_centroids <- res$prototypes
      cluster<-res$cluster
    }
  }

 return(list(prototypes=best_centroids, cluster=cluster, value=maxval))
}


######################################
# SKPC Function
######################################
# Spherical k-means based on cosine dissimilarity
clusterMeans <- function(data, k = 2, nruns = 100) {
  skmeans(data, k, method = "pclust", control = list(nruns = nruns))$prototypes
}

#######################
# Single iteration
# centroids is a k*d matrix with current proposals
# These functions are from papaer XXX
clusterPC_iter <- function(data, centroids) {
  k <- nrow(centroids)
  n <- nrow(data)

  M <- data %*% t(centroids)

  # Find current value
  v <- mean(apply(M, 1, max)^2)

  gr <- max.col(M, ties.method = "first")  # More efficient than apply(M, 1, which.max)

  for (i in 1:k) {
    seldata <- data[gr == i, , drop = FALSE]  # Ensure it's treated as a matrix even if one row

    if (nrow(seldata) > 0) {
      Sig <- t(seldata) %*% seldata / n
      res <- eigen(Sig)
      centroids[i, ] <- abs(res$vectors[, 1])  # Use the first eigenvector
    }
  }
  list(centroids, v)
}

# Pick randomly the initial centers
clusterPCOnce <- function(data, k, tol, startFromMeans = FALSE) {
  val <- 0
  n <- nrow(data)

  centroids <- if (startFromMeans) {
    clusterMeans(data, k)
  } else {
    data[sample(1:n, k), , drop = FALSE]  # Ensure centroids is a matrix
  }

  niter <- 0
  repeat {
    niter <- niter + 1
    res <- clusterPC_iter(data, centroids)
    centroids <- res[[1]]
    diff <- res[[2]] - val
    val <- res[[2]]

    if (diff < tol) break
  }

  list(centroids, val)
}

# Iterate nrep times and pick the best
clusterPC <- function(data, k = 2, tol = 1e-5, nrep = 100, startFromMeans = FALSE) {
  maxval <- 0
  best_centroids <- NULL

  for (i in 1:nrep) {
    res <- clusterPCOnce(data, k, tol, startFromMeans && (i == 1))
    if (res[[2]] > maxval) {
      maxval <- res[[2]]
      best_centroids <- res[[1]]
    }
  }

  best_centroids
}

##################################################
#################### Useful routines #############
# Assign groups
getClusterIndex <- function(data, centroids) {
  M <- data %*% t(centroids)
  gr <- max.col(M, ties.method = "first")  # More efficient than argmax
  gr
}

# Compute dissimilarity cost
getCost <- function(data, centroids, cosine = TRUE) {
  M <- data %*% t(centroids)
  products <- apply(M, 1, max)

  if (cosine) {
    return(1 - mean(products))
  } else {
    return(1 - mean(products^2))
  }
}

# Main function for spherical k-means
SKPC <- function(data, k) {
  centroids <- clusterPC(data, k, startFromMeans = TRUE)
  clusterInd <- getClusterIndex(data, centroids)

  li <- list(centroids, clusterInd)
  names(li) <- c('prototypes', 'cluster')

  return(li)
}


##################################################
# General Spherical Cluster Functions
##################################################

SphCluster<-function(data,k,method,tol = 1e-5, nrep = 100, startFromMeans = FALSE){
  if(method=='k-means'){
    output<-clusterKM(data, k, tol, nrep)
  }
  if(method=='k-pc'){
    output<-SKPC(data,k)
  }
  return(list(prototypes=output$prototypes,cluster=output$cluster))
}
