######################################
#Penalized Silhouette Method
######################################

#the output is the silhouette value -- number of clusters

######################################
#Cosine Dismilarity
######################################

# Efficient Cosine Dissimilarity Function
cosine_dissimilarity <- function(A, B) {
  dot_product <- sum(A * B)
  magnitude_A <- sqrt(sum(A^2))
  magnitude_B <- sqrt(sum(B^2))

  # Calculate cosine dissimilarity
  dissimilarity <- 1 - dot_product / (magnitude_A * magnitude_B)

  return(dissimilarity)
}

# Calculate cosine dissimilarities between a vector and each row of a matrix
calculate_cosine_dissimilarity <- function(data1, data2) {
  # Use matrix operations for more efficient computation
  data1_matrix <- matrix(rep(data1, nrow(data2)), nrow = nrow(data2), byrow = TRUE)
  dot_products <- rowSums(data1_matrix * data2)
  magnitudes_data1 <- sqrt(sum(data1^2))
  magnitudes_data2 <- sqrt(rowSums(data2^2))

  # Calculate cosine dissimilarities
  dissimilarities <- 1 - dot_products / (magnitudes_data1 * magnitudes_data2)

  return(dissimilarities)
}

calculate_cosine_dissimilarityPC <- function(data1, data2) {
  # data1<-data[i,]
  # data2<-center[-kx,]
  dissimilarities <- numeric(nrow(data2))

  for (i in 1:nrow(data2)) {
    dissimilarities[i] <- cosine_dissimilarity(data1, data2[i, ])*(1+data1%*%data2[i, ])
  }

  return(dissimilarities)
}

# Helper function to compute the basic silhouette for each point
compute_silhouette <- function(data_point, center_point, other_centers, method) {
  # Compute cosine dissimilarity for the current cluster
  ax <- cosine_dissimilarity(data_point, center_point)

  # If using the k-pc method, modify ax accordingly
  if (method == 'k-pc') {
    ax <- ax * (1 + data_point %*% t(center_point))
  }

  # Compute cosine dissimilarity for other clusters (bx)
  if (nrow(other_centers) == 1) {
    bx <- cosine_dissimilarity(data_point, other_centers)
    if (method == 'k-pc') {
      bx <- bx * (1 + data_point %*% other_centers)
    }
  } else {
    bx <- min(calculate_cosine_dissimilarity(data_point, other_centers))
    if (method == 'k-pc') {
      bx <- bx * min(1 + data_point %*% t(other_centers))
    }
  }

  # Compute silhouette score for the point
  silhouette <- (bx - ax) / max(ax, bx)
  return(silhouette)
}

# Helper function to calculate inter-cluster dissimilarities
compute_inter_cluster_dissimilarities <- function(center, k, method) {
  inter_cluster_dissimilarity <- numeric(k)

  for (i in 1:k) {
    if (method == "k-pc") {
      inter_cluster_dissimilarity[i] <- min(calculate_cosine_dissimilarityPC(center[i, ], center[-i, , drop = FALSE]))
    } else {
      inter_cluster_dissimilarity[i] <- min(calculate_cosine_dissimilarity(center[i, ], center[-i, , drop = FALSE]))
    }
  }

  return(inter_cluster_dissimilarity)
}

# Main PSilhouette function
PSilhouette <- function(data, clusters = NULL, center, t , method) {
  n <- nrow(data)

  # Handle the case where there is only one cluster
  if (dim(center)[1] == 1) {
    silhouette_v <- numeric(n)

    for (i in 1:n) {
      ax <- cosine_dissimilarity(data[i, ], center)
      if (method == 'k-pc') {
        ax <- ax * (1 + data[i, ] %*% t(center))
      }
      silhouette_v[i] <- ax
    }

    sil_value <- mean(silhouette_v)
    return(sil_value)
  }

  # For multiple clusters
  k <- length(unique(clusters))
  silhouette_vec <- numeric(n)

  for (i in 1:n) {
    kx <- clusters[i]
    center_point <- center[kx, ]
    other_centers <- center[-kx, , drop = FALSE]

    silhouette_vec[i] <- compute_silhouette(data[i, ], center_point, other_centers, method)
  }

  simple_sil_mean <- mean(silhouette_vec)

  # If t == 0, return the simple silhouette score
  if (t == 0) {
    return(simple_sil_mean)
  }

  # Penalized Silhouette (t > 0)
  cluster_proportions <- table(clusters) / n
  inter_cluster_dissimilarity <- compute_inter_cluster_dissimilarities(center, k, method)

  # Penalization term
  penalization_term <- 1 - (min(k * cluster_proportions)^t * min(inter_cluster_dissimilarity)^t)

  # Compute the penalized silhouette score
  penalized_silhouette_mean <- simple_sil_mean - penalization_term
  return(penalized_silhouette_mean)
}

