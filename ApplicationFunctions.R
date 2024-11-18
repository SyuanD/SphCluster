######################################
#Functions to calculate Silhouette values
# For given K and T
######################################

#compute_silhouette_scores<-function(data,rangeK,rangeT,method){
#  output<-matrix(data=NA,ncol = length(rangeK),nrow = length(rangeT))
#   for (i in 1:length(rangeT)) {
#    for (j in 1:length(rangeK)) {
#      res<-SphCluster(data,rangeK[j],method)
#      output[i,j]<-PSilhouette(data, clusters = res$cluster, center=res$prototypes, t = rangeT[i], method)
#    }
#  }
#  colnames(output)<-paste0('K=',rangeK)
#  rownames(output)<-paste0('T=',rangeT)
#  return(output)
#}

compute_silhouette_scores<-function(data,rangeK,rangeT,method){
  output<-matrix(data=NA,ncol = length(rangeK),nrow = length(rangeT))
  for (j in 1:length(rangeK)) {
    res<-SphCluster(data,rangeK[j],method)
    for (i in 1:length(rangeT)) {
      output[i,j]<-PSilhouette(data, clusters = res$cluster, center=res$prototypes, t = rangeT[i], method)
    }
  }
  colnames(output)<-paste0('K=',rangeK)
  rownames(output)<-paste0('T=',rangeT)
  return(output)
}

library(ggplot2)
library(reshape2)

# Function to plot silhouette scores
plot_silhouette_scores <- function(output_matrix,rangeK,rangeT) {
  # Convert the matrix to a data frame
  df <- data.frame(output_matrix)

  # Add columns for K and T
  df$Penalty <- row.names(output_matrix)
  df_long <- melt(df, id.vars = "Penalty", variable.name = "Number of Clusters", value.name = "silhouette_score")

  # Convert K to numeric based on the original rangeK vector
  df_long$"Number of Clusters"<- as.numeric(gsub("K.", "", df_long$"Number of Clusters"))
  df_long$"Number of Clusters" <- rangeK[df_long$"Number of Clusters"]
  df_long$"Number of Clusters"<-as.numeric( df_long$"Number of Clusters")

  # Plot using ggplot2
  ggplot(df_long, aes(x = df_long$"Number of Clusters", y = silhouette_score, group = Penalty, color = as.factor(Penalty))) +
    geom_line() +  # Draw lines connecting the points
    geom_point() +
    labs(x = "Number of Clusters", y = "Silhouette Score", color = "Penalty Values") +
    theme_minimal() +
    theme(legend.position = "right") +
    ggtitle("Silhouette Scores for Different Penalty Values and Number of Clusters")
}

# Function to obtain the optimal K that maximizes the silhouette score for each T
get_optimal_k <- function(output_matrix, rangeK, rangeT) {
  optimal_k <- numeric(length(rangeT))  # Initialize vector to store optimal K values

  for (i in 1:length(rangeT)) {
    # Extract the silhouette scores for the current T
    silhouette_scores <- output_matrix[i, ]

    # Find the index of the maximum silhouette score
    max_index <- which.max(silhouette_scores)

    # Get the corresponding K value from rangeK
    optimal_k[i] <- rangeK[max_index]
  }

  # Create a data frame for plotting
  plot_data <- data.frame(Penalty = rangeT, Optimal_K = optimal_k)

  # Generate the plot
  p <- ggplot(plot_data, aes(x = Penalty, y = Optimal_K)) +
    geom_line(color = "blue") +  # Connect the points with a line
    geom_point(size = 3, color = "red") +  # Plot the points
    labs(x = "Penalty", y = "Optimal K", title = "Optimal Number of Clusters  for Each Penalty Value") +
    theme_minimal()

  # Print the plot
  #print(p)

  # Return the vector of optimal K values and the plot object
  return(list(optimal_k = optimal_k, plot = p))
}




