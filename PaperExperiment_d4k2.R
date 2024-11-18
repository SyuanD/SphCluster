source('SphericalCluster.R')
source('PenalizedSilhouette.R')
source('ApplicationFunctions.R')

#####################
### d4k2
#####################

rangeK<-2:14
rangeT<-c(0,0.001,0.003,0.005,0.007,0.01)

### 15%
d4k2_15<-readRDS(file = '~/SphCluster/data/d4k2_15.rds')
d4k2_15_kmean<-matrix(data = NA,nrow =length(rangeT) ,ncol = 100)
d4k2_15_pc<-matrix(data = NA,nrow =length(rangeT) ,ncol = 100)
for (i in 1:100) {
  output_matrix<-compute_silhouette_scores(d4k2_15[[i]],rangeK,rangeT,method='k-means')
  d4k2_15_kmean[,i]<-apply(output_matrix, 1, which.max)
}

for (i in 1:100) {
  output_matrix<-compute_silhouette_scores(d4k2_15[[i]],rangeK,rangeT,method='k-pc')
  d4k2_15_pc[,i]<-apply(output_matrix, 1, which.max)
}

saveRDS(d4k2_15_kmean,file = 'd4k2_15_kmean.rds')
saveRDS(d4k2_15_kpc,file = 'd4k2_15_kpc.rds')

### 10%
d4k2_10<-readRDS(file = '~/SphCluster/data/d4k2_10.rds')
d4k2_10_kmean<-matrix(data = NA,nrow =length(rangeT) ,ncol = 100)
d4k2_10_pc<-matrix(data = NA,nrow =length(rangeT) ,ncol = 100)
for (i in 1:100) {
  output_matrix<-compute_silhouette_scores(d4k2_10[[i]],rangeK,rangeT,method='k-means')
  d4k2_10_kmean[,i]<-apply(output_matrix, 1, which.max)
}

for (i in 1:100) {
  output_matrix<-compute_silhouette_scores(d4k2_10[[i]],rangeK,rangeT,method='k-pc')
  d4k2_10_pc[,i]<-apply(output_matrix, 1, which.max)
}

saveRDS(d4k2_10_kmean,file = 'd4k2_10_kmean.rds')
saveRDS(d4k2_10_kpc,file = 'd4k2_10_kpc.rds')

### 5%
d4k2_5<-readRDS(file = '~/SphCluster/data/d4k2_5.rds')
d4k2_5_kmean<-matrix(data = NA,nrow =length(rangeT) ,ncol = 100)
d4k2_5_pc<-matrix(data = NA,nrow =length(rangeT) ,ncol = 100)
for (i in 1:100) {
  output_matrix<-compute_silhouette_scores(d4k2_5[[i]],rangeK,rangeT,method='k-means')
  d4k2_5_kmean[,i]<-apply(output_matrix, 1, which.max)
}

for (i in 1:100) {
  output_matrix<-compute_silhouette_scores(d4k2_5[[i]],rangeK,rangeT,method='k-pc')
  d4k2_5_pc[,i]<-apply(output_matrix, 1, which.max)
}

saveRDS(d4k2_5_kmean,file = 'd4k2_5_kmean.rds')
saveRDS(d4k2_5_kpc,file = 'd4k2_5_kpc.rds')

### 1%
d4k2_1<-readRDS(file = '~/SphCluster/data/d4k2_1.rds')
d4k2_1_kmean<-matrix(data = NA,nrow =length(rangeT) ,ncol = 100)
d4k2_1_pc<-matrix(data = NA,nrow =length(rangeT) ,ncol = 100)
for (i in 1:100) {
  output_matrix<-compute_silhouette_scores(d4k2_1[[i]],rangeK,rangeT,method='k-means')
  d4k2_1_kmean[,i]<-apply(output_matrix, 1, which.max)
}

for (i in 1:100) {
  output_matrix<-compute_silhouette_scores(d4k2_1[[i]],rangeK,rangeT,method='k-pc')
  d4k2_1_pc[,i]<-apply(output_matrix, 1, which.max)
}

saveRDS(d4k2_1_kmean,file = 'd4k2_1_kmean.rds')
saveRDS(d4k2_1_kpc,file = 'd4k2_1_kpc.rds')
