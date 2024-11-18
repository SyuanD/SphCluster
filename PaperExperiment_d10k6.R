source('SphericalCluster.R')
source('PenalizedSilhouette.R')
source('ApplicationFunctions.R')

#####################
### d10k6
#####################

rangeK<-2:14
rangeT<-c(0,0.001,0.003,0.005,0.007,0.01)

### 15%
d10k6_15<-readRDS(file = '~/SphCluster/data/d10k6_15.rds')
d10k6_15_kmean<-matrix(data = NA,nrow =length(rangeT) ,ncol = 100)
d10k6_15_pc<-matrix(data = NA,nrow =length(rangeT) ,ncol = 100)
for (i in 1:100) {
  output_matrix<-compute_silhouette_scores(d10k6_15[[i]],rangeK,rangeT,method='k-means')
  d10k6_15_kmean[,i]<-apply(output_matrix, 1, which.max)
}

for (i in 1:100) {
  output_matrix<-compute_silhouette_scores(d10k6_15[[i]],rangeK,rangeT,method='k-pc')
  d10k6_15_pc[,i]<-apply(output_matrix, 1, which.max)
}

saveRDS(d10k6_15_kmean,file = 'd10k6_15_kmean.rds')
saveRDS(d10k6_15_kpc,file = 'd10k6_15_kpc.rds')

### 10%
d10k6_10<-readRDS(file = '~/SphCluster/data/d10k6_10.rds')
d10k6_10_kmean<-matrix(data = NA,nrow =length(rangeT) ,ncol = 100)
d10k6_10_pc<-matrix(data = NA,nrow =length(rangeT) ,ncol = 100)
for (i in 1:100) {
  output_matrix<-compute_silhouette_scores(d10k6_10[[i]],rangeK,rangeT,method='k-means')
  d10k6_10_kmean[,i]<-apply(output_matrix, 1, which.max)
}

for (i in 1:100) {
  output_matrix<-compute_silhouette_scores(d10k6_10[[i]],rangeK,rangeT,method='k-pc')
  d10k6_10_pc[,i]<-apply(output_matrix, 1, which.max)
}

saveRDS(d10k6_10_kmean,file = 'd10k6_10_kmean.rds')
saveRDS(d10k6_10_kpc,file = 'd10k6_10_kpc.rds')

### 5%
d10k6_5<-readRDS(file = '~/SphCluster/data/d10k6_5.rds')
d10k6_5_kmean<-matrix(data = NA,nrow =length(rangeT) ,ncol = 100)
d10k6_5_pc<-matrix(data = NA,nrow =length(rangeT) ,ncol = 100)
for (i in 1:100) {
  output_matrix<-compute_silhouette_scores(d10k6_5[[i]],rangeK,rangeT,method='k-means')
  d10k6_5_kmean[,i]<-apply(output_matrix, 1, which.max)
}

for (i in 1:100) {
  output_matrix<-compute_silhouette_scores(d10k6_5[[i]],rangeK,rangeT,method='k-pc')
  d10k6_5_pc[,i]<-apply(output_matrix, 1, which.max)
}

saveRDS(d10k6_5_kmean,file = 'd10k6_5_kmean.rds')
saveRDS(d10k6_5_kpc,file = 'd10k6_5_kpc.rds')

### 1%
d10k6_1<-readRDS(file = '~/SphCluster/data/d10k6_1.rds')
d10k6_1_kmean<-matrix(data = NA,nrow =length(rangeT) ,ncol = 100)
d10k6_1_pc<-matrix(data = NA,nrow =length(rangeT) ,ncol = 100)
for (i in 1:100) {
  output_matrix<-compute_silhouette_scores(d10k6_1[[i]],rangeK,rangeT,method='k-means')
  d10k6_1_kmean[,i]<-apply(output_matrix, 1, which.max)
}

for (i in 1:100) {
  output_matrix<-compute_silhouette_scores(d10k6_1[[i]],rangeK,rangeT,method='k-pc')
  d10k6_1_pc[,i]<-apply(output_matrix, 1, which.max)
}

saveRDS(d10k6_1_kmean,file = 'd10k6_1_kmean.rds')
saveRDS(d10k6_1_kpc,file = 'd10k6_1_kpc.rds')
