# classification models --------------------------------------------------------
#load libraries ----------------------------------------------------------------
library(ggfortify)
library(cluster)
library(fpc)
#set up clustering data --------------------------------------------------------
set.seed(1)
r_data_small <- r_data %>% 
  sample_frac(0.2) 

r_data_small <- na.omit(r_data_small)
r_data_small <- scale(r_data_small)
names(r_data_small)

#Principle Component Analysis -------------------------------------------------- 
S <- cov(r_data_small[,2:50]) #S matrix covariance
S.variance <- sum(diag(S)) #total variance or sum of eigenvalues of S
S.eigen <-  eigen(S) #compute eigenvalues and vectors of S
S.variance # view results of eigen() and variance
for (s in S.eigen$values) { #print eigenvalues
  print(s / sum(S.eigen$values))
}
plot(S.eigen$values, xlab = 'Eigenvalue Number', ylab = 'Eigenvalue Size', main = 'Scree Graph') +
  lines(S.eigen$values) 

R <- cor(r_data_small[,2:50]) #R matrix correlation
R.eigen <-eigen(R)
for (r in R.eigen$values) { #print eigenvalues
  print(r / sum(R.eigen$values))
}

#calculate pca 
r_data_small.pca <- prcomp(r_data_small[,2:50])
summary(r_data_small.pca)

#pca plot
pca.plot <- autoplot(r_data_small.pca, data = r_data_small, colour = 'id')
pca.plot

#initial model -----------------------------------------------------------------
# Determine number of clusters
wss <- (nrow(r_data_small)-1)*sum(apply(r_data_small,2,var))
 for (i in 2:15) wss[i] <- sum(kmeans(r_data_small,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

# K-Means Cluster Analysis
fit <- kmeans(r_data_small, 5) # 5 cluster solution
# get cluster means
aggregate(r_data_small,by=list(fit$cluster),FUN=mean)
# append cluster assignment
r_data_small <- data.frame(r_data_small, fit$cluster)

# K-Means Clustering with 5 clusters
fit <- kmeans(r_data_small, 5)

# Cluster Plot against 1st 2 principal components

# vary parameters for most readable graph
clusplot(r_data_small, fit$cluster, color=TRUE, shade=TRUE,
         labels=2, lines=0)

# Centroid Plot against 1st 2 discriminant functions
plotcluster(r_data_small, fit$cluster)