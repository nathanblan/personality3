# classification models --------------------------------------------------------
#load libraries ----------------------------------------------------------------
library(ggfortify)
library(cluster)
library(fpc)
#set up clustering data --------------------------------------------------------
set.seed(2)
r_time_small <- r_time %>% 
  sample_frac(0.2) 

r_time_small <- na.omit(r_time_small)
r_time_small <- scale(r_time_small)
names(r_time_small)

#Principle Component Analysis -------------------------------------------------- 
time_S <- cov(r_time_small[,52:56]) #S matrix covariance
time_S.variance <- sum(diag(time_S)) #total variance or sum of eigenvalues of S
time_S.eigen <-  eigen(time_S) #compute eigenvalues and vectors of S
time_S.variance # view results of eigen() and variance
for (s in time_S.eigen$values) { #print eigenvalues
  print(s / sum(time_S.eigen$values))
}
plot(time_S.eigen$values, xlab = 'Eigenvalue Number', ylab = 'Eigenvalue Size', main = 'Scree Graph') +
  lines(time_S.eigen$values) 

#calculate pca 
r_time_small.pca <- prcomp(r_time_small[,52:56])
summary(r_time_small.pca)
names(r_time_small.pca)
#Principal Variance Explained
time.var <- r_time_small.pca$sdev
time_pve <- time.var/sum(time.var)
time_pve
#pca plot
time_pca.plot <- autoplot(r_time_small.pca, data = r_time_small, colour = 'id')
time_pca.plot
plot(time_pve, xlab=" Principal Component", ylab="Proportion of Variance Explained", 
     ylim=c(0,1),type='b')
plot(cumsum(time_pve), xlab="Principal Component", ylab="Cumulative Proportion of Variance Explained",
     ylim=c(0,1),
     type='b')

#initial model -----------------------------------------------------------------
# Determine number of clusters
wss <- (nrow(r_time_small)-1)*sum(apply(r_time_small,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(r_time_small,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

# K-Means Cluster Analysis
fit <- kmeans(r_time_small, 5) # 5 cluster solution
# get cluster means
aggregate(r_time_small,by=list(fit$cluster),FUN=mean)
# append cluster assignment
r_time_small <- data.frame(r_time_small, fit$cluster)

# K-Means Clustering with 5 clusters
fit <- kmeans(r_time_small, 3)
fit$tot.withinss
fit$withinss #The variation is *quite* large, maybe we didn't scale and center it right? 
