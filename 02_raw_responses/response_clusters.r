# classification models --------------------------------------------------------
#load libraries ----------------------------------------------------------------
library(ggfortify)

#set up clustering data --------------------------------------------------------
set.seed(1)
r_data_small <- r_data %>% 
  sample_frac(0.2) %>% 
  na.omit(r_data)

scale(r_data_small)
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
k_model <- r_data_small %>% 
  select(-id) %>% 
  kmeans(50, nstart = 50)

k_model$tot.withinss
# read islr and determine number of clusters we need, and how much of the data we really need to explain