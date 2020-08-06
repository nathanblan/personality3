# islr labs

#pca ---------------------------------------------------------------------------
states = row.names(USArrests)
states
names(USArrests)
apply(USArrests, 2, mean)
apply(USArrests, 2, var)
pr.out=prcomp(USArrests , scale=TRUE)
names(pr.out)
pr.out$center
pr.out$scale
pr.out$rotation
biplot (pr.out , scale =0)
pr.out$rotation=-pr.out$rotation
pr.out$x=-pr.out$x
biplot (pr.out , scale =0)
pr.out$sdev
pr.var <- pr.out$sdev^2
pr.var
pve=pr.var/sum(pr.var)
pve
par(mfrow = c(2,2))
plot(pve , xlab=" Principal Component ", ylab="Proportion of Variance Explained ", 
     ylim=c(0,1),type='b')
plot(cumsum(pve), xlab="Principal Component ", ylab=" Cumulative PVE", 
     ylim=c(0,1), type='b')

# k means ----------------------------------------------------------------------
set.seed(2)
x=matrix(rnorm (50*2), ncol=2)
x[1:25,1]=x[1:25,1]+3
x[1:25,2]=x[1:25,2]-4
km.out=kmeans (x,2, nstart =20)
plot(x, col=(km.out$cluster +1), main="K-Means Clustering Results with K=2", 
     xlab="", ylab="", pch=20, cex=2)
set.seed(4)
km.2=kmeans (x,5, nstart =20) # arg2 signinifies no. of cluster sizes to try, 
                              # nstart signifies pool of cluster sizes to choose from
km.2