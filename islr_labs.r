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
par(mfrow = c(1,2))
plot(pve , xlab=" Principal Component ", ylab="Proportion of Variance Explained ", 
     ylim=c(0,1),type='b')
plot(cumsum(pve), xlab="Principal Component ", ylab=" Cumulative PVE", 
     ylim=c(0,1), type='b')

# k means ----------------------------------------------------------------------
