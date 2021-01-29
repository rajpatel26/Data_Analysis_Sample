rec <- function(x) (abs(x) < 1) * 0.5
tri <- function(x) (abs(x) < 1) * (1 - abs(x))
gauss <- function(x) 1/sqrt(2*pi) * exp(-(x^2)/2)
x <- seq(from = -3, to = 3, by = 0.001)
plot(x, rec(x), type = "l", ylim = c(0,1), lty = 1,
       ylab = expression(K(x)))
lines(x, tri(x), lty = 2)
lines(x, gauss(x), lty = 3)
legend(-3, 0.8, legend = c("Rectangular", "Triangular", "Gaussian"),
         lty = 1:3, title = "kernel functions", bty = "n")

x <- c(0, 1, 1.1, 1.5, 1.9, 2.8, 2.9, 3.5)
n <- length(x)
xgrid <- seq(from=min(x)-1, to=max(x)+1, by=0.01)
h <- 0.4
bumps <- sapply(x, function(a) gauss((xgrid-a)/h)/(n*h))
plot(xgrid, rowSums(bumps), ylab=expression(hat(f)(x)),
       type = "l", xlab = "x", lwd = 2)
rug(x, lwd=2)
out <- apply(bumps, 2, function(b) lines(xgrid, b))

set.seed(2)
x <- matrix(rnorm(50*2),ncol=2)
x[1:25,1] <- x[1:25,1]+3
x[1:25,2] <- x[1:25,2]-4

km.out <- kmeans(x, 2, nstart=20)
km.out$cluster
plot(x, col=(km.out$cluster+1), main="K-Means Clustering Results
with K=2", xlab="", ylab="", pch=20, cex=2)


set.seed(4)
km.out <- kmeans(x, 3, nstart=20)
km.out

plot(x, col=(km.out$cluster+1), main="K-Means Clustering Results
with K=3", xlab="", ylab="", pch=20, cex=2)

set.seed(3)
km.out <- kmeans(x, 3, nstart=1)
km.out$tot.withinss

km.out <- kmeans(x, 3, nstart=20)
km.out$tot.withinss

hc.complete <- hclust(dist(x), method="complete")
hc.average <- hclust(dist(x), method="average")
hc.single <- hclust(dist(x), method="single")

par(mfrow=c(1,3))
plot(hc.complete,main="Complete Linkage",xlab="",sub="",cex=.9)
plot(hc.average,main="Average Linkage",xlab="",sub="",cex=.9)
plot(hc.single,main="Single Linkage",xlab="",sub ="",cex =.9)

cutree(hc.complete, 2)
cutree(hc.average, 2)
cutree(hc.single, 2)
cutree(hc.single, 4)
xsc <- scale(x, center=FALSE, scale=TRUE)
plot(hclust(dist(xsc),method="complete"),main="Hierarchical Clustering
with Scaled Observations")

xsc <- scale(x)
plot(hclust(dist(xsc),method="complete"),main="Hierarchical Clustering
with Scaled Observations")

#Exercise 1

x <- c(0, 1, 1.1, 1.5, 1.9, 2.8, 2.9, 3.5)
n <- length(x)
xgrid <- seq(from=min(x)-1, to=max(x)+1, by=0.01)
h <- 0.8
bumps <- sapply(x, function(a) gauss((xgrid-a)/h)/(n*h))
plot(xgrid, rowSums(bumps), ylab=expression(hat(f)(x)),
       type = "l", xlab = "x", lwd = 2)
rug(x, lwd=2)
out <- apply(bumps, 2, function(b) lines(xgrid, b))

#Exercise 2
set.seed(2)
x <- matrix(rnorm(20 * 3 * 50, mean = 0, sd = 0.001), ncol = 50)
x[1:20, 2] <- 1
x[21:40, 1] <- 2
x[21:40, 2] <- 2
x[41:60, 1] <- 1
true.labels <- c(rep(1, 20), rep(2, 20), rep(3, 20))
pr.out <- prcomp(x)
plot(pr.out$x[, 1:2], col = 1:3, xlab = "Z1", ylab = "Z2", pch = 19)

km.out <- kmeans(x, 3, nstart = 20)
table(true.labels, km.out$cluster)
#perfectly clustered

km.out <- kmeans(x, 2, nstart = 20)
table(true.labels, km.out$cluster)
#All observations of one of the three clusters is now absorbed in one of the two clusters.

km.out <- kmeans(x, 4, nstart = 20)
table(true.labels, km.out$cluster)
#The first cluster is splitted into two clusters.

#Exercise 3
library(ISLR)
set.seed(1)
dsc <- scale(USArrests)
d1 <- dist(dsc)^2
d2 <- as.dist(1 - cor(t(dsc)))
summary(d2 / d1)
pr.out <- prcomp(USArrests, scale = TRUE)
pr.var <- pr.out$sdev^2
pve <- pr.var / sum(pr.var)
sum(pr.var)
pve
loadings <- pr.out$rotation
USArrests2 <- scale(USArrests)
sumvar <- sum(apply(as.matrix(USArrests2)^2, 2, sum))
apply((as.matrix(USArrests2) %*% loadings)^2, 2, sum) / sumvar
set.seed(2)
hc.complete <- hclust(dist(USArrests), method = "complete")
plot(hc.complete)
cutree(hc.complete, 3)
sd.data <- scale(USArrests)
hc.complete.sd <- hclust(dist(sd.data), method = "complete")
plot(hc.complete.sd)
cutree(hc.complete.sd, 3)
table(cutree(hc.complete, 3), cutree(hc.complete.sd, 3))
#Scaling the variables affect the clusters obtained although the trees are somewhat similar. The variables should be scaled beforehand because the data measures have different units.


