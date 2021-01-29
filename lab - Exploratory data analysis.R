states <- row.names(USArrests)
states
names(USArrests)
apply(USArrests,2,mean)
apply(USArrests,2,var)
pr.out<-prcomp(USArrests,scale=TRUE)
names(pr.out)
pr.out$center
pr.out$scale
pr.out$rotation
dim(pr.out$x)
biplot(pr.out,scale=0)
pr.out$rotation <- -pr.out$rotation
pr.out$x <- -pr.out$x
biplot(pr.out,scale=0)
pr.out$sdev
pr.var <- pr.out$sdev^2
pr.var
pve <- pr.var/sum(pr.var)
pve
plot(pve,xlab="Principal Component",ylab="Propoertion of Variance Explained",ylim=c(0,1),type="b")
plot(cumsum(pve),xlab="Principal Component",ylab="Cumulative Propoertion of Variance Explained",ylim=c(0,1),type="b")


#Exercise 1
set.seed(100)
x <- matrix(rnorm(20 * 3 * 50, mean = 0, sd = 0.001), ncol = 50)
x[1:20, 2] <- 1
x[21:40, 1] <- 2
x[21:40, 2] <- 2
x[41:60, 1] <- 1
true.labels <- c(rep(1, 20), rep(2, 20), rep(3, 20))

#Exercise 2
pr.out <- prcomp(x)
plot(pr.out$x[, 1:2], col = 1:3, xlab = "Z1", ylab = "Z2", pch = 19)


#Exercise 3
km.out <- kmeans(x, 3, nstart = 20)
table(true.labels, km.out$cluster)

#EXERCISE 4:
km.out <- kmeans(pr.out$x[, 1:2], 3, nstart = 20)
table(true.labels, km.out$cluster)
