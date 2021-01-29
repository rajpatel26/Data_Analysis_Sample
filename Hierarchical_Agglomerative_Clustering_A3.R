#Task1

#Function hclust inputs data and linkage method
hclust = function(v, linkage=c("single","complete","average","centroid"))
{
  #distance matrix
  mt=v
  mt=data.matrix(mt)
  p=apply(mt*mt,1,sum) 
  y=matrix(1.0,1,nrow(mt))
  z=p%*%y
  x=sqrt(abs(z + t(z) - 2 * mt %*% t(mt)))
  
  #if data not in matrix convert into matrix
  if(!is.matrix(x)) x = data.matrix(x)
  #switch as per input linkage method passed
  method_fn = switch(match.arg(linkage),
                     single   = min,
                     complete = max,
                     average  = mean)
  N = nrow(x)
  diag(x)=Inf
  a = -(1:N)  #grouping
  b = matrix(0,nrow=N-1, ncol=2) #merging matrix   
  h = rep(0,N-1)  #height of cluster                 
  for(j in seq(1,N-1))
  {
    h[j] = min(x)   #minimum
    i = which(x - h[j] == 0, arr.ind=TRUE)
    i = i[1,,drop=FALSE]
    p = a[i]
    p = p[order(p)]
    b[j,] = p
    grp = c(i, which(a %in% a[i[1,a[i]>0]])) #grouping
    a[grp] = j
    if (linkage=="centroid") #for centroid
    { 
      r = apply(x[i,],2,mean)
    }
    else{
      r = apply(x[i,],2,method_fn)
    }
    x[min(i),] = x[,min(i)] = r
    x[min(i),min(i)]        = Inf
    x[max(i),] = x[,max(i)] = Inf
  }
  
  structure(list(merge = b, height = h, order = seq_order(b),labels = trimws(unlist(strsplit(label, split = "\r\n"))), method = linkage,
                 call = match.call(), dist.method = "euclidean"),class = "hclust")
}

#Task 2
#Preparing or data to fit in hclust
att<-read.table("C:/Users/rraj2/Documents/nci.data.txt", header = FALSE, sep = "", dec = ".")
label<-readChar("C:/Users/rraj2/Documents/label.txt", file.info("C:/Users/rraj2/Documents/label.txt")$size)
att1 <- na.omit(att)

att2<-t(att1)

data1<-scale(att2)

source('other.R')

#Task3
#single linkage
sl = hclust(data1, linkage="single")
plot(sl)

#complete linkage
cl = hclust(data1, linkage="complete")
plot(cl)

#average linkage
al = hclust(data1, linkage="average")
plot(al)

#centroid linkage
ctl = hclust(data1, linkage="centroid")
plot(ctl)

#k=2
grp<-cutree(sl, k=2)
table(grp)
grp<-cutree(cl, 2)
table(grp)
grp<-cutree(al, 2)
table(grp)
grp<-cutree(ctl, 2)
table(grp)

#k=4
grp<-cutree(sl, 4)
table(grp)
grp<-cutree(cl, 4)
table(grp)
grp<-cutree(al, 4) 
table(grp)
grp<-cutree(ctl, 4) 
table(grp)

#k=6
grp<-cutree(sl, 6)
table(grp)
grp<-cutree(cl, 6)
table(grp)
grp<-cutree(al, 6)
table(grp)
grp<-cutree(ctl, 6)
table(grp)


#Task4
set.seed(26)
data2 <- scale(att2)

km.out1 <- kmeans(matrix(scale(data2)), 2, nstart=20)
km.out1$cluster
plot(scale(data2), col=(km.out1$cluster+1), main="K-Means Clustering with K=2",xlab="", ylab="")
table(km.out1$cluster)

km.out2 <- kmeans(matrix(scale(data2)), 4, nstart=20)
km.out2$cluster
plot(scale(data2), col=(km.out2$cluster+1), main="K-Means Clustering with K=4", xlab="", ylab="")
table(km.out2$cluster)

km.out3 <- kmeans(matrix(scale(data2)), 6, nstart=20)
km.out3$cluster
plot(scale(data2), col=(km.out3$cluster+1), main="K-Means Clustering with K=6", xlab="", ylab="")
table(km.out3$cluster)

km.out4 <- kmeans(matrix(scale(data2)), 8, nstart=20)
km.out4$cluster
plot(scale(data2), col=(km.out4$cluster+1), main="K-Means Clustering with K=8", xlab="", ylab="")
table(km.out4$cluster)

km.out5 <- kmeans(matrix(scale(data2)), 11, nstart=20)
km.out5$cluster
plot(scale(data2), col=(km.out5$cluster+1), main="K-Means Clustering with K=11", xlab="", ylab="")
table(km.out5$cluster)