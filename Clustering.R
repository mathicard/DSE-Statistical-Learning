dataset <- read.csv('usa_final.csv', sep=',')
rownames(dataset)<-dataset[,1]
dataset <- dataset[,2:19]


ds<-dist(scale(dataset))

agglo <- function(hc){
  data.frame(row.names=paste0("Cluster",seq_along(hc$height)),
             height=hc$height,
             compoments=ifelse(hc$merge<0,
        hc$labels[abs(hc$merge)], paste0("Cluster",hc$merge))
  )}

opar <- par(mfrow = c(1, 3))


par(mfrow=c(1,3)) 
#first clustering: average linkage
h1 <- hclust(ds,method="average")
agglo(h1)

plot(h1, main="Average linkage")

#add robustness to the analysis trying other methods
h2 <- hclust(ds,method="complete")
agglo(h2)
plot(h2, main="Complete linkage")

#Ward method can help us in reducing distances of outliers such as California
h3 <- hclust(ds,method="ward.D2")
agglo(h3)
plot(h3, main="Ward linkage")

#choice of number of clusters - elbow method
set.seed(123)
#compute and plot wss
wss <- sapply(1:12, 
              function(k){kmeans(ds, k, nstart=50,iter.max = 12 )$tot.withinss})
wss
plot(1:12, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

#the plot suggests the number k of clusters
cut_av <- cutree(h1, k = 8)
cut_compl <- cutree(h2, k = 8)
cut_ward <- cutree(h3, k = 8)

#confront the different clustering methods
table(cut_av,cut_compl)
table(cut_av,cut_ward)

plot(h3, main="Clusters with Ward linkage method")
rect.hclust(h3,8)

plot(dataset, col=cut_ward)
