library(MASS)
library(ggplot2)
library(plotly)

#Create fit from sample data
item.dist<-dist(SampleData1)
fit<-isoMDS(item.dist,k=2)
fit

# plot solution 
x <- fit$points[,1]
y <- fit$points[,2]
plot(x, y)
text(x, y, labels = row.names(SampleData), cex=.7)

#kmeans
dist.sample<-data.frame(cbind(x,y))
cluster.sample<-kmeans(dist.sample,5)
cluster.sample$cluster <- as.factor(cluster.sample$cluster)
plot<-ggplot(dist.sample, aes(x, y, color = cluster.sample$cluster)) + geom_point(size=10)
ggplotly(plot)

#hierarchical cluster
mds.dist<-dist(dist.sample)
hc<-hclust(mds.dist) 
plot(hc)
groups<-cutree(hc,k=8)

#Print HCA clusters (function from Practical Data Science in R)
print_clusters<-function(labels,k) {
  for(i in 1:k) {
    print(paste("cluster",i))
    print(SampleData1[labels==i,c("Type","Score")])
  }
}
print_clusters(groups,8)
