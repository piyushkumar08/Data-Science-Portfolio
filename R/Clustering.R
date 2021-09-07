#CLUSTERING
colg <- read.csv("University_Data.csv", header = TRUE)

#Scaling to make our dataset normal i.e mean zero and sd 1 
#Removing the non-numeric columns
scale_colg <- scale(colg[,-1])

#Mean of each column
apply(scale_colg,MARGIN = 2, mean)

#Stdev 
apply(scale_colg,MARGIN = 2, sd)

#Calculating distance between the observations
colg_distance <- dist(scale_colg,method = 'euclidean')

#using LINKAGE to create hierarchy
hier_clust <- hclust(colg_distance,method = 'ward.D')

#Visualizing the hierarchical clusters
plot(hier_clust, labels = colg$Univ)

#Assigning cluster to observations (here dividing in 4 clusters)
colg$clust <- cutree(hier_clust,4)

#Partitive Clustering
kmeans_model <- kmeans(scale_colg,centers=4,iter.max=10)


# assign the obs to whichever cluster

kmeans_model$cluster



#centers

kmeans_model$centers



# total SS of the points inside the cluster

kmeans_model$tot.withinss


# clusters from 1-15
withinss <- sapply(1:15,function(x){kmeans(scale_colg,x,iter.max=10)$tot.withinss})


plot(1:15,withinss,xlab='no of clusters',ylab='total within ss',type='b')

#Wherever the graph becomes constant , take that point on x axis, thats the amount of optimum clusters

install.packages('factoextra')
library('factoextra')
fviz_nbclust(scale_colg,kmeans,method='wss')
