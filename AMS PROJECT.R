#### LOADING REQUIRED PACKAGES
library(factoextra)
library(cluster)
library(stats)
library(klustR)
### LOADING THE DATASET
data<- read.csv("/Users/saineymanga/Desktop/Country-data.csv")
attach(data)
head(data)
any(is.na(data)) #### FINDING OUT IF THERE ARE MISSING DATA
row.names(data)<-data$country
data$country<- NULL
data

###############################  K-MEANS CLUSTERING########################################
set.seed(123)
df<-scale(data)
### THE ELBOW METHOD
fviz_nbclust(df, kmeans, method = "wss") +
  geom_vline(xintercept = 5, linetype = 2) + # add line for better visualisation
  labs(subtitle = "Elbow method") # add subtitle
### Compute k-means clustering with k = 5, FROM THE ELBOW METHOD
set.seed(123)
K <- kmeans(df, 5, nstart = 25)
print(K)
fviz_cluster(K, data = df)
# compute gap statistic method
set.seed(123)
gap_stat <- clusGap(df, kmeans , nstart = 25,
                    K.max = 10, B = 50)
fviz_gap_stat(gap_stat)
K <- kmeans(df, 3, nstart = 25) ## FROM THE GAP METHOD
fviz_cluster(K, data=df)

#### THE SILHOUETTE METHOD FOR OPTIMAL CLUSTERS
fviz_nbclust(df, kmeans, method = "silhouette")### ALSO SHOWS A 5 OPTIMAL CLUSTER


##### PACOPLOT k-means#### parallel coordinates
clus <- kmeans(df, 5)$cluster
pacoplot(df, clus)


##################################################### MODEL BASED ########################################
library("ggpubr")
library("mclust")
set.seed(123)
mc<- Mclust(df)
summary(mc)
####### calling some properties#####
mc$modelName                # Optimal selected model ==> "VVV"
mc$G                        # Optimal number of cluster => 3
head(mc$z, 10)              # Probality to belong to a given cluster
head(mc$classification, 50) # Cluster assignement of each observation

# BIC values used for choosing the number of clusters
fviz_mclust(mc, "BIC", palette = "jco")
# Classification: plot showing the clustering
fviz_mclust(mc, "classification", geom=c("text","point"),
            pointsize = 1.5, palette = "jco", repel=FALSE)

# Classification uncertainty
fviz_mclust(mc, "uncertainty", palette = "jco")

