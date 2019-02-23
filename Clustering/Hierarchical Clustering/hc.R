# Hierarchical Clustering
#Denisse comment: there's two types of hierarchical clustering: agglomerative (bottom up approach) and divisive (top bottom aproach).
#Her will do agglomerative 


# Importing the dataset
dataset = read.csv('Mall_Customers.csv')
dataset = dataset[4:5]

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
# library(caTools)
# set.seed(123)
# split = sample.split(dataset$DependentVariable, SplitRatio = 0.8)
# training_set = subset(dataset, split == TRUE)
# test_set = subset(dataset, split == FALSE)

# Feature Scaling
# training_set = scale(training_set)
# test_set = scale(test_set)

# Using the dendrogram to find the optimal number of clusters
dendrogram = hclust(dist(dataset, method = "euclidean"),
                    method = "ward.D")
#Denisse comment: ward.d minimize variance instead of euclideand distance 
plot(dendrogram,
     main = "Dendrogram",
     xlab = "Customers",
     ylab = "Euclidean distances")
#Denisse Comment: I thing it should be 3 clusters, instructor think 5 clusters


# Fitting Hierarchical Clustering to the dataset
hc = hclust(dist(dataset, method = "euclidean"),
                    method = "ward.D")
y_hc = cutree(hc, k=5)


# Visualising the clusters
library(cluster)
clusplot(dataset,
         y_hc,
         lines =0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = "Clusters of Clients",
         xlab = "Anual Income",
         ylab = "Spending Score"
)
