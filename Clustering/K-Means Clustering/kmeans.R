# K-Means Clustering

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

# Using the elbow method to find the optimal number of clusters
set.seed(6)
wcss = vector()
for(i in 1:10) {
  wcss[i] = sum(kmeans(dataset,i)$withinss)
}

plot(
  x = 1:10,
  y = wcss,
  type = "b",
  main = "wcss vs Number of clusters",
  xlab = "Number of clusters",
  ylab = "WCSS"
)
#5 clusters :D


# Fitting K-Means to the dataset
set.seed(29)

kmeans = kmeans(dataset, 
                5,
                iter.max = 300,
                nstart = 10) 
#Denisse comment: nstart option attempts multiple initial configurations and reports on the best one. 
#For example, adding nstart=25 will generate 25 initial configurations.


# Visualising the clusters
library(cluster)
clusplot(dataset,
         kmeans$cluster,
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
