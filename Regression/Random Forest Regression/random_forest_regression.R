# Random Forest Regression

#denisse comment: the random forest is just a team of decision trees each one making some prediction of your dependent variable.
#                 The ultimate prediction of the random first itself is simply the average of the different predictions of all the different trees in the forest.

# Importing the dataset
dataset = read.csv('Position_Salaries.csv')
dataset = dataset[,2:3]


# Splitting the dataset into the Training set and Test set
# # install.packages('caTools')
# library(caTools)
# set.seed(123)
# split = sample.split(dataset$Salary, SplitRatio = 2/3)
# training_set = subset(dataset, split == TRUE)
# test_set = subset(dataset, split == FALSE)

# Feature Scaling
# training_set = scale(training_set)
# test_set = scale(test_set)

# Fitting Random Forest Regression to the dataset
#install.packages('randomForest')
library('randomForest')

set.seed(1234)
regressor = randomForest( x = dataset[1],     #returns a data frame (because we are taking a subset of dataframe)    #returns a vector
                          y = dataset$Salary, #returns a vector
                          ntree = 500)

# Predicting a new result with Random Forest Regression
y_pred = predict(regressor, newdata = data.frame(Level = 6.5))

# Visualising the Random Forest Regression results (higher resolution)
# install.packages('ggplot2')
library(ggplot2)

x_grid = seq(from = min(dataset$Level),
             to = max(dataset$Level),
             by = 0.01)
ggplot() +
  geom_point(aes(x=dataset$Level,
                 y=dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = x_grid,
                y = predict(regressor, data.frame(Level = x_grid))),
            colour = 'blue') +
  xlab('Level') +
  ylab('Salary') +
  ggtitle('Truth or Bluff (Random Forest Regression)')
