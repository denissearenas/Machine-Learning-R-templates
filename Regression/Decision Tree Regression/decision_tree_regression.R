# Decision Tree Regression

# Importing the dataset
dataset = read.csv('Position_Salaries.csv')
dataset = dataset[2:3]

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

#Denisse note:  we don't need to do any feature scaling because the way this model is built is based on conditions on the independent variable and not on Euclidean distances.

# Fitting Decision Tree Regression to the dataset
install.packages('rpart')
library(rpart)

regressor = rpart(formula = Salary ~ ., 
                  data = dataset,
                  control = rpart.control(minsplit = 1) )
#minsplit = the minimum number of observations that must exist in a node in order for a split to be attempted.
summary(regressor)

# Predicting a new result with Decision Tree Regression
y_pred = predict(regressor, newdata = data.frame(Level = 6.5))

# Visualising the Decision Tree Regression results (higher resolution)
# install.packages('ggplot2')
library(ggplot2)

# Plotting the tree

ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'Red') +
  geom_line(aes(x = dataset$Level , y = predict(regressor, dataset)),
            colour = 'Blue') +
  xlab('Levels') +
  ylab('Salary') +
  ggtitle('Truth or Bluff (Decision Tree Regression)')


x_grid = seq( from = min(dataset$Level), 
              to = max(dataset$Level),
              by = 0.01)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'Red') +
  geom_line(aes( x = x_grid , 
                 y = predict(regressor, data.frame(Level = x_grid) )),
            colour = 'Blue') +
  xlab('Level') +
  ylab('Salary') +
  ggtitle('Truth or Bluff (Decision Tree Regression)')
