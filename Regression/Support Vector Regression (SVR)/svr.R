# SVR

# Importing the dataset
dataset = read.csv('Position_salaries.csv')
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

# Fitting SVR to the dataset
#install.packages('e1071')
library(e1071)

regressor = svm( Salary ~ . ,
                 data = dataset,
                 type = 'eps-regression')

#type: specify if you're making an SVM model which is used for classification (C-classification) or SVR model which  is used for regression (eps-regression).


# Predicting a new result
y_pred = predict(regressor, newdata = data.frame( Level = 6.5))



# Visualising the SVR results
# install.packages('ggplot2')
library(ggplot2)


# Visualising the SVR results (for higher resolution and smoother curve)
# install.packages('ggplot2')

ggplot() +
  geom_point(aes( x = dataset$Level, y = dataset$Salary ),
             colour = 'red') +
  geom_line(aes( x = dataset$Level, y = predict(regressor, newdata = dataset ) ),
            colour = 'blue') +
  xlab('Lavel') +
  ylab('Salary') +
  ggtitle('Truth or Bluff (SVR)')




