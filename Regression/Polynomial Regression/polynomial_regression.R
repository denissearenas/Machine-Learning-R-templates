# Polynomial Regression

# Importing the dataset
dataset = read.csv('Position_Salaries.csv')
dataset = dataset[,2:3] ##denisse: we are not usion the position because the level it like an encoding of the position

# Splitting the dataset into the Training set and Test set
# # install.packages('caTools')
# library(caTools)
# set.seed(123)
# split = sample.split(dataset$Salary, SplitRatio = 2/3)
# training_set = subset(dataset, split == TRUE)
# test_set = subset(dataset, split == FALSE)
#denisse comment: will not split it in to training set and dataset because is to small and just want to get an idea how polynomial regression works

# Feature Scaling
# training_set = scale(training_set)
# test_set = scale(test_set)

#Denisse: we are going to compare the lineal model with the polynomial model
# Fitting Linear Regression to the dataset
lin_reg = lm( formula = Salary ~ Level,
              data = dataset)
summary(lin_reg)

# Fitting Polynomial Regression to the dataset
dataset$Level2 = dataset$Level^2
dataset$Level3 = dataset$Level^3
poly_reg = lm( formula = Salary ~ .,
               data = dataset)
#Note: a polynomial regression model is a multiple regression model that is composed of one independent variable and additional independent variables that are pulling in terms of this first independent variable
summary(poly_reg)

# Visualising the Linear Regression results
#install.packages('ggplot2')
library(ggplot2)

ggplot() +
  geom_point(aes( x = dataset$Level  , y = dataset$Salary ),
             color = 'red') + 
  geom_line( aes( x = dataset$Level, y = predict( lin_reg, newdata = dataset ) ),
             color = 'blue') +
  ggtitle('Truth or Bluff (Linear Refression)') + 
  xlab('Lavel') + 
  ylab('Salary')

# Visualising the Polynomial Regression results
# install.packages('ggplot2')
ggplot() +
  geom_point(aes( x = dataset$Level  , y = dataset$Salary ),
             color = 'red') + 
  geom_line( aes( x = dataset$Level, y = predict( poly_reg, newdata = dataset ) ),
             color = 'blue') +
  ggtitle('Truth or Bluff (Polynomial Refression)') + 
  xlab('Lavel') + 
  ylab('Salary')


#denisse note: adding a 4 level
dataset$Level4 = dataset$Level^4
poly_reg = lm( formula = Salary ~ .,
               data = dataset)
summary(poly_reg)
ggplot() +
  geom_point(aes( x = dataset$Level  , y = dataset$Salary ),
             color = 'red') + 
  geom_line( aes( x = dataset$Level, y = predict( poly_reg, newdata = dataset ) ),
             color = 'blue') +
  ggtitle('Truth or Bluff (Polynomial Refression)') + 
  xlab('Lavel') + 
  ylab('Salary')

# Visualising the Regression Model results (for higher resolution and smoother curve)
#denisse note: same plot than before but predicting more points so the line looks smoother
# install.packages('ggplot2')
x_grid = seq(min(dataset$Level), max(dataset$Level), 0.1)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = x_grid, y = predict(poly_reg,
                                        newdata = data.frame(Level = x_grid,
                                                             Level2 = x_grid^2,
                                                             Level3 = x_grid^3,
                                                             Level4 = x_grid^4))),
            colour = 'blue') +
  ggtitle('Truth or Bluff (Polynomial Regression)') +
  xlab('Level') +
  ylab('Salary')



# Predicting a new result with Linear Regression
y_pred = predict(lin_reg, newdata = data.frame( Level = 6.5))


# Predicting a new result with Polynomial Regression

y_pred = predict(poly_reg, newdata = data.frame( Level = 6.5, 
                                                 Level2 = 6.5^2, 
                                                 Level3 = 6.5^3, 
                                                 Level4 = 6.5^4 ))
