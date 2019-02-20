# Simple Linear Regression

# Importing the dataset
dataset = read.csv('Salary_Data.csv')

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Salary, SplitRatio = 2/3)  #usually 75% is a good split, but we want 20 and 10
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)


#So we are trying to understand if there is a correlation between the salary and the number of years and mostly we're trying to see if it's a linear correlation.
#we are trying to predict the dependent variable salary based on the informations of the independent variable Years of experience.

#we dont need feature scaling for this, because the library takes care of that.

#FITING SIMPLE LINEAR REGRESSION TO THE TRAINING SET
regressor = lm( formula = Salary ~ YearsExperience,
               data = training_set)
#Salary ~ YearsExperience : salary is proportional to years experience.

summary(regressor)# to get impotant information about our fitting

# Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        25592       2646   9.672 1.49e-08 ***
# YearsExperience     9365        421  22.245 1.52e-14 ***
#   ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Residual standard error: 5391 on 18 degrees of freedom
# Multiple R-squared:  0.9649,	Adjusted R-squared:  0.963 
# F-statistic: 494.8 on 1 and 18 DF,  p-value: 1.524e-14


#we observe three stars here. 
#That means the years experience independent variable is highly statistically significant because you can either have no star or one star two stars three stars.
#No star it means that there is no statistical significance and three stars means that there is a high statistical significance.

#the other info here is the P-value and the P-value is another indicator of the statistical significance because the lower the p value (Pr(>|t|) ) is the more significant your independent variable is going to be.
#And usually a good threshold for the P-value is five percent which means that when you are below 5 percent the independent variable is highly significant.
#And when we are over 5 percent that means that it's less significant.


#PREDICTING THE TEST SET RESULT
#creating our vector of predictions
y_pred = predict(regressor, newdata = test_set)

#VISUALISING THE TRAINING SET RESULTS
#install.packages('ggplot2')
library(ggplot2)

ggplot() +
  geom_point(aes(x = training_set$YearsExperience, y = training_set$Salary),
             colour = 'red') +      #to scater plot all the observations of our training_set
  geom_line(aes(x = training_set$YearsExperience, y = predict(regressor, training_set) ),
            colour = 'blue') +        #to  plot the reggresion line (for the Y we want to plor the predicted values so ge get the straight line)
  geom_smooth(data= training_set, aes(x=YearsExperience, y= Salary), method = lm) + #this is not necesary. It will add the 95% confidence range
  ggtitle('Salary vs Experience (Training set)') +
  xlab('Years of Experience') +   # x label
  ylab('Salary')                 # y label


##geom came from geometrical


#VISUALISING THE TEST SET RESULTS
ggplot() +
  geom_point(aes(x = test_set$YearsExperience, y = test_set$Salary),
             colour = 'red') +      #to scater plot all the observations of our training_set
  geom_line(aes(x = training_set$YearsExperience, y = predict(regressor, training_set) ),
            colour = 'blue') +        #to  plot the reggresion line (for the Y we want to plor the predicted values so ge get the straight line)
  ggtitle('Salary vs Experience (test_set)') +
  xlab('Years of Experience') +   # x label
  ylab('Salary')                 # y label



#### MY DOING (blue dots training set, red dots test set)

ggplot() +
  geom_point(aes(x = training_set$YearsExperience, y = training_set$Salary),
             colour = 'blue') +      #to scater plot all the observations of our training_set
  geom_point(aes(x = test_set$YearsExperience, y = test_set$Salary),
             colour = 'red') +      #to scater plot all the observations of our training_set
  geom_line(aes(x = training_set$YearsExperience, y = predict(regressor, training_set) ),
            colour = 'blue') +        #to  plot the reggresion line (for the Y we want to plor the predicted values so ge get the straight line)
  ggtitle('Salary vs Experience (Training set)') +
  xlab('Years of Experience') +   # x label
  ylab('Salary')  
