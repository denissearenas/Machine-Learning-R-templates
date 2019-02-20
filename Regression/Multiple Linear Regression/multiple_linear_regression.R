# Multiple Linear Regression

# Importing the dataset
dataset =read.csv('50_startups.csv')


# Encoding categorical data
dataset$State = factor(dataset$State, 
                       levels = c('New York','California', 'Florida' ),
                       labels = c(1,2,3))


# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Profit, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)


# Feature Scaling
# training_set = scale(training_set)
# test_set = scale(test_set)

# Fitting Multiple Linear Regression to the Training set
# the profit is going to be a linear combination of the independent variable 
#regressor = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + State)  #we can simplify with a .
regressor = lm(formula = Profit ~ .,
               data = training_set) #it means: you want to express the Profit as a linear combinarion of all the independent variables

summary(regressor)

#Call:
#  lm(formula = Profit ~ ., data = training_set)
#
#Residuals:
#  Min     1Q Median     3Q    Max 
#-33128  -4865      5   6098  18065 
#
#Coefficients:
#                   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)      4.965e+04  7.637e+03   6.501 1.94e-07 ***
# R.D.Spend        7.986e-01  5.604e-02  14.251 6.70e-16 ***          #this is the only variable with statistical significan
# Administration  -2.942e-02  5.828e-02  -0.505    0.617    
# Marketing.Spend  3.268e-02  2.127e-02   1.537    0.134    
# State2           1.213e+02  3.751e+03   0.032    0.974    
# State3           2.376e+02  4.127e+03   0.058    0.954              #the library took care of the dummy variables for us because they where factors. Also it remove 1 of them (to avoid dummy vaiable trap)
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 9908 on 34 degrees of freedom
# Multiple R-squared:  0.9499,	Adjusted R-squared:  0.9425 
# F-statistic:   129 on 5 and 34 DF,  p-value: < 2.2e-16

#denisse notes:
#Estimate: coefficient in the linear regression equation.
#p-value: Pr(>|t|)


# Predicting the Test set results
y_pred = predict(regressor, newdata = test_set)

#Building the optimal model using Backward Elimination
regressor = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + State,
               data = dataset )
#We can actually do backward elimination using the training set but we're just taking the whole dataset in order to have complete informations about which independent variables are statistically significant

#we'll select a sicnificance level of 5%. SL = 0.05
summary(regressor)


#removing State. He removed state completelly. I would like to check later how to remove 1 dummy vatiable at the time
regressor = lm( formula = Profit ~ R.D.Spend + Administration + Marketing.Spend,
                data = dataset)
summary(regressor)

#removing administartion
regressor = lm( formula = Profit ~ R.D.Spend + Marketing.Spend,
                data = dataset)
summary(regressor)

#removing marketing spend. it's 6% significance, but i'm scare. If we decide 5 I'll stick with that
regressor = lm( formula = Profit ~ R.D.Spend,
                data = dataset)
summary(regressor)

#later will see how to use the extra info at the bottom to take a better decition if tho remove marketing spend or not


#Automatic Backward Elimination
backwardEliminationDenisse <- function(x, sl) {
  numVars = length(x)
  for (i in c(1:numVars)){
    regressor = lm(formula = Profit ~ ., data = x)
    maxVar = max(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"])
    if (maxVar > sl){
      j = which(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"] == maxVar)
      x = x[, -j]
    }
    numVars = numVars - 1
  }
  return(summary(regressor))
}

#Denisse:
backwardElimination <- function(x, sl){
  numVars = length(x)
  for(i in c(1:numVars)){
    print(i)
    regressor = lm(formula = Profit ~ .,
                   data = x)
    maxVars = max(coef(summary(regressor))[c(2:numVars),"Pr(>|t|)"])
    if(maxVars > sl){
      j = which(coef(summary(regressor))[c(2:numVars),"Pr(>|t|)"] == maxVars)
      x = x[,-j]
    }else{  #I've added this to not continue looping if not necesary
      break
    }
    numVars = numVars - 1
    
  }
  return(summary(regressor))
}

SL = 0.05
dataset2 = dataset[,c(1,2,3,4,5)]
backwardElimination(dataset2,SL)
