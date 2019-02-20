dataset =read.csv('50_startups.csv')

dataset$State = factor(dataset$State, 
                       levels = c('New York','California', 'Florida' ),
                       labels = c(1,2,3))
regressor = lm(formula = Profit ~ .,
               data = dataset) 

#denisse doing:
#different ways of getting the coefficients

coef(summary(regressor))

x = summary(regressor)
x2 = x["coefficients"]  #creates a list of 1 (kind of useless really)

x2 = x[["coefficients"]] #creates a matrix
x2 = x$coefficients #creates a matrix so I can do this to get rid of the intersect x$coefficients[-1,4]


x2 = summary(regressor)$coefficients
x4 = coef(summary(regressor)) #this is different to x["coefficients"] because it creates a matrix with column and rows names as the one on the coeficient. Making it easier to query. I's the same than summary(regressor)$coefficients

coef(summary(regressor))[-1, "Pr(>|t|)"]
x4 = coef(summary(regressor))

#conclusion coef(summary(regressor)) is the same than summary(regressor)$coefficients



#Automatic Backward Elimination
#TO DO: I would like to find a way of not using Profit in the formula and use the position instead
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