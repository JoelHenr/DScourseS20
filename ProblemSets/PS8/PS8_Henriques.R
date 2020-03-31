library(mlr)
library(tidyverse)
library(magrittr)
library(glmnet)

# Load the Housing data from UCI
housing <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data")
names(housing) <- c("crim","zn","indus","chas","nox","rm","age","dis","rad","tax","ptratio","b","lstat","medv")

# Adding a new feature to the data
housing$lmedv <- log(housing$medv)
housing$medv  <- NULL # We are dropping a median value 
formula <- as.formula(lmedv ~ .^3 + 
                        poly(crim, 6) + 
                        poly(zn, 6) + 
                        poly(indus, 6) + 
                        poly(rm, 6) + 
                        poly(age, 6) + 
                        poly(dis, 6) + 
                        poly(rad, 6) +
                        poly(tax, 6) + 
                        poly(ptratio, 6) + 
                        poly(b, 6) + 
                        poly(lstat, 6))
mod_matrix <- data.frame(model.matrix(formula, housing))

# Now replace the intercept column by the response since MLR will do 
# "y ~ ." and get the intercept by default 
mod_matrix [, 1] = housing$lmedv 
colnames(mod_matrix)[1] = "lmedv" # Make sure to rename it otherwise MLR wont find it 
head(mod_matrix) # Just make sure everything is hunky-dory 

# Break up the data: 
n <- nrow(mod_matrix) 
train <- sample(n, size = .8*n) 
test <- setdiff(1:n, train) 
housing.train <- mod_matrix[train,]
housing.test <- mod_matrix[test,]

# This is the Lasso Model

# Defining a Task 
theTask <- makeRegrTask(id = "FindLMedianPrice", data = housing.train, target = "lmedv")
print(theTask)

# Tell it a new prediction algorithm
Alg <- makeLearner("regr.glmnet")

# Printing an out of sample rmse 
get.rmse <- function(y1,y2)
{
  return(sqrt(mean((y1-y2)^2)))
}

# Setting the resampling strategy which will be 6-folds 
RSStrat <- makeResampleDesc(method = "CV", iter = 6)

# Search over penalty parameter lambda and force elastic net parameter to be 1 (LASSO)
MP <- makeParamSet(makeNumericParam("lambda",lower=0,upper=1),makeNumericParam("alpha",lower=1,upper=1))

# Taking 200 random guesses at lambda within the interval I specified above
TM <- makeTuneControlRandom(maxit = 200L)

# Do the tuning
TMD <- tuneParams(learner = Alg,
                         task = theTask,
                         resampling = RSStrat,
                         measures = rmse,      
                         par.set = MP,
                         control = TM,
                         show.info = TRUE)

# Now applying the optimal algorithm parameters to this model 
Alg <- setHyperPars(learner = Alg, par.vals = TMD$x)

# Doing cross validation on sample sets 
resample(Alg,theTask,RSStrat,measures = list(rmse)) 

# Train the model 
finalM <- train(learner = Alg, task = theTask)

# Predict in the test set 
prediction <- predict(finalM, newdata = housing.test)

# Showing the RMSE
print(head(prediction$data))
print(get.rmse(prediction$data$truth,prediction$data$response)) 

# Trained parameter estimates 
getLearnerModel(finalM)$beta 

# This is the Ridge Model 

# Redoing the Model parameters and setting an alpha of zero
MP <- makeParamSet(makeNumericParam("lambda",lower=0,upper=1),
                            makeNumericParam("alpha",lower=0,upper=0))
# Redoing the tuning again 
TMD <- tuneParams(learner = Alg,
                         task = theTask,
                         resampling = RSStrat,
                         measures = rmse,       
                         par.set = MP,
                         control = TM,
                         show.info = TRUE)

# Appplying the optimal alg. par. to our model 
Alg <- setHyperPars(learner=Alg, par.vals = TMD$x) 

# Verifying with cross validation for sampl sets 
resample(Alg, theTask, RSStrat, measures = list(rmse)) 

# Time to train the model and assigning finalM
finalM <- train(learner=Alg, task = theTask)

# Predict test set and reassigning prediction variable 
prediction <- predict(finalM, newdata = housing.test)

# Show the RMSE Results
print(get.rmse(prediction$data$truth,prediction$data$response))


# The trained estimate results 
getLearnerModel(finalM)$beta

#elastic net 

# Redoing the Model parameters and setting an alpha between 0 and 1
MP <- makeParamSet(makeNumericParam("lambda",lower=0,upper=1),makeNumericParam("alpha",lower=0,upper=1))

# Redoing the tuning again 
TMD <- tuneParams(learner = Alg,
                  task = theTask,
                  resampling = RSStrat,
                  measures = rmse,       
                  par.set = MP,
                  control = TM,
                  show.info = TRUE)

# Appplying the optimal alg. par. to our model 
Alg <- setHyperPars(learner=Alg, par.vals = TMD$x) 

# Verifying with cross validation for sampl sets 
resample(Alg, theTask, RSStrat, measures = list(rmse)) 

# Time to train the model and assigning finalM
finalM <- train(learner=Alg, task = theTask)

# Predict test set and reassigning prediction variable 
prediction <- predict(finalM, newdata = housing.test)

# Show the RMSE Results
print(get.rmse(prediction$data$truth,prediction$data$response))
