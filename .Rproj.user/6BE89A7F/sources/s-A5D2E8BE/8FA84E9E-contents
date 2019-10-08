#link: https://machinelearningmastery.com/feature-selection-with-the-caret-r-package/

#RANK BY IMPORTANCE OF ATTRIBUTES/FEATURES----

set.seed(7)
# load the library
library(mlbench)
library(caret)
# load the dataset
data(datafile)
# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model <- train(diabetes~., data=nameofdata, method="lvq", preProcess="scale", trControl=control)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)

#FEATURE SELECTION----
#Automatic feature selection methods can be used to build many models with 
#different subsets of a dataset and identify those attributes that are and are 
#not required to build an accurate model.

#A popular automatic method for feature selection provided by the caret R package 
#is called Recursive Feature Elimination or RFE.

set.seed(7)
# load the library
library(mlbench)
library(caret)
# load the data
data(datafile)
# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(PimaIndiansDiabetes[,1:8], PimaIndiansDiabetes[,9], sizes=c(1:8), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results


plot(results, type=c("g", "o"))