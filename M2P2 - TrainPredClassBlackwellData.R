
# libraries ---------------------------------------------------------------
library(mlbench)
library(caret)
library(readr)
install.packages(C50)

# LOAD DATA ---------------------------------------------------------------
CompleteResponses <- read_csv(
  "CompleteResponses.csv",
  col_types = cols(
    brand = col_factor(levels = c("0","1")),
    zipcode = col_factor(levels = c("0","1", "2", "3", "4", "5", "6", "7","8"))
    )
  )

#PREPROCESSING completeresponse data----

# transform to factor
CompleteResponses$car <- as.factor(CompleteResponses$car)
CompleteResponses$elevel <- as.factor(CompleteResponses$elevel)

# check missing values
is.na(CompleteResponses)
ComResp <- CompleteResponses
sum(is.na(ComResp))
View(ComResp)

#RANK BY IMPORTANCE OF ATTRIBUTES/FEATURES----

set.seed(7)

# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model <- train(brand~., data=ComResp, method="lvq", preProcess="scale", trControl=control)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)

#CREATE NEW DATAFRAME----
head(CompleteResponses)
dfnew <- CompleteResponses[,c(1,2,7)]
ComResp <- dfnew

#1st MODEL AUTOM GRID C5.0----

set.seed(7)

#create a 20% sample of the data
ComResp <- ComResp[sample(1:nrow(ComResp), 2000,replace=FALSE),]

# define an 75%/25% train/test split of the dataset
inTraining <- createDataPartition(ComResp$brand, p = .75, list = FALSE)
training <- ComResp[inTraining,]
testing <- ComResp[-inTraining,]

#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
#train C5.0 model with a tuneLenght = 1 (trains with 1 mtry value for C5.0)

rfFit <- train(brand~., 
                      data = training, 
                      method = "C5.0", 
                      trControl=fitControl, 
                      tuneLength = 2,
                      preProcess=c("center", "scale"))
#training results
rfFit

#2nd MODEL AUTOM GRID rpartScore----
#caret model - Automatic Tuning Grid
#dataframe = CompleteResponses (ComResp)
#Y Value = brand

set.seed(7)
#SET SPLIT 75%/25% for train/test in the dataset
inTraining2 <- createDataPartition(ComResp$brand, p = .75, list = FALSE)
training2 <- ComResp[inTraining,]
testing2 <- ComResp[-inTraining,]
#10 fold cross validation
fitControl2 <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
#train Linear Regression model with a tuneLenght = `2`1 (trains with 1 mtry values for rpartScore)
rfFit2 <- train(brand~., 
                data = training, 
                method = "rpartScore", 
                trControl=fitControl, 
                tuneLength = 1,
                preProcess=c("center", "scale"))
#training results
rfFit2

#defaultSummary(fitControl5, lev = NULL, model = NULL)

#POSTRESAMPLE(pred, obs)----
postResample(pred = predict(object = rfFit2, newdata = testing2), obs = testing2$brand)
##output = Accuracy     Kappa 
##rfFit2 = 0.6172345 0.0000000 
postResample(pred = predict(object = rfFit, newdata = testing), obs = testing$brand)
##output = Accuracy     Kappa 
##rfFit 0.9098196 0.8104663 

#PREDICT---- 
predict(object = rfFit, newdata = testing)

#twoClassSummary(data, lev = NULL, model = NULL)
twoClassSummary(rfFit, lev = NULL, model = NULL)

#mnLogLoss(data, lev = NULL, model = NULL)
#multiClassSummary(data, lev = NULL, model = NULL)
#prSummary(data, lev = NULL, model = NULL)