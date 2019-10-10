
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

#C50 rfFit MODEL AUTOM GRID----

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

#KNN MODEL rfFit2 AUTOM GRID----
#caret model - Automatic Tuning Grid
#dataframe = CompleteResponses (ComResp)
#Y Value = brand

getModelInfo("rf")

set.seed(7)
#SET SPLIT 75%/25% for train/test in the dataset
inTraining2 <- createDataPartition(ComResp$brand, p = .75, list = FALSE)
training2 <- ComResp[inTraining,]
testing2 <- ComResp[-inTraining,]
#10 fold cross validation
fitControl2 <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
#train knn model with a tuneLenght = `1`(trains with 1 mtry values for knn)
rfFit2 <- train(brand~., 
                data = training, 
                method = "kknn", 
                trControl=fitControl, 
                tuneLength = 1,
                preProcess=c("center", "scale"))
#training results
rfFit2

#RF MODEL rfFit3 AUTOM GRID----
#caret model - Automatic Tuning Grid
#dataframe = CompleteResponses (ComResp)
#Y Value = brand

set.seed(7)
#SET SPLIT 75%/25% for train/test in the dataset
inTraining3 <- createDataPartition(ComResp$brand, p = .75, list = FALSE)
training3 <- ComResp[inTraining,]
testing3 <- ComResp[-inTraining,]
#10 fold cross validation
fitControl3 <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
#train rf model with a tuneLenght = `1`(trains with 1 mtry values for rf)
rfFit3 <- train(brand~., 
                data = training, 
                method = "rf", 
                trControl=fitControl, 
                tuneLength = 1,
                preProcess=c("center", "scale"))
#training results
rfFit3

#ADABOOST dec tree Model rfFit4 AUTOM GRID----
#caret model - Automatic Tuning Grid
#dataframe = CompleteResponses (ComResp)
#Y Value = brand

set.seed(7)
#SET SPLIT 75%/25% for train/test in the dataset
inTraining4 <- createDataPartition(ComResp$brand, p = .75, list = FALSE)
training4 <- ComResp[inTraining,]
testing4 <- ComResp[-inTraining,]
#10 fold cross validation
fitControl4 <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
#train rf model with a tuneLenght = `1`(trains with 1 mtry values for rf)
rfFit4 <- train(brand~., 
                data = training, 
                method = "adaboost",
                trControl=fitControl, 
                tuneLength = 1,
                preProcess=c("center", "scale"))
#training results
rfFit4

#POSTRESAMPLE(pred, obs)----
postResample(pred = predict(object = rfFit2, newdata = testing2), obs = testing2$brand)
##output = Accuracy     Kappa 
##rfFit2 = 0.9180660 0.8262386 
postResample(pred = predict(object = rfFit, newdata = testing), obs = testing$brand)
##output = Accuracy     Kappa 
##rfFit 0.9098196 0.8104663 
