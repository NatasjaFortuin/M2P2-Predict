#install.packages("caret", dependencies = c("Depends", "Suggests"))
library(caret)
nfwy <- WholeYear

#FIRSTEXAMPLE AUTOM GRID RANDOM FOREST

set.seed(998)

#create a 20% sample of the data
nfwy <- nfwy[sample(1:nrow(nfwy), 7000,replace=FALSE),]
# define an 75%/25% train/test split of the dataset
inTraining <- createDataPartition(nfwy$SolarRad, p = .75, list = FALSE)
training <- nfwy[inTraining,]
testing <- nfwy[-inTraining,]
#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
#train Random Forest Regression model with a tuneLenght = 1 (trains with 1 mtry value for RandomForest)
rfFit1 <- train(SolarRad~., data = training, method = "rf", trControl=fitControl, tuneLength = 1)
#training results
rfFit1

#SECONDEXAMPLE AUTOM GRID RANDOM FOREST
#caret model - Automatic Tuning Grid
#http://topepo.github.io/caret/bytag.html
#model training: http://topepo.github.io/caret/training.html
#model measurement: http://topepo.github.io/caret/other.html
#dataframe = WholeYear (nfwy)
#Y Value = SolarRad

set.seed(998)
#create a 20% sample of the data
nfwy <- nfwy[sample(1:nrow(nfwy), 7000,replace=FALSE),]
# define an 75%/25% train/test split of the dataset
inTraining2 <- createDataPartition(nfwy$SolarRad, p = .75, list = FALSE)
training2 <- nfwy[inTraining,]
testing2 <- nfwy[-inTraining,]
#10 fold cross validation
fitControl2 <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
#train Linear Regression model with a tuneLenght = 2 (trains with 2 mtry values for RandomForest)
rfFit2 <- train(SolarRad~., data = training, method = "rf", trControl=fitControl, tuneLength = 2)
#training results
rfFit2

#THIRDEXAMPLE MANUAL GRID RANDOM FOREST
#caret model - Manual Tuning Grid
#http://topepo.github.io/caret/bytag.html
#model training: http://topepo.github.io/caret/training.html
#model measurement: http://topepo.github.io/caret/other.html
#dataframe = WholeYear
#Y Value = SolarRad

#create a 20% sample of the data
nfwy <- nfwy[sample(1:nrow(nfwy), 7000,replace=FALSE),]
# define an 75%/25% train/test split of the dataset
inTraining3 <- createDataPartition(nfwy$SolarRad, p = .75, list = FALSE)
training3 <- nfwy[inTraining,]
testing3 <- nfwy[-inTraining,]
#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
#dataframe for manual tuning of mtry
rfGrid3 <- expand.grid(mtry=c(1,2,3))
#train Random Forest Regression model
  #note the system time wrapper. system.time()
    #this is used to measure process execution time 
system.time(rfFitm3 <- train(SolarRad~., data = training, method = "rf", trControl=fitControl, tuneGrid=rfGrid3))
#training results
rfFitm3

#FOURTHEXAMPLE RANDOM SEARCH RANDOM FOREST
#caret model - Random Tuning Search
#dataframe = WholeYear
#Y Value = SolarRad
nfwy <- WholeYear
set.seed(998)
#create a 20% sample fo the data
nfwy <- nfwy[sample(1:nrow(nfwy), 7000,replace=FALSE),]
#define an 75%/25% train/test split of the dataset
inTraining4 <- createDataPartition(nfwy$SolarRad, p = .75, list = FALSE)
training4 <- nfwy[inTraining,]
testing4 <- nfwy[-inTraining,]
#10 fold cross validation
rfitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1, search = 'random')
#train Random Forest Regression model
rfFit4 <- train(SolarRad~., data = training, method = "rf", trControl=rfitControl)
#training results
rfFit4

#FIFTHEXAMPLE AUTOM GRID LINEAR MODEL
#caret models
#dataframe = WholeYear
#Y Value = SolarRad
nfwy <- WholeYear
set.seed(998)
#create a 20% sample fo the data
nfwy<- nfwy[sample(1:nrow(nfwy), 7000,replace=FALSE),]
# define an 75%/25% train/test split of the dataset
inTraining5 <- createDataPartition(nfwy$SolarRad, p = .75, list = FALSE)
training5 <- nfwy[inTraining,]
testing5 <- nfwy[-inTraining,]
#10 fold cross validation
fitControl5 <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
#train Linear Regression model
LMFit5 <- train(SolarRad~., data = training, method = "lm", trControl=fitControl5)
#check the results
LMFit5

#TODO
defaultSummary(fitControl5, lev = NULL, model = NULL)
postResample(pred, obs)
twoClassSummary(data, lev = NULL, model = NULL)
mnLogLoss(data, lev = NULL, model = NULL)
multiClassSummary(data, lev = NULL, model = NULL)
prSummary(data, lev = NULL, model = NULL)