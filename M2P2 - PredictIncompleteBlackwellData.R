
# libraries ---------------------------------------------------------------
library(mlbench)
library(caret)
library(readr)
library(ggplot2)
library(dplyr)

#clean Global Environment
rm(fitControl2)
rm(dfnew)
rm(importance)
rm(ComResp)

# LOAD DATA ---------------------------------------------------------------
library(readr)
Incom <- SurveyIncomplete
View(Incom)

#PREPROCESSING incompleteresponse data----

# transform to factor
Incom$car <- as.factor(Incom$car)

# check missing values
is.na(Incom)

sum(is.na(Incom))
View(Incom)

#PREDICT C50---- ADDS NEW COLUMN WITH PRED
Incom$brand_pred <- predict(object = rfFit, newdata = Incom)

#Postpresample---- CHECK KPI'S of new column
postResample(pred = Incom$brand_pred, obs = Incom$brand)
readRDS(Incom$brand)

#PREDICT KNN---- ADDS NEW COLUMN WITH PRED
Incom$brand_predKNN <- predict(object = rfFit2, newdata = Incom)

#Postpresample---- CHECK KPI'S of new column
postResample(pred = Incom$brand_pred, obs = Incom$brand)

#PREDICT RF---- ADDS NEW COLUMN WITH PRED
Incom$brand_predRF <- predict(object = rfFit3, newdata = Incom)

#Postpresample---- CHECK KPI'S of new column
postResample(pred = Incom$brand_predRF, obs = Incom$brand)

#PREDICT ADA---- ADDS NEW COLUMN WITH PRED
Incom$brand_predADA <- predict(object = rfFit4, newdata = Incom)

#Postpresample---- CHECK KPI'S of new column
postResample(pred = Incom$brand_predADA, obs = Incom$brand)

summary(Incom)

library(ggplot2)
ggplot(data = Incom)aes(x=age, y=salary)) +
  geom_point(shape=1) + 
  geom_smooth(method=lm) 
       