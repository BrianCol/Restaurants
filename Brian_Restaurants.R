library(tidyverse)
library(DataExplorer)
library(lattice)
library(caret)
library(naniar)
library(textcat)

test <- read_csv("test.csv")

train <- read_csv("train.csv")

#use xgb
xgbtree <- train(form=revenue~.,
                 data=train %>% select(-Id, -`Open Date`, -City, -`City Group`, -Type ),
                 method="xgbTree",
                 trControl=trainControl(method="repeatedcv",
                                        number=3, #Number of pieces of your data
                                        repeats=1) #repeats=1 = "cv"
)

xgbtree$results
xgbtree.preds <- data.frame(Id=test$Id, Prediction=predict(xgbtree, newdata=test))

write_csv(x=xgbtree.preds, path="./xgbtree.csv")


#use gbm (is better than xgb)
gbm_model <- train(form=revenue~.,
                   data=train %>% select(-Id, -`Open Date`, -City, -`City Group`, -Type ),
                   method="gbm",
                   trControl=trainControl(method="repeatedcv",
                                          number=3, #Number of pieces of your data
                                          repeats=1) #repeats=1 = "cv"
)

gbm_model$results
gbm.preds <- data.frame(Id=test$Id, Prediction=predict(gbm_model, newdata=test))

write_csv(x=gbm.preds, path="./gbm.csv")


gbm_model <- train(form=revenue~.,
                   data=train %>% select(-Id, -`Open Date`, -City, -`City Group`, -Type ),
                   method="gbm",
                   trControl=trainControl(method="repeatedcv",
                                          number=3, #Number of pieces of your data
                                          repeats=1) #repeats=1 = "cv"
)

gbm_model$results
gbm.preds <- data.frame(Id=test$Id, Prediction=predict(gbm_model, newdata=test))

write_csv(x=gbm.preds, path="./gbm.csv")







