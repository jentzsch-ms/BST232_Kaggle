library(tidyverse)
training_data <- read.csv("./data/train.csv", header=T, row.names = "ID")
training_data <- training_data[!(row.names(training_data) %in% c("4995")),] #remove outlier
testing_data <- read.csv("./data/test.csv", header=T)

linFit <- lm(Y~., data=training_data)
stepFit <- step(linFit, direction="both")

library(caret)
library(gbm)
set.seed(123)
rfFit <- train(Y~X1+X2+X1:X2+Z1+Z2+Z5+Z7+Z9, data=training_data, method="rf", prox=T, trControl=trainControl(method="cv",number=5), allowParallel=T)

rfPred <- predict(rfFit, newdata=testing_data)
dfPred <- data.frame(ID=testing_data$ID, Y=rfPred)
write.table(dfPred, file="episodeI.csv", row.names = F, sep=",")


