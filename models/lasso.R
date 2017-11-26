rm(list = ls())
library(glmnet)

#Load data and label columns.
DF.test <- read.csv("test.csv", header=TRUE)
DF.train <- read.csv("train.csv", header=TRUE)

vars <- c("ID", "manganese", "arsenic", "lead",
          "age", "education", "age.centered", "IQ", "home.quality",
          "meat", "fish", "eggs", "smoking")

colnames(DF.test) <- vars
colnames(DF.train) <- c(vars,"cog.score")
rm(vars)

#Generate interaction effects between smoking and heavy metals.
DF.train$manganeseXsmoking <- DF.train$manganese*DF.train$smoking
DF.train$arsenicXsmoking <- DF.train$arsenic*DF.train$smoking
DF.train$leadXsmoking <- DF.train$lead*DF.train$smoking

DF.test$manganeseXsmoking <- DF.test$manganese*DF.test$smoking
DF.test$arsenicXsmoking <- DF.test$arsenic*DF.test$smoking
DF.test$leadXsmoking <- DF.test$lead*DF.test$smoking

#Square and cube the heavy metals.
DF.train$manganese2 <- DF.train$manganese^2
DF.train$arsenic2 <- DF.train$arsenic^2
DF.train$lead2 <- DF.train$lead^2

DF.test$manganese2 <- DF.test$manganese^2
DF.test$arsenic2 <- DF.test$arsenic^2
DF.test$lead2 <- DF.test$lead^2

DF.train$manganese3 <- DF.train$manganese^3
DF.train$arsenic3 <- DF.train$arsenic^3
DF.train$lead3 <- DF.train$lead^3

DF.test$manganese3 <- DF.test$manganese^3
DF.test$arsenic3 <- DF.test$arsenic^3
DF.test$lead3 <- DF.test$lead^3

#Calculate log of the heavy metals.
DF.train$ln.manganese <- log(DF.train$manganese)
DF.train$ln.arsenic <- log(DF.train$arsenic)
DF.train$ln.lead <- log(DF.train$lead)

DF.test$ln.manganese <- log(DF.test$manganese)
DF.test$ln.arsenic <- log(DF.test$arsenic)
DF.test$ln.lead <- log(DF.test$lead)

#Select variables for analysis
vars <- c("manganese", "manganese2", "manganese3",
          "arsenic", "arsenic2", "arsenic3",
          "lead", "lead2", "lead3",
          "age", "education", "IQ", "home.quality",
          "meat", "fish", "eggs",
          "smoking")

#Run LASSO on all observations.
X <- data.matrix(DF.train[,vars])
Y <- DF.train$cog.score
fit <- glmnet(X, Y)

MSE.CrossVal <- function(lambda){

  #Set seed for reproducibility and shuffle the data.
  set.seed(0)
  K <- 10
  shuffle.index <- sample(nrow(X), replace=FALSE)
  DF <- X[shuffle.index,]
  Y <- Y[shuffle.index]
  
  folds <- cut(seq(1, nrow(DF)), breaks=K, labels=FALSE)
  
  RMSE.CrossVal <- NULL
  for(k in 1:K){
    #Split data.
    test.index <- which(folds==k, arr.ind=TRUE)
    
    test.data <- DF[test.index,]
    test.target <- Y[test.index]
    
    train.data <- DF[-test.index,]
    train.target <- Y[-test.index]
    
    #Remove low outliers.
    keep.index <- train.target>mean(train.target)-3*sd(train.target)
    train.data <- train.data[keep.index,]
    train.target <- train.target[keep.index]
    
    fit <- glmnet(train.data, train.target, thresh=1e-10, lambda=lambda^2)
    
    test.predict <- predict(fit, test.data)
    Y.hat <- test.predict[,dim(test.predict)[2]]
    
    MSPE <- (Y.hat - test.target)^2
    RMSE.CrossVal <- c(RMSE.CrossVal, sqrt(MSPE))
  }
  
  mean(RMSE.CrossVal)^2
}

fit <- optim(1, MSE.CrossVal, method="BFGS", control=list(REPORT=1, trace=3))
lambda <- fit$par^2
RMSE <- sqrt(fit$value)
print(RMSE)