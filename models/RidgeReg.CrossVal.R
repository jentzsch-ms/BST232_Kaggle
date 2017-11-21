RidgeReg.CrossVal <- function(Y, DF, K, Ls){
  #This function performs K-fold cross-validation on ridge regression. Requires the MXM package.
  
  #Y - response variable (vector).
  #DF - design matrix which should include a column of ones for the intercept term (matrix/DF)/
  #K - number of K-fold cross-validation.
  #Ls - sequence of lambdas to try for the ridge regression (vector).
  
  #Set seed for reproducibility and shuffle the data.
  set.seed(0)
  shuffle.index <- sample(nrow(DF), replace=FALSE)
  DF <- DF[shuffle.index,]
  Y <- Y[shuffle.index]
  
  #Create K-folds.
  folds <- cut(seq(1, nrow(DF)), breaks=K, labels=FALSE)
  
  #Loop over each lambda.
  RMSE <- matrix(0,length(Ls),1)
  for(i in 1:length(Ls)){
    L <- Ls[i]
    
    #Loop over each K-fold.
    RMSE.CrossVal <- matrix(0,K,1)
    for(k in 1:K){
      #Split data.
      test.index <- which(folds==k, arr.ind=TRUE)
      
      test.data <- DF[test.index, ]
      test.target <- Y[test.index]
      
      train.data <- DF[-test.index, ]
      train.target <- Y[-test.index]
      
      #Use ridge regression to make predictions on the test set and calculate prediction RMSE.
      beta <- ridge.reg(train.target, train.data, L, B=1)$beta
      test.predict <- test.data %*% matrix(beta)
      SSE <- sum((test.predict - test.target)^2)
      RMSE.CrossVal[k] <- sqrt(mean(SSE))
    }
    RMSE[i] <- mean(RMSE.CrossVal)
  }
 
  plot(Ls, RMSE,
       xlab="Lambda", ylab="RMSE of Validation Set",
       main="Results of K-Fold Cross Validation with Ridge Regression")
  
  best.RMSE.index <- which(RMSE==min(RMSE))
  return(best.lambda=Ls[best.RMSE.index])
}