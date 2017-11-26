RidgeReg.CrossVal1 <- function(Y, DF, K, Ls){
  #This function performs K-fold cross-validation on ridge regression. 
  #Requires the MXM package.
  
  #Y - response variable (vector).
  #DF - dataframe containing covariates (matrix).
  #K - number of K-fold cross-validations.
  #Ls - sequence of lambdas to try for the ridge regression (vector).
  
  #Set seed for reproducibility and shuffle the data.
  set.seed(0)
  shuffle.index <- sample(nrow(DF), replace=FALSE)
  DF <- DF[shuffle.index,]
  Y <- Y[shuffle.index]
  
  #Variables.
  vars <- c("manganese", "manganese2",
            "lead", "lead2",
            "arsenic", "arsenic2",
            "home.quality", "age.centered",
            "eggs", "fish", "smoking",
            "education", "IQ")
  
  #Create K-folds.
  folds <- cut(seq(1, nrow(DF)), breaks=K, labels=FALSE)
  
  #Loop over each lambda.
  RMSE <- matrix(0,length(Ls),1)
  for(i in 1:length(Ls)){
    L <- Ls[i]
    
    #Loop over each K-fold.
    RMSE.CrossVal <- matrix(0,K,1)
    coeff <- numeric()
    for(k in 1:K){
      #Split data.
      test.index <- which(folds==k, arr.ind=TRUE)
      
      test.data <- cbind(1,DF[test.index, vars])
      test.target <- Y[test.index]
      
      train.data <- cbind(1,DF[-test.index, vars])
      train.target <- Y[-test.index]
      
      #Use regression to make predictions on the test set and calculate RMSPE.
      fit <- lm(train.target~manganese+manganese2+
                  lead+lead2+
                  +arsenic+arsenic2+
                  home.quality+age.centered+eggs+fish+smoking+education+IQ,
                data=train.data) #model for submission
      #fit <- ridge.reg(train.target, train.data, lambda=L, B=1)
      coeff <- rbind(coeff, fit$coeff)
      test.predict <- predict.lm(fit, test.data)
      
      MSPE <- mean((test.predict - test.target)^2)
      RMSE.CrossVal[k] <- sqrt(MSPE)
    }
    RMSE[i] <- mean(RMSE.CrossVal)
  }
 
  plot(Ls, RMSE,
       xlab="Lambda", ylab="RMSE of Validation Set",
       main="Results of K-Fold Cross Validation with Ridge Regression")
  
  print(RMSE)
  return(coeff)
}