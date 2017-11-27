## tweaking RidgeReg.CrossVal

# load in the original training and test data
test.dat <- read.csv("./data/test.csv", header = T)
train.dat <- read.csv("./data/train.csv", header = T)

# rename the variables to reflect what they mean
colnames(test.dat) <- c("ID","manganese", "arsenic", "lead", "age.child", "education", "age.mother.centered", "IQ", "home.quality", "egg", "meat", "fish", "smoking")

colnames(train.dat) <- c("ID","manganese", "arsenic", "lead", "age.child", "education", "age.mother.centered", "IQ", "home.quality", "egg", "meat", "fish", "smoking", "Y")

# remove the outlier
train.dat <- train.dat[-c(train.dat$Y <= 200),]
# create squared variables within both datasets
train.dat <- within(train.dat, {
  manganese2 <- manganese^2
  lead2 <- lead^2
  arsenic2 <- arsenic^2
})

test.dat <- within(test.dat, {
  manganese2 <- manganese^2
  lead2 <- lead^2
  arsenic2 <- arsenic^2
})

# Variables of interest. Start with all of them
vars <- c(
  "manganese",
  "manganese2",
  "lead",
  "lead2",
  "arsenic",
  "arsenic2",
  "age.child",
  "education",
  "age.mother.centered",
  "IQ",
  "home.quality",
  "egg",
  "meat",
  "fish",
  "smoking"
)


RidgeReg.CrossVal <- function(Y, DF, K, Ls, vars){
  # This function performs K-fold cross-validation on ridge regression. 
  # Requires the MXM package. 
  # Originally by Raffi Small, edited by Max Jentzsch
  
  # Y - response variable (vector).
  # DF - dataframe containing covariates (matrix).
  # K - number of K-fold cross-validations.
  # Ls - sequence of lambdas to try for the ridge regression (vector).
  # vars - variables of interest
  
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
    coeff <- numeric()
    for(k in 1:K){
      #Split data.
      test.index <- which(folds==k, arr.ind=TRUE)
      
      test.target <- Y[test.index]
      test.data <- model.matrix(test.target ~ ., data=DF[test.index, vars])
      
      train.target <- Y[-test.index]
      train.data <- model.matrix(train.target ~ ., data=DF[-test.index, vars])
      
      # str(test.data)
      # print(head(test.data))
      
      #Use regression to make predictions on the test set and calculate RMSPE.
      #fit <- lm(train.target~manganese+manganese2+
      #            lead+lead2+
      #            +arsenic+arsenic2+
      #            home.quality+age.centered+eggs+fish+smoking+education+IQ,
      #          data=train.data) #model for submission
      
      fit <- glmnet( y = train.target, x = train.data, lambda=L, alpha = 0, family = "gaussian")
      # fit <- ridge.reg(train.target, as.matrix(train.data), lambda=L, B=1)
      
      coeff <- rbind(coeff, fit$beta)
      # print(head(test.data))
      # print(head(fit$est))
      # test.predict <- as.matrix(test.data) %*% as.matrix(fit$beta)
      test.predict <- predict.glmnet(fit, newx = test.data)
      print(head(test.predict))
      print(head(test.target))
      MSPE <- mean((test.predict - test.target)^2)
      RMSE.CrossVal[k] <- sqrt(MSPE)
    }
    RMSE[i] <- mean(RMSE.CrossVal)
  }
  
  plot(Ls, RMSE,
       xlab="Lambda", ylab="RMSE of Validation Set",
       main="Results of K-Fold Cross Validation with Ridge Regression")
  
  print(RMSE)
  return(fit)
}

library(MXM)
library(glmnet)

dropvars <- c("ID", "Y")
DF <- train.dat[,(!names(train.dat) %in% dropvars)]

# all variables
regfit <- RidgeReg.CrossVal(Y = train.dat$Y,
                  DF = DF,
                  K = 10, 
                  Ls = c(0,0.0001,0.001, 0.01, 0.1, 0.398,1), 
                  vars)

# run prediction on the test dataset
newdat <- model.matrix(~., data = test.dat[,-1])
pred.out <- predict.glmnet(regfit, newx = newdat)
pred.out <- cbind(test.dat$ID, pred.out)
colnames(pred.out) <- c("ID", "Y")
write.csv(pred.out, file = "./results/glmnet_max_2017-11-25.csv")

# Raffi's variables
vars2 <-  c("manganese", "manganese2",
                     "lead", "lead2",
                     "arsenic", "arsenic2",
                     "home.quality", "age.mother.centered",
                     "egg", "fish", "smoking",
                     "education", "IQ")

RidgeReg.CrossVal(Y = train.dat$Y,
                  DF = DF,
                  K = 10, 
                  Ls = c(0,0.0001, 0.001, 0.01, 0.1, 1), 
                  vars = vars2)

# Raffi's variables potentially adjusting for incorrect labels?
vars3 <- c("manganese", "manganese2",
           "lead", "lead2",
           "arsenic", "arsenic2",
           "home.quality", "age.mother.centered",
           "meat", "fish", "smoking",
           "education", "IQ")

RidgeReg.CrossVal(Y = train.dat$Y,
                  DF = DF,
                  K = 10, 
                  Ls = c(0,0.0001, 0.001, 0.01, 0.1, 1), 
                  vars = vars3)
