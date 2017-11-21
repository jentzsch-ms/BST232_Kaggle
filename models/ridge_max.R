#################################
# Simple ridge regression as done in
# https://drsimonj.svbtle.com/ridge-regression-with-glmnet
################################

library(tidyverse)
library(broom)
library(glmnet)

# load dataset
train.dat <- read.csv(file = "./data/train.csv")
test.dat <- read.csv(file = "./data/test.csv")

# split training datset in half
set.seed(555)
samp.id <- sample(nrow(train.dat), nrow(train.dat)/2)
ttrain.dat <- train.dat[samp.id,] 
ttest.dat <- train.dat[-samp.id,]

Y <- ttrain.dat$Y
X <- model.matrix(Y ~ ., data = ttrain.dat[,-1])
lambdas <- 10^seq(3, -2, by = -.1)

# test model
m.1 <- glmnet(X, Y, alpha = 0, lambda = lambdas)
summary(m.1)

cv_fit <- cv.glmnet(X, Y, alpha = 0, lambda = lambdas)
plot(cv_fit)

# determine optimal lambda
opt_lambda <- cv_fit$lambda.min
opt_lambda

y_pred <- predict(m.1, s = opt_lambda, 
                  newx = model.matrix(Y ~ ., data = ttest.dat[,-1]))

rmse <- sqrt(sum((ttest.dat$Y - y_pred)^2))

# Prediction will probably work better if we run the regression on the full dataset. Redo above, but on on the full trainingset

Y <- train.dat$Y
X <- model.matrix(Y ~ ., data = train.dat[,-1])
lambdas <- 10^seq(3, -2, by = -.1)

# test model
m.full <- glmnet(X, Y, alpha = 0, lambda = lambdas)
summary(m.full)

cv_fit <- cv.glmnet(X, Y, alpha = 0, lambda = lambdas)
plot(cv_fit)

opt_lambda <- cv_fit$lambda.min
opt_lambda

y_pred <- predict(m.1, s = opt_lambda, newx = model.matrix( ~ ., data = test.dat[,-1]))

out.dat <- data.frame(ID = test.dat$ID, Y = y_pred)
colnames(out.dat) <- c("ID", "Y")
write.csv(out.dat, file = "./results/ridge_2017-11-20.csv", row.names = FALSE)
