
rm(list = ls())

library(Hmisc) 
library(rms) 

# -------------------------------------------------------------------
setwd("~/GoogleDrive/Graduate School/Masters Year 2/BST 230/Project")
train <- read.csv("train.csv", header = TRUE) # 400  14 
test <- read.csv("test.csv", header = TRUE) # 9588   13

test$Y <- NA
train$group <- rep("train", 400)
test$group <- rep("test", 9588)
all <- rbind(train, test) # 9988 14
all$group <- factor(all$group, levels = c("train", "test"))

# -------------------------------------------------------------------

# Step 1
# check the descriptive stats between train and test group

output1 <- summaryM(X1 + X2 + X3 + Z1 + Z2 + Z3 + Z4 + Z5 + Z6 + 
                    Z7 + Z8 + Z9 + Y ~ group, 
                    data = all, test = TRUE, overall = TRUE)
output1
rm(output1)

# Comments: 
# X1, X2, X3 and all potential confounders (Z1 - Z9) are not 
# significant different between train and test dataset.

# -------------------------------------------------------------------

# Step 2
# check the distribution of Y in train dataset

hist(train$Y, freq = FALSE, xlim = c(0, 350), ylim = c(0, 0.08),
     breaks = seq(0, 350, 10), 
     main = "Histogram of Y in Train Dataset", xlab = "Y",
     col = "lightblue", border = "skyblue3")
lines(density(train$Y), col = "red", lw = 1)

boxplot(train$Y, xlab = "Y")

table(train$Y)

min(train$Y)
train[which(train$Y == min(train$Y)),]

# Comments: 
# 1. From the histogram with density plot, boxplot and table, 
#    we found there is a extra small value of Y. The value is 44.80891 (ID=4995).
# 2. The distribution of Y is skewed to the left.

# -------------------------------------------------------------------

# Step 3
# check the correlation among the variables in train dataset

plot(train[,2:14])

# Comments: 
# 1. Z1 and Z3 has a very strong positive correlation.
# 2. The subject with the min value of Y (ID=4995) has the large value of
#    X1 and X2 and small value of Z6, Z7 and Z8.
# 3. can also check the spearman correlation using below code

rcorr(as.matrix(train[,2:14]), type = "spearman")

# -------------------------------------------------------------------

# Step 4
# check the correlation among the variables in test dataset

plot(test[,2:13])

# Comments: 
# 1. Z1 and Z3 has a very strong positive correlation.
#    The pattern is same as we saw in train dataset. 
# 2. We may consider to drop Z3 in the model analysis.

# -------------------------------------------------------------------

# Step 5
# use LASSO to do variable selection

library(glmnet)

train.y <- train$Y
train.X <- as.matrix(train[,2:13])

set.seed(12345)
fit1 <- cv.glmnet(train.X, train.y, family = "gaussian", type.measure = "deviance")

plot(fit1) # CV plot for lambda

fit1.coef.lambda.1se <- coef(fit1, s = fit1$lambda.1se)
fit1.coef.lambda.1se

fit1.coef.lambda.min <- coef(fit1, s = fit1$lambda.min)
fit1.coef.lambda.min

fit2 <- glmnet(train.X, train.y)
plot(fit2, xvar = "lambda")

fit1.coef.lambda06 <- coef(fit1, s = exp(-0.5))
fit1.coef.lambda06

# Comments: 
# 1. Using cross validation to find the lambda.
#    The optimal lambda is 5.319097.
#    If we use lambda = 5.319097, only X1 be chosen.
#    If we use min lambda = 0.02412146, all variables are in.
# 2. From Solution Paths of LASSO, we choose lambda = 0.6 (ie exp(-0.5))
#    to have 7 variables (X1, X2, X3, Z1, Z5, Z6, Z7).


# -------------------------------------------------------------------

# Step 6
# model fitting

plot(train[,c(2,3,4,5,9,10,11,14)])

lm.0 <- lm(Y ~ X1 + X2 + X3 + Z1 + Z2 + Z4 + Z5 + Z6 + Z7 + Z8 + Z9, data = train)
summary(lm.0)
plot(lm.0)

library(car)
vif(lm.0)

# from web
# https://onlinecourses.science.psu.edu/stat501/node/347
# A VIF of 1 means that there is no correlation among the kth predictor and 
# the remaining predictor variables, and hence the variance of bk is not inflated at all. 
# The general rule of thumb is that VIFs exceeding 4 warrant further investigation, 
# while VIFs exceeding 10 are signs of serious multicollinearity requiring correction.

lm.1 <- lm(Y ~ X1 + X2 + X3 + Z1 + Z5 + Z6 + Z7, data = train)
summary(lm.1)
plot(lm.1)
vif(lm.1)

lm.2 <- lm(Y ~ X1 + X2 + X3 + Z1 + Z5 + Z6 + Z7 +
              I(X1^2) + I(X2^2) + I(X3^2), data = train)
summary(lm.2)
plot(lm.2)


lm.3 <- lm(Y ~ X1 + X2 + X3 + Z5 + Z6 + Z7 + I(X1^2) + I(X2^2), data = train)
summary(lm.3)
plot(lm.3)

train2 <- train[-c(15,46,123,226),]
hist(train2$Y)

lm.3.a <- lm(Y ~ X1 + X2 + X3 + Z5 + Z6 + Z7 + I(X1^2) + I(X2^2), data = train2)
summary(lm.3.a)
plot(lm.3.a)

lm.3.b <- lm(Y ~ X1 + X2 + X3 + Z5 + Z6 + Z7 + I(X1^2), data = train2)
summary(lm.3.b)
plot(lm.3.b)


# -------------------------------------------------------------------
# -------------------------------------------------------------------
# -------------------------------------------------------------------


# (1)

train$X1p2 <- train$X1^2
train$X2p2 <- train$X2^2
train$X3p2 <- train$X3^2

test$X1p2 <- test$X1^2
test$X2p2 <- test$X2^2
test$X3p2 <- test$X3^2


lm.3.c <- lm(Y ~ X1 + X2 + X3 + Z1 + Z2 + Z4 + Z5 + Z6 + Z7 + Z8 + Z9 +
               X1p2 + X2p2 + X3p2, data = train)
summary(lm.3.c)
plot(lm.3.c)

train2 <- train[-c(15,46,123,226),]
hist(train2$Y)

# backward selection on p-value
lm.3.d <- lm(Y ~ X1 + X2 + X3 + Z1 + Z2 + Z5 + Z7 + Z9 +
               X1p2 + X2p2, data = train2)
summary(lm.3.d)
plot(lm.3.d)

# check linearity using Partial Regression Plots
# library(car)
av.plots(lm.3.d)

# final model
# y = beta0 + beta1*X1 + beta2*X2 + beta3*X3 + beta4*Z1 + beta5*Z2 +
#     beta6*Z5 + beta7*Z7 + beta8*Z9 + beta9*X1p2 + beta10*X2p2 + e

# ------------------------

# (2)

# that is, in this model, need to find the distribution for
# (17-4)*beta1_hat + (17^2 - 4^2)*beta9_hat

# beta_hat ~ N(beta, sigma^2 * inv(t(X)*X))

anova(lm.3.d) # MSE = 1.3
xbeta0 <- rep(1, dim(train2)[1])
XX <- cbind(xbeta0, train2[,c(2,3,4,5,6,9,11,13)], train2$X1^2, train2$X2^2)
XX <- as.matrix(XX)
estvarcov <- 1.3*solve(t(XX) %*% XX)

# est var for beta1 = 0.0001034556  # estvarcov[2,2]
# est var for beta9 = 1.9523e-08  # estvarcov[10,10]

(17-4)*5.156e-02 + (17^2 - 4^2)*(-4.512e-03)
# [1] -0.5615247

(17-4)^2*0.0001034556 + (17^2 - 4^2)^2*1.9523e-08
# [1] 0.01893903

# the estimate of the dist of the est of the marginal causal effect is
# Normal distribution with est mean -0.5615247 and est var 0.01893903.



# ------------------------

# (3)

# point estimzator
(17-4)*5.156e-02 + (17^2 - 4^2)*(-4.512e-03)
# [1] -0.561496

# 95% CI 

# lower 
-0.561496 - 1.96*sqrt(0.01893903)
# [1] -0.8312295

# upper 
-0.561496 + 1.96*sqrt(0.01893903)
# -0.2917625

# -0.561496 with 95% CI (-0.8312295, -0.2917625)
# it's not cover 0


# ------------------------

# (4)

train2$pred.Y.1 <- predict(lm.3.d, train2)
mean((train2$Y - train2$pred.Y.1)^2) # 1.238523
plot(train2$Y, train2$pred.Y.1)

train$pred.Y.1 <- predict(lm.3.d, train)
mean((train$Y - train$pred.Y.1)^2) # 7.521463 
plot(train$Y, train$pred.Y.1)

test$pred.Y.1 <- predict(lm.3.d, test)
sum(test$pred.Y.1 <= 0)
# [1] 1 # one pred value less than 0

hist(train$Y, freq = FALSE, xlim = c(0, 350), ylim = c(0, 0.08),
     breaks = seq(0, 350, 10), 
     main = "Histogram of Y in Train Dataset", xlab = "Y",
     col = "lightblue", border = "skyblue3")
lines(density(train$Y), col = "red", lw = 1)

lines(density(test$pred.Y.1), col = "green", lw = 1, lty = 2)

legend("topleft",
       legend = c("True Y in Train", "Pred Y in Test"),
       col = c("red", "green"), lw= 1, lty= c(1, 2), bty = "n")

mean(test$pred.Y.1) #  290.8732
mean(train$Y) # 290.6447


# estimated K-fold cross-validation prediction error 
library(boot)
glm.3.d <- glm(Y ~ X1 + X2 + X3 + Z1 + Z2 + Z5 + Z7 + Z9 +
               X1p2 + X2p2, data = train2)
cv.glm(train2, glm.3.d, K = 5)$delta
# [1] 1.413801 1.393180


glm.3.d <- glm(Y ~ X1 + X2 + X3 + Z1 + Z2 + Z5 + Z7 + Z9 +
                 X1p2 + X2p2, data = train)
cv.glm(train, glm.3.d, K = 5)$delta
# [1] 12.65122 11.30600



# ------------------------

# (5)


lm.3.c.2 <- lm(Y ~ X1 + X2 + X3 + Z1 + Z2 + Z4 + Z5 + Z6 + Z7 + Z8 + Z9 +
               I(X1^2) + I(X2^2) + I(X3^2) + X1*X2 + X1*X3 + X2*X3 +
               X1*Z1 + X1*Z2 + X1*Z4 + X1*Z5 + X1*Z6 + X1*Z7 + X1*Z8 + X1*Z9 +
               X2*Z1 + X2*Z2 + X2*Z4 + X2*Z5 + X2*Z6 + X2*Z7 + X2*Z8 + X2*Z9 +
               X3*Z1 + X3*Z2 + X3*Z4 + X3*Z5 + X3*Z6 + X3*Z7 + X3*Z8 + X3*Z9, 
               data = train)
summary(lm.3.c.2)

# backwards selection
step(lm.3.c.2, direction = "backward")

lm.3.e <- lm(Y ~ X1 + X2 + X3 + Z1 + Z2 + Z5 + Z6 + Z7 + Z9 + 
  I(X1^2) + X1*X2 + X2*X3 + X1*Z1 + X1*Z6 + X1*Z7 + X1*Z9 + 
  X2*Z1 + X2*Z7 + X3*Z1 + X3*Z2, data = train)
summary(lm.3.e)
anova(lm.3.e) # MSE = 539/379 = 1.4

train$pred.Y.2 <- predict(lm.3.e, train)
table(test$pred.Y.2)

##RMSE
sqrt(mean((train$Y - train$pred.Y.2)^2)) ##1.16
plot(train$Y, train$pred.Y.2)

test$pred.Y.2 <- predict(lm.3.e, test)
sqrt(mean((train$Y-test$pred.Y.2)^2))
sum(test$pred.Y.2 <= 0)
# [1] 1 # one pred value less than 0

hist(train$Y, freq = FALSE, xlim = c(0, 350), ylim = c(0, 0.08),
     breaks = seq(0, 350, 10), 
     main = "Histogram of Y in Train Dataset", xlab = "Y",
     col = "lightblue", border = "skyblue3")
lines(density(train$Y), col = "red", lw = 1)

lines(density(test$pred.Y.2), col = "green", lw = 1, lty = 2)

legend("topleft",
       legend = c("True Y in Train", "Pred Y in Test"),
       col = c("red", "green"), lw= 1, lty= c(1, 2), bty = "n")


mean(test$pred.Y.1) #  290.8732
mean(test$pred.Y.2) #290.8761
mean(train$Y) # 290.6447


glm.3.e <- glm(Y ~ X1 + X2 + X3 + Z1 + Z2 + Z5 + Z6 + Z7 + Z9 + 
                 I(X1^2) + X1*X2 + X2*X3 + X1*Z1 + X1*Z6 + X1*Z7 + X1*Z9 + 
                 X2*Z1 + X2*Z7 + X3*Z1 + X3*Z2, data = train)
cv.glm(train, glm.3.e, K = 5)$delta
# [1] 5.223677 4.525856




