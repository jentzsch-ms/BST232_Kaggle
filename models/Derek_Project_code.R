
rm(list = ls())

library(Hmisc) 
library(rms) 

# -------------------------------------------------------------------

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
#1. Using cross validation to find the lambda.
 #  The optimal lambda is 5.319097.
  # If we use lambda = 5.319097, only X1 be chosen.
   #If we use min lambda = 0.02412146, all variables are in.
#2. From Solution Paths of LASSO, we choose lambda = 0.6 (ie exp(-0.5))
 #  to have 7 variables (X1, X2, X3, Z1, Z5, Z6, Z7).

# -------------------------------------------------------------------

# Step 6
# model fitting

plot(train[,c(2,3,4,5,9,10,11,14)])

lm.1 <- lm(Y ~ X1 + X2 + X3 + Z1 + Z5 + Z6 + Z7, data = train)
summary(lm.1)
plot(lm.1)

lm.2 <- lm(Y ~ X1 + X2 + X3 + Z1 + Z5 + Z6 + Z7 +
              I(X1^2) + I(X2^2) + I(X3^2), data = train)
summary(lm.2)
plot(lm.2)










