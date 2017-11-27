## Causal model (Max's version)

library(tidyverse)

# read in the data
train.dat <- read.csv("./data/train.csv", header = T)
test.dat <- read.csv("./data/test.csv", header = T)

# rename the variables to reflect what they mean
colnames(test.dat) <- c("ID","manganese", "arsenic", "lead", "age.child", "education", "age.mother.centered", "IQ", "home.quality", "egg", "meat", "fish", "smoking")

colnames(train.dat) <- c("ID","manganese", "arsenic", "lead", "age.child", "education", "age.mother.centered", "IQ", "home.quality", "egg", "meat", "fish", "smoking", "Y")

# create squared and interaction terms for metals
train.dat <- within(train.dat, {
  manganese2 <- manganese^2
  lead2 <- lead^2
  arsenic2 <- arsenic^2
  mxa <- manganese*arsenic
  mxl <- manganese*lead
  axl <- arsenic*lead
})

test.dat <- within(test.dat, {
  manganese2 <- manganese^2
  lead2 <- lead^2
  arsenic2 <- arsenic^2
  mxa <- manganese*arsenic
  mxl <- manganese*lead
  axl <- arsenic*lead
})

# base model: squared terms, no interactions 
m.base <- lm(Y ~ manganese + arsenic + lead  + education + age.mother.centered + IQ + egg + meat + fish + smoking + arsenic2 + manganese2 + lead2, data = train.dat)

# model with squared terms and interaction terms
m.intxn <- lm(Y ~ manganese + arsenic + lead + education + age.mother.centered + IQ  + egg + meat + fish + smoking + arsenic2 + manganese2 + lead2 + mxa + mxl + axl, data = train.dat) 

# test whether the interaction terms add information 
anova(m.base, m.intxn)
summary(m.intxn)

# model with just interaction terms
m.intx.nosqu <- lm(Y ~ manganese + arsenic + lead + education + age.mother.centered + IQ + egg + meat + fish + smoking + mxa + mxl + axl, data = train.dat)

# testing whether the interaction models "require" the squared terms
anova(m.intx.nosqu, m.intxn)


#######################################
#    PREDICTED VALUES
#######################################

# test data, removing the ID variable 
test.data <- test.dat[,-1]

#obtain predicted values
pred_causal <- predict.lm(m.intxn, newdata = test.data)

# combine the data with the ID variable, and export for submission
pred_causal.out <- cbind(test.dat$ID, pred_causal)
colnames(pred_causal.out) <- c("ID", "Y")
write.csv(pred_causal.out, file = "./results/pred_causal.csv", row.names = F)

#########################################
#   DETERMINING MCE
#########################################

# coefficients of interest: everything involving manganese
coi <- c("manganese", "manganese2", "mxl", "mxa")
coeffs <- coefficients(m.intxn)
coeffs <- coeffs[names(coeffs) %in% coi]

### Determining point estimate

# 1. determine average values
lbar <- mean(test.dat$lead) 
abar <- mean(test.dat$arsenic)

# 2. obtain the point estimate
v <- matrix(c(17,17^2,17*lbar, 17*abar), nrow = 1, byrow = T)
MCE <- v %*% as.matrix(coeffs)
MCE
