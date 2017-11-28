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
v <- matrix(c(13,(17^2-4^2),13*lbar, 13*abar), nrow = 1, byrow = T)
MCE <- v %*% as.matrix(coeffs)
MCE

############################################
#       REGRESSION DIAGNOSTICS
###########################################

plot(m.intxn) # looks like 226, 391, and 46 are outliers

outrm.dat <- train.dat[-c(226,391,46),]
m.orm <- lm(Y ~ manganese + arsenic + lead + education + age.mother.centered + IQ  + egg + meat + fish + smoking + arsenic2 + manganese2 + lead2 + mxa + mxl + axl, data = outrm.dat) 

plot(m.orm) # maybe exclude 15 too?


outrm2.dat <- train.dat[-c(226,391,46,15),]
m.orm2 <- lm(Y ~ manganese + arsenic + lead + education + age.mother.centered + IQ  + egg + meat + fish + smoking + arsenic2 + manganese2 + lead2 + mxa + mxl + axl, data = outrm2.dat) 

plot(m.orm2) # looks a lot better. This is the final causal model

m.causal <- m.orm2

MCE <- function(dat, mod){
  coi <- c("manganese", "manganese2", "mxl", "mxa")
  coeffs <- coefficients(mod)
  coeffs <- coeffs[names(coeffs) %in% coi]
  print(coeffs)
  
  ### Determining point estimate
  
  # 1. determine average values
  lbar <- mean(dat$lead) 
  abar <- mean(dat$arsenic)
  
  # 2. obtain the point estimate
  v <- matrix(c(13,(17^2-4^2),13*abar, 13*lbar), nrow = 1, byrow = T)
  MCE <- v %*% as.matrix(as.numeric(coeffs))
  
  return(MCE)
}

MCE(outrm2.dat, m.orm2)

####################################
#   PREDICTION, OUTLIERS REMOVED
####################################

pred_causal_orm <- predict.lm(m.orm2, newdat = test.data)
pred_causal_orm.out <- cbind(test.dat$ID, pred_causal_orm)
colnames(pred_causal_orm.out) <- c("ID", "Y")
write.csv(pred_causal_orm.out, file = "./results/pred_causal_orm.csv", row.names = F)

# obtain in-sample RMSE, outliers removed (orm)
rmse_causal_orm <- sqrt(mean((outrm2.dat$Y - m.causal$fitted.values)^2))
rmse_causal_orm

##################################
#   MISC
##################################

### F-test for squared terms

# minimal model (without squared terms)
m.min <- lm(Y ~ manganese + arsenic + lead  + education + age.mother.centered + IQ + egg + meat + fish + smoking, data = train.dat)

anova(m.min, m.base)
