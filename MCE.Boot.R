rm(list = ls())
set.seed(0)

#Read in the data.
train.dat <- read.csv("train.csv", header = TRUE)
test.dat <- read.csv("test.csv", header = TRUE)

#Remove outliers excluded from causal model.
train.dat <- train.dat[-c(15, 46, 226, 391),]

#Label variables.
colnames(test.dat) <- c("ID","manganese", "arsenic", "lead", 
                        "age.child", "education", "age.mother.centered",
                        "IQ", "home.quality",
                        "egg", "meat", "fish", "smoking")

colnames(train.dat) <- c(colnames(test.dat), "Y")

#Create squared and interaction terms for metals.
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

#Causal model and point estimate.
fit.full <- lm(Y ~ manganese + arsenic + lead + 
            education + age.mother.centered + IQ + 
            egg + meat + fish + smoking + 
            arsenic2 + manganese2 + lead2 + 
            mxa + mxl + axl, data = train.dat)

arsenic.mean <- mean(train.dat$arsenic)
lead.mean <- mean(train.dat$lead)

beta.manganese <- fit.full$coefficients[2]
beta.manganese2 <- fit.full$coefficients[13]
beta.mxa <- fit.full$coefficients[15]
beta.mxl <- fit.full$coefficients[16]

point.estimate <- beta.manganese*13+beta.manganese2*(17^2-4^2)+
  beta.mxa*arsenic.mean*13 + beta.mxl*lead.mean*13

txt <- paste("Point estimate =", round(point.estimate,3), sep=" ")
print(txt, quote=FALSE)

#Bootstrap the distribution of the marginal effect change.
B <- 1e4
n.samples <- dim(train.dat)[1]
causal <- matrix(0,B,1)
boot.lead.mean <- matrix(0,B,1)
boot.arsenic.mean <- matrix(0,B,1)

for(i in 1:B){
  sample <- sample(n.samples, n.samples, replace=TRUE)
  
  fit <- lm(Y ~ manganese + arsenic + lead + 
              education + age.mother.centered + IQ + 
              egg + meat + fish + smoking + 
              arsenic2 + manganese2 + lead2 + 
              mxa + mxl + axl, data = train.dat[sample,])
  
  arsenic.mean <- mean(train.dat$arsenic[sample])
  lead.mean <- mean(train.dat$lead[sample])
  
  beta.manganese <- fit$coefficients[2]
  beta.manganese2 <- fit$coefficients[13]
  beta.mxa <- fit$coefficients[15]
  beta.mxl <- fit$coefficients[16]
  
  causal[i] <- beta.manganese*13+beta.manganese2*(17^2-4^2)+
    beta.mxa*arsenic.mean*13 + beta.mxl*lead.mean*13
  
  #Bootstrapped arsenic and lead means for quality control.
  boot.arsenic.mean[i] <- arsenic.mean
  boot.lead.mean[i] <- lead.mean
}

#Histogram of results.
png("bootstrap_results.png")
hist(causal, freq=FALSE, xlim=c(-1.5,0.5),
     main="Distribution of Marginal Causal Effect of Changing Manganese",
     xlab="Marginal Effect",
     ylab="Density")
lines(density(causal), col="red", lwd=2)
dev.off()

#Print point estimate and 95% 
causal.mean <- mean(causal)
LB <- quantile(causal, 0.025)
UB <- quantile(causal, 0.975)

txt <- paste("Mean of bootstrap Causal Effect =", round(causal.mean,3), sep=" ")
print(txt, quote=FALSE)

txt <- paste("95% CI:", round(LB,3),",", round(UB,3), sep=" ")
print(txt, quote=FALSE)