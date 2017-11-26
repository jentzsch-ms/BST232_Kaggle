rm(list = ls())
library(MXM)
source("RidgeReg.CrossVal1.R")
source("RidgeReg.CrossVal2.R")
source("RidgeReg.CrossVal3.R")
source("RidgeReg.CrossVal4.R")

#Load data and label columns.
DF.test <- read.csv("test.csv", header=TRUE)
DF.train <- read.csv("train.csv", header=TRUE)

vars <- c("ID", "manganese", "arsenic", "lead",
          "age", "education", "age.centered", "IQ", "home.quality",
          "meat", "fish", "eggs", "smoking")

colnames(DF.test) <- vars
colnames(DF.train) <- c(vars,"cog.score")
rm(vars)

#Remove the ridiculously low outlier.
#remove.index <- which(DF.train$cog.score<=200)
#DF.train <- DF.train[-remove.index,]

keep.index <- DF.train$cog.score>mean(DF.train$cog.score)-3*sd(DF.train$cog.score)
DF.train <- DF.train[keep.index, ]

#Simple descriptive statistics.
cor(DF.train[,2:dim(DF.train)[2]]) #correlation matrix

#Create a scatter plot for each covariate.
for(x.var in colnames(DF.train)[2:(dim(DF.train)[2]-1)]){
  file.name <- paste0(x.var,".png")
  png(file.name)
  
  plot(DF.train[,x.var], DF.train$cog.score,
       xlab=x.var, ylab="Cognitive Score")
  
  dev.off()
  rm(x.var, file.name)
}

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

DF.train$ln.cog.score <- log(DF.train$cog.score)

#Backward selection.
fit.full <- lm(cog.score~manganese+arsenic+lead+education+age.centered+IQ+home.quality+
                 meat+fish+eggs+smoking+manganeseXsmoking+arsenicXsmoking+leadXsmoking,
               data=DF.train)
step(fit.full, direction="backward")
#includes manganese, arsenic, education, age.centered, home.quality, fish, eggs,
#arsenicXsmoking

#Forward selection.
step(lm(cog.score~1, data=DF.train),~manganese+arsenic+lead+education+age.centered+
       IQ+home.quality+meat+fish+eggs+smoking+manganeseXsmoking+arsenicXsmoking+
       leadXsmoking, direction="forward")
#includes manganese, fish, home.quality, arsenicXsmoking, age.centered, education,
#leadXsmoking, arsenic, eggs

#Step-wise selection.
step(lm(cog.score~1, data=DF.train),~manganese+arsenic+lead+education+age.centered+
       IQ+home.quality+meat+fish+eggs+smoking+manganeseXsmoking+arsenicXsmoking+
       leadXsmoking, direction="both")
#includes manganese, fish, home.quality, arsenicXsmoking, age.centered, education,
#arsenic, eggs.

coeff <- RidgeReg.CrossVal1(DF.train$cog.score, DF.train, 10, 0)
beta <- matrix(colMeans(coeff))