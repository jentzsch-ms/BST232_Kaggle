rm(list = ls())
library(MXM)
source("./models/RidgeReg.CrossVal.R")

#Load data and label columns.
DF.test <- read.csv("./data/test.csv", header=TRUE)
DF.train <- read.csv("./data/train.csv", header=TRUE)

vars <- c("ID", "manganese", "arsenic", "lead",
          "age", "education", "age.centered", "IQ", "home.quality",
          "meat", "fish", "eggs", "smoking")

colnames(DF.test) <- vars
colnames(DF.train) <- c(vars,"cog.score")
rm(vars)

#Remove the ridiculously low outlier.
remove.index <- which(DF.train$cog.score<=200)
DF.train <- DF.train[-remove.index,]

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

#Square the heavy metals.
DF.train$manganese2 <- DF.train$manganese^2
DF.train$arsenic2 <- DF.train$arsenic^2
DF.train$lead2 <- DF.train$lead^2

DF.test$manganese2 <- DF.test$manganese^2
DF.test$arsenic2 <- DF.test$arsenic^2
DF.test$lead2 <- DF.test$lead^2

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

coeff <- RidgeReg.CrossVal(DF.train$cog.score, DF.train, 10, c(0,1,10,100))
beta <- matrix(colMeans(coeff))

