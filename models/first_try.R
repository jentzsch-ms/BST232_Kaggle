library(ridge)
library(ggplot2)

train.dat <- read.csv(file = "./data/train.csv")
test.dat <- read.csv(file = "./data/test.csv")

# run linearRidge using all covariates, excluding the ID column
m1 <- linearRidge(Y ~ ., data = train.dat[,-1])
m1.pred <- predict(m1, newdata = test.dat)


out.dat <- data.frame(ID = test.dat$ID, Y = m1.pred)
write.csv(out.dat, file = "./results/firsttry.csv")

