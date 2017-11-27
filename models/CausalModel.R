# Causal Model
library(tidyverse)

train_dat <- read.csv("./data/train.csv") %>%
      mutate(X1X1 = X1*X1) %>% select(-c(ID,Z1))
test_dat <- read.csv("./data/test.csv") %>%
      mutate(X1X1 = X1*X1) %>% select(-c(ID,Z1))
pop_dat <- rbind(subset(train_dat, select=-Y), test_dat)

# univariate screening - associated w. exposure
var_list <- names(pop_dat)[!(names(pop_dat) %in% c("ID","Y", "X1X1", "X1"))]
conf1_list <- c()
p_list <- c()
for (i in 1:length(var_list)) {
      var_i <- var_list[i]
      reg1 <- paste0("X1~",var_i)
      fit_i <- lm(reg1, data=pop_dat)
      p_val <- summary(fit_i)$coefficients[2,4]
      if (p_val <= 0.05) {
            conf1_list <- c(conf1_list, var_i)
            p_list <- c(p_list, p_val)
            }
}

# univariate screening - associated w. outcome | exposure
conf2_list <- c()
p2_list <- c()
for (i in 1:length(var_list)) {
      var_i <- var_list[i]
      reg1 <- paste0("Y~X1+X1X1+",var_i)
      fit_i <- lm(reg1, data=train_dat)
      p_val <- summary(fit_i)$coefficients[2,4]
      if (p_val <= 0.05) {
            conf2_list <- c(conf2_list, var_i)
            p2_list <- c(p2_list, p_val)
      }
}

# assess multicollinearity
multico_dat <- pop_dat %>% select(-X1X1)
for (i in 1:ncol(multico_dat)) {
      var_i <- names(multico_dat)[i]
      multest <- paste0(var_i, "~.")
      vif <- 1/(1-summary(lm(multest, data=multico_dat))$r.squared)
      print(vif)
}

test model <- lm(Y~., train_dat))





