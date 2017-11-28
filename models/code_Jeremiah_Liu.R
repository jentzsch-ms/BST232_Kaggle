final_model <- 
  function(data_name = "train.csv") {
    # Step 1: read-in and attach data (DON't CHANGE)
    data_232 <- read.csv(data_name)
    attach(data_232)
    
    # Step 2:
    # define the formula of your final chosen model,
    # do not modify the variable names for exposures (i.e. X1, X2, X3)
    final_formula <- Y ~ X1 + X2 + X3 + X1*X2 + Z1 + Z2
    
    # Step 3:
    # fit formula using linear regression, then return the fitted object
    lm_submit <- lm(formula = final_formula)
    return(lm_submit)
  }

# now test if your function returns correct output
print(final_model("train.csv"))
