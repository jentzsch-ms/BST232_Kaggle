final_model <- 
  function(data_name = "train.csv") {
    # Step 1: read-in and attach data (DON't CHANGE)
    data_232 <- read.csv(data_name)
    attach(data_232)
    
    # Step 2:
    # define the formula of your final chosen model,
    # do not modify the variable names for exposures (i.e. X1, X2, X3)
    
    # final causal model formula
    final_formula <- final_formula_pred <- Y ~ X1 + X2 + X3 + Z1 + Z2 + Z5 + Z6 + Z7 + Z9 + I(X1^2) + X1*X2 + X2*X3 + X1*Z1 + X1*Z6 + X1*Z7 + X1*Z9 + 
      X2*Z1 + X2*Z7 + X3*Z1 + X3*Z2
    
    # Step 3:
    # fit formula using linear regression, then return the fitted object
    
    #final causal model
    lm_submit_pred <- lm(formula = final_formula)
    
    
    return(lm_submit_pred)
  }

# now test if your function returns correct output
print(final_model("./data/train.csv"))
