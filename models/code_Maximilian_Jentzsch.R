final_model <- 
  function(data_name = "train.csv") {
    # Step 1: read-in and attach data (DON't CHANGE)
    data_232 <- read.csv(data_name)
    attach(data_232)
    
    # Step 2:
    # define the formula of your final chosen model,
    # do not modify the variable names for exposures (i.e. X1, X2, X3)
    
    # final causal model formula
    final_formula <- Y ~ X1 + I(X1^2) + X2 + I(X2^2) + X3 + I(X3^2) + Z2 + Z3 + Z4 + Z6 + Z7 + Z8 + Z9 + X1*X2 + X1*X3 + X2*X3
    
    # Step 3:
    # fit formula using linear regression, then return the fitted object
    
    #final causal model
    lm_submit <- lm(formula = final_formula)
    
    
    return(lm_submit)
  }

# now test if your function returns correct output
print(final_model("./data/train.csv"))

