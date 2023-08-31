# Regressions
# M.L.R (O.L.S)
  # General Linear Models
  # Y is between -inf and inf
  # estimate parameter mu
  # y is assumed to be normally distributed
  # E(y|x1...xk)=b0+b1x1+...+xkbk
    # Cannot use this for Logistic regression, why? 
      # Because p is a number between 0 and 1 we cannot fit a linear equation into it
      # Similarly with lambda it must be a positive number.
# Logistic regression
  # Y is either 1 or 0
  # estimate p-> probability of success.
  # Bernoulli distributed
  # Logit(p)= log(p/(1-p))= Linear Equation
#Poisson regression
  #Generalized Linear Models
  # y is in between 0 and positive inf (integers only)
  # estimate lambda (mean and variance)
  # Poisson Distributed
  # Log(lambda) = Linear Equation
# In all three models we estimate 


#Final

# Very basic from multiple linear regression
  # When to use adjusted R^2 and what R^2 is.
    # adjusted R^2 is for when we are model selection. Not R^2.
  # Predict the value of a target variable suing regression output. 
  # The p-values, of the coefficients and the model. How to interpret that? 
  # Coefficient estimates and what they mean. (multiple regression, controlling for the other independent variables)
  # For discrete variables, the reference value. 
    # How being in a particular dummy variable affects the target variable relative to the reference (control) value

# Where is the AIC and BIC used? model selection
# BIC has a stronger penalty factor for model complexity. 
# More robust models you use BIC. Lower AIC and BIC are better

#Logistic regression
  # Predict from the output.
  # When to use this or the others?
    # Boolean, 0 to 1
    # multiple is a continuous variable
    # poisson is for count variable

#Poisson Regression
  # predict from the output. take the exp of the linear equation.
  # anova function can compare the residual deviances automatically.

#Hypothesis test to compare models.(subtract residual deviances from each other)
# How to check whether using the model is better than using the average value of your target variable.