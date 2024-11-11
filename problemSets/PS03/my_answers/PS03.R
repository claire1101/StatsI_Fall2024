### Problem set 3
getwd()
# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}
# load packages
lapply(c("stargazer","vioplot","arm","broom","ggplot2","fastDummies"),  pkgTest)

incumbents <- read.csv("/Users/claireott/Documents/incumbents_subset.csv")

##Question 1

# estimate the regression manually
  lm_by_hand <- function(data, predictors, outcome) {
    # ensure predictors is a vector of column names
    if (!is.character(predictors)) predictors <- as.character(predictors)
    
    # creating matrices
    X <- as.matrix(cbind(1, data[, predictors]))  # add a column of 1s for the intercept
    Y <- as.matrix(data[, outcome])
    
    # calculating betas (coefficients)
    betas <- solve(t(X) %*% X) %*% (t(X) %*% Y)
    rownames(betas) <- c("Intercept", predictors)
    
    # number of observations and parameters
    n <- nrow(X)
    k <- ncol(X)
    
    # estimating sigma^2 (variance of the residuals)
    residuals <- Y - X %*% betas
    sigma_squared <- sum(residuals^2) / (n - k)
    
    # covariance matrix for betas
    var_covar_mat <- sigma_squared * solve(t(X) %*% X)
    
    # SEs for coefficient estimates
    SEs <- sqrt(diag(var_covar_mat))
    
    # t-statistics and p-values
    t_stats <- betas / SEs
    p_values <- 2 * pt(abs(t_stats), df = n - k, lower.tail = FALSE)
    
    # return all results in a list
    return(list(
      coefficients = betas,
      standard_errors = SEs,
      t_statistics = t_stats,
      p_values = p_values,
      residuals = residuals,
      sigma_squared = sigma_squared,
      var_covar_matrix = var_covar_mat
    ))
  }


#trying this
result1 <- lm_by_hand(data = incumbents, predictors = "difflog", outcome = "voteshare")
result1
# print results
print(result1$coefficients)       # coefficients
print(result1$standard_errors)    # SEs
print(result1$t_statistics)       # t-statistics
print(result1$p_values)           # p-values


#trying the built-in lm function
auto_results1 <- lm(voteshare ~ difflog, data= incumbents)
summary(auto_results1)

#making a scatterplot
ggplot(incumbents, aes(x = difflog, y = voteshare)) +
  geom_point(size = 1) +  # adjust the size of the points
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Vote Share vs Difference in Log Spending", 
       x = "Difference in Log Spending", 
       y = "Vote Share")

#getting/saving residuals
residuals1 <- resid(auto_results1)
residuals1

#writing the prediction equation
cat("Prediction equation: voteshare =", coef(auto_results1)[1], "+", coef(auto_results1)[2], "* difflog")

## Question 2

#running a regression where outcome variable is presvote and explanatory is difflog 
#using the function from Q1
result2 <- lm_by_hand(data = incumbents, predictors = "difflog", outcome = "presvote")
result2
# print results
print(result2$coefficients)       # coefficients
print(result2$standard_errors)    # SEs
print(result2$t_statistics)       # t-statistics
print(result2$p_values)           # p-values

#trying the built-in lm function
auto_results2 <- lm(presvote ~ difflog, data= incumbents)
summary(auto_results2)

#making a scatterplot
ggplot(incumbents, aes(x = difflog, y = presvote)) +
  geom_point(size = 1) +  # adjust the size of the points
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Vote Share vs Difference in Log Spending", 
       x = "Difference in Log Spending", 
       y = "Vote Share")

#getting/saving residuals
residuals2 <- resid(auto_results2)
residuals2

#write the prediction equation
cat("Prediction equation: presvote =", coef(auto_results2)[1], "+", coef(auto_results2)[2], "* difflog")

## Question 3

#running a regression where outcome variable is voteshare and explanatory is presvote 
#using the function from Q1
result3 <- lm_by_hand(data = incumbents, predictors = "presvote", outcome = "voteshare")
result3
# print results
print(result3$coefficients)       # coefficients
print(result3$standard_errors)    # SEs
print(result3$t_statistics)       # t-statistics
print(result3$p_values)           # p-values

#trying the built-in lm function
auto_results3 <- lm(voteshare ~ presvote, data= incumbents)
summary(auto_results3)

#making a scatterplot
ggplot(incumbents, aes(x = presvote, y = voteshare)) +
  geom_point(size = 1) +  # adjust the size of the points
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Vote Share vs Electoral Success", 
       x = "Electoral Success", 
       y = "Vote Share")

#getting/saving residuals
residuals3 <- resid(auto_results3)
residuals3

#getting residuals from the lm_by_hand function 
residuals_3 <- result3$residuals

#writing the prediction equation
cat("Prediction equation: voteshare =", coef(auto_results3)[1], "+", coef(auto_results3)[2], "* presvote")

## Question 4

#running a regression where outcome variable is Q1 residuals and explanatory is Q2 residuals 
## with the built-in lm function
auto_results4 <- lm(residuals1 ~ residuals2)
summary(auto_results4)

#making a scatterplot
ggplot(incumbents, aes(x = residuals2, y = residuals1)) +
  geom_point(size = 1) +  # adjust the size of the points
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Q1 Residuals vs Q2 Residuals", 
       x = "Q2 residuals", 
       y = "Q1 residuals")

#write the prediction equation
cat("Prediction equation: residuals1 =", coef(auto_results4)[1], "+", coef(auto_results4)[2], "* residuals2")

## Question 5

#running a regression where outcome variable is voteshare and explanatory are difflog and presvote 
#using the function from Q1
result5 <- lm_by_hand(data = incumbents, predictors = c("difflog", "presvote"), outcome = "voteshare")
result5
# print results
print(result5$coefficients)       # coefficients
print(result5$standard_errors)    # SEs
print(result5$t_statistics)       # t-statistics
print(result5$p_values)           # p-values

#trying the built-in lm function
auto_results5 <- lm(voteshare ~ difflog + presvote, data = incumbents)
summary(auto_results5)

# write the prediction equation
cat("Prediction equation: voteshare =", coef(auto_results5)[1], "+", coef(auto_results5)[2], "* difflog +", coef(auto_results5)[3], "* presvote")

#comparing with results from Q4 to see what is identical
# compare the coefficients
identical(auto_results4$coefficients, auto_results5$coefficients)  # TRUE if identical
# compare the residuals
identical(auto_results4$residuals, auto_results5$residuals)  # TRUE if identical
# compare the variance-covariance matrices
identical(auto_results4$var_covar_matrix, auto_results5$var_covar_matrix)  # TRUE if identical
# compare sigma_squared (estimated residual variance)
identical(auto_results4$sigma_squared, auto_results5$sigma_squared)  # TRUE if identical









