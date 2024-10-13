#### Problem set 2
## remove objects and detach libraries
rm(list = ls())
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search())) == 1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list) > 0)  for (package in package.list) detach(package,  character.only = TRUE)
}
detachAllPackages()
## load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

## Question 1
## part a
## creating a matrix for the data
crossroads <- matrix(c(14,6,7,7,7,1), nrow=2, byrow=TRUE)
## getting expected frequencies from row/column/grand totals
row_totals <- rowSums(crossroads)
col_totals <- colSums(crossroads)
grand_total <- sum(crossroads)
expectedf <- outer(row_totals, col_totals)/grand_total
print(expectedf)
## calculate chi-squared
chi_squared <- sum((crossroads-expectedf)^2/expectedf)
print(chi_squared)

## part b
## getting degrees of freedom
df <- (nrow(crossroads)-1)*(ncol(crossroads)-1)
print(df)
## getting p-value
p_value <- 1-pchisq(chi_squared,df)
print(p_value)

## part c
## getting standardized residuals
standard_res <- (crossroads-expectedf)/sqrt(expectedf)
print(standard_res)


#### Question 2
## part a 
women <- read.csv("C:\\Users\\OTTC\\Downloads\\women.csv")
print(women)

women$female
women$water

## trying to do a regression 
water_female_model <- lm(women$water~women$female, data=women)
water_female_model
summary(water_female_model)
print(water_female_model$coefficients)

plot(women$female,women$water, 
     xlab="Male leader=0, Female leader=1",
     ylab="Number of new/repaired drinking water facilities",
     main="Scatter Plot of Water vs Female",
     abline((water_female_model), col="red")) ## added in the regression line and colored it red



