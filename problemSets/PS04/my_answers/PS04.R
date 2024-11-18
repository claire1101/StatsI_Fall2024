### Problem set 4
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

# loading/running the programs as described in the assignment
install.packages("car")
library("car")
data(Prestige)
help(Prestige)
Prestige

## Question 1
# creating a new variable "professional" from "type" so that professionals = 1 and other (blue/white collar) = 0
Prestige$professional <- ifelse(Prestige$type == "prof", 1, 0)
# checking to see if it worked
print(Prestige)

# running a linear model with interaction
interact_reg <- lm(prestige ~ income * professional, data=Prestige)
summary(interact_reg)

# prediction equation
coefficients <- coef(interact_reg)
cat("prestige =", 
    coefficients[1], "+",  
    coefficients[2], "* income +",  
    coefficients[3], "* professional +",  
    coefficients[4], "* income * professional\n")  

## Question 2
# setting/defining the coefficients and se's
coef_signs <- 0.042
se_signs <- 0.016
coef_adjacent <- 0.042
se_adjacent <- 0.013
constant <- 0.302
r_squared <- 0.094
alpha <- 0.05
df <- 128 

#part a
t_signs <- coef_signs / se_signs
t_signs

#part b
t_adjacent <- coef_adjacent / se_adjacent 
t_adjacent













