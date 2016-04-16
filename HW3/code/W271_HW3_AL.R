## MIDS W271-4 HW3            ##
## Carin, Davis, Levato, Song ##


## @knitr Libraries-Functions-Constants
# LIBRARIES, FUNCTIONS AND CONSTANTS -------------------------------------------------
# Load libraries
library(e1071)
library(ggplot2)
library(ggfortify)
library(knitr)
library(pastecs)
library(lmtest)
library(stargazer)

# Define functions

# THE FOLLOWING FUNCTIONS ARE JUST FOR FORMATTING PURPOSES

# A function to apply format
frmt <- function(qty, digits = 3) {
  formatC(qty, digits = digits, format = "f", drop0trailing = FALSE, 
          big.mark = ",")
}

# Functions to count & refer figures & tables
# http://rmflight.github.io/posts/2012/10/papersinRmd.html
incCount <- function(inObj, useName) {
  nObj <- length(inObj)
  useNum <- max(inObj) + 1
  inObj <- c(inObj, useNum)
  names(inObj)[nObj + 1] <- useName
  inObj
}
pasteLabel <- function(preText, inObj, objName, insLink = TRUE) {
  objNum <- inObj[objName]
  
  useText <- paste(preText, objNum, sep = " ")
  if (insLink) {
    useText <- paste("[", useText, "](#", objName, ")", sep = "")
  }
  useText
}
# Counters for Figures and Tables
figCount <- c(`_` = 0)
tableCount <- c(`_` = 0)



## @knitr Question1
# QUESTION 1 --------------------------------------------------------------
# Load the twoyear.RData dataset and describe the basic structure of the data
# Path relative to W271.Rproj, never to be run by the .Rmd (conflict with knitr)
#setwd('HW3/data')
load("twoyear.RData")
desc 
summary(data)


## @knitr Question2
# QUESTION 2 --------------------------------------------------------------
# Interpret the coefficients beta4 and beta8.
data$experXblack<-data$exper*data$black
model1<-lm(lwage~jc+univ+exper+black+hispanic+AA+BA+experXblack, data=data)
stargazer(model1, type="text")
coeftest(model1, vcov = vcovHC)


## @knitr Question3
# QUESTION 3 --------------------------------------------------------------
# Test that the return to university education is 7%.
coeffs<-coefficients(model1)
coeffs[3]


## @knitr Question4
# QUESTION 4 --------------------------------------------------------------
# Test that the return to junior college education is equal for black and 
# non-black.



## @knitr Question5
# QUESTION 5 --------------------------------------------------------------
# Test whether the return to university education is equal to the return to 
# 1 year of working experience.



## @knitr Question6
# QUESTION 6 --------------------------------------------------------------
# Test the overall significance of this regression.
summary(model1)



## @knitr Question7
# QUESTION 7 --------------------------------------------------------------
# Including a square term of working experience to the regression model,
# estimate the linear regression model again.
# What is the estimated return to work experience in this model?
data$experSq<-data$exper^2
model2<-lm(lwage~jc+univ+exper+black+hispanic+AA+BA+experXblack+experSq, data=data)
stargazer(model2, type="text")
coeftest(model2, vcov = vcovHC)


## @knitr Question8
# QUESTION 8 --------------------------------------------------------------
# Provide the diagnosis of the homoskedasticity assumption.
# Does this assumption hold?
# If so, how does it affect the testing of no effect of university education on
# salary change?
# If not, what potential remedies are available?
plot(model2, which=1)
