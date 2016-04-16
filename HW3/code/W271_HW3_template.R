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

# Define functions

# THE FOLLOWING FUNCTIONS ARE JUST FOR FORMATTING PURPOSES

# A function to apply format
frmt <- function(qty, digits = 3) {
  formatC(qty, digits = digits, format = "f", drop0trailing = FALSE, 
          big.mark = ",")
}

# A function that codes significance level
sig_stars <- function(p) {
  stars = symnum(p, na = F, cutpoints = c(0, .001, .01, .05, .1, 1), 
                 symbols=c("**`***`**","**`** `**", "**`*  `**", "**.  **", 
                           "   "))
  return(stars)
}

# A function that draws a nice-looking table (following standard format for 
# publication) with the summary of the regression model 
create_regtable <- function(model, params, causes, effect) {
  model_summary <- summary(model)
  model_coefs <- model_summary$coefficients
  estimate <- unlist(lapply(c(seq(2, 1+length(params)), 1), function(x) 
    paste0(frmt(model_coefs[x, 1]), sig_stars(model_coefs[x, 4]))))
  SE <- unlist(lapply(c(seq(2, 1+length(params)), 1), function(x) 
    paste0("(", frmt(model_coefs[x, 2]), ")  ")))
  N <- paste0(length(model_summary$residuals), "   ")
  R2 <- paste0(frmt(model_summary$r.squared), "   ")
  Fsttstc <- model_summary$fstatistic
  Fstatistic <- paste0(frmt(Fsttstc["value"]), "   ")
  p <- pf(q = Fsttstc["value"], df1 = Fsttstc["numdf"], df2 = Fsttstc["dendf"], 
          lower.tail = FALSE)
  if (p < 0.001) {
    pvalue <- paste0(formatC(p, digits = 1, format = "e"), "   ")
  } else {
    pvalue <- paste0(frmt(p), "   ")
  }
  table <- matrix(c(t(matrix(c(estimate, SE), ncol = 2)), R2, Fstatistic, 
                    pvalue, N), ncol = 1)
  rows <- NULL
  for (cause in causes) {
    rows <- c(rows, paste("**", cause, "**", sep = ""), "")
  }
  rownames(table) <- c(rows, "Baseline (Intercept)", " ", "$R^2$", "F", "p", 
                       "N")
  colnames(table) <- effect
  return(table)
}

# Same function to draw table with regression results, but using robust SEs
create_regtable_RSEs <- function(model, params, causes, effect) {
  model_summary <- summary(model)
  require(sandwich, quietly = TRUE)
  require(lmtest, quietly = TRUE)
  newSE <- vcovHC(model)
  model_coefs <- coeftest(model, newSE)
  estimate <- unlist(lapply(c(seq(2, 1+length(params)), 1), function(x) 
    paste0(frmt(model_coefs[x, 1]), sig_stars(model_coefs[x, 4]))))
  SE <- unlist(lapply(c(seq(2, 1+length(params)), 1), function(x) 
    paste0("(", frmt(model_coefs[x, 2]), ")  ")))
  N <- paste0(length(model_summary$residuals), "   ")
  R2 <- paste0(frmt(model_summary$r.squared), "   ")
  Fsttstc <- waldtest(model, vcov = vcovHC)
  Fstatistic <- paste0(frmt(Fsttstc$F[2]), "   ")
  p <- Fsttstc$`Pr(>F)`[2]
  if (p < 0.001) {
    pvalue <- paste0(formatC(p, digits = 1, format = "e"), "   ")
  } else {
    pvalue <- paste0(frmt(p), "   ")
  }
  table <- matrix(c(t(matrix(c(estimate, SE), ncol = 2)), R2, Fstatistic, 
                    pvalue, N), ncol = 1)
  rows <- NULL
  for (cause in causes) {
    rows <- c(rows, paste("**", cause, "**", sep = ""), "")
  }
  rownames(table) <- c(rows, "Baseline (Intercept)", " ", "$R^2$", "F", "p", 
                       "N")
  colnames(table) <- effect
  return(table)
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

# Define constants



## @knitr Question1
# QUESTION 1 --------------------------------------------------------------
# Load the twoyear.RData dataset and describe the basic structure of the data
# Path relative to W271.Rproj, never to be run by the .Rmd (conflict with knitr)
# setwd('HW3/data')
load("twoyear.RData")



## @knitr Question2
# QUESTION 2 --------------------------------------------------------------
# Interpret the coefficients beta4 and beta8.



## @knitr Question3
# QUESTION 3 --------------------------------------------------------------
# Test that the return to university education is 7%.



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




## @knitr Question7
# QUESTION 7 --------------------------------------------------------------
# Including a square term of working experience to the regression model,
# estimate the linear regression model again.
# What is the estimated return to work experience in this model?



## @knitr Question8
# QUESTION 8 --------------------------------------------------------------
# Provide the diagnosis of the homoskedasticity assumption.
# Does this assumption hold?
# If so, how does it affect the testing of no effect of university education on
# salary change?
# If not, what potential remedies are available?
