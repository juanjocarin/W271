## MIDS W271-4 HW2            ##
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

# Functions to calculate Robust Standard Errors
# http://drewdimmery.com/robust-ses-in-r/
RSEs <- function(model){
  require(sandwich, quietly = TRUE)
  require(lmtest, quietly = TRUE)
  newSE <- vcovHC(model)
  coeftest(model, newSE)
}

# The following function has the same output than Stata
# But that could also be performed using the following in the previous function
# newSE <- vcovHC(model, type = "HC1")
# https://thetarzan.wordpress.com/2011/05/28/heteroskedasticity-robust-and-clustered-standard-errors-in-r/
summaryw <- function(model) {
  s <- summary(model)
  X <- model.matrix(model)
  u2 <- residuals(model)^2
  XDX <- 0
  # Here one needs to calculate X'DX. But due to the fact that D is huge (NxN), 
  # it is better to do it with a cycle.
  for(i in 1:nrow(X)) {
    XDX <- XDX + u2[i]*X[i,]%*%t(X[i,])
  }
  # inverse(X'X)
  XX1 <- solve(t(X)%*%X)
  # Variance calculation (Bread x meat x Bread)
  varcovar <- XX1 %*% XDX %*% XX1
  # degrees of freedom adjustment
  dfc <- sqrt(nrow(X))/sqrt(nrow(X)-ncol(X))
  # Standard errors of the coefficient estimates are the square roots of the 
  # diagonal elements
  stdh <- dfc*sqrt(diag(varcovar))
  t <- model$coefficients/stdh
  p <- 2*pnorm(-abs(t))
  results <- cbind(model$coefficients, stdh, t, p)
  dimnames(results) <- dimnames(s$coefficients)
  results
}

# THE FOLLOWING FUNCTIONS ARE JUST FOR FORMATTING PURPOSES

# A function to apply format (using formatC
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
  pvalue <- paste0(frmt(1 - pf(q = Fsttstc["value"], 
                               df1 = Fsttstc["numdf"], 
                               df2 = Fsttstc["dendf"])), "   ")
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
  model_coefs <- RSEs(model)
  estimate <- unlist(lapply(c(seq(2, 1+length(params)), 1), function(x) 
    paste0(frmt(model_coefs[x, 1]), sig_stars(model_coefs[x, 4]))))
  SE <- unlist(lapply(c(seq(2, 1+length(params)), 1), function(x) 
    paste0("(", frmt(model_coefs[x, 2]), ")  ")))
  N <- paste0(length(model_summary$residuals), "   ")
  R2 <- paste0(frmt(model_summary$r.squared), "   ")
  Fsttstc <- model_summary$fstatistic
  Fstatistic <- paste0(frmt(Fsttstc["value"]), "   ")
  pvalue <- paste0(frmt(1 - pf(q = Fsttstc["value"], 
                               df1 = Fsttstc["numdf"], 
                               df2 = Fsttstc["dendf"])), "   ")
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



## @knitr Load_Data
# LOAD DATA --------------------------------------------------------------
# Load the 401K contributions dataset
# Path relative to W271.Rproj, never to be run by the .Rmd (conflict with knitr)
# setwd('HW2/data')
load("401k_w271.Rdata")



## @knitr Question1-1
# QUESTION 1 --------------------------------------------------------------
# Examine the prate variable and comment on the shape of its distribution



## @knitr Question2-1
# QUESTION 2 --------------------------------------------------------------
# Examine the mrate variable and comment on the shape of its distribution



## @knitr Question3-1
# QUESTION 3 --------------------------------------------------------------
# Scatterplot of prate against mrate and linear regression of former on latter



## @knitr Question4
# QUESTION 4 --------------------------------------------------------------
# Assumption of zero-conditional mean: E[u|x] = 0



## @knitr Question5
# QUESTION 5 --------------------------------------------------------------
# ...



## @knitr Question6-1
# QUESTION 6 --------------------------------------------------------------
# ...




## @knitr Question7
# QUESTION 7 --------------------------------------------------------------
# ...



## @knitr Question8
# QUESTION 8 --------------------------------------------------------------
# ...
