## MIDS W271-4 HW4            ##
## Carin, Davis, Levato, Song ##


## @knitr Libraries-Functions-Constants
# LIBRARIES, FUNCTIONS AND CONSTANTS -------------------------------------------------
# Load libraries
library(e1071)
library(ggplot2)
library(ggfortify)
library(scales)
library(knitr)
library(pastecs)
library(car)
library(sandwich)
library(lmtest)
library(dplyr)
library(stargazer)
library(texreg)

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
                 symbols=c("$^{***}$","$^{**}$", "$^{*}$", 
                           "$^{\\mathbf{\\cdot}}$", ""))
  return(stars)
}

# A function that draws a nice-looking table, based on stargazer
# USING HETEROSKEDASTICTY-ROBUST STANDARD ERRORS AND F STATISTIC
stargazer2 <- function(models, type = 'latex', ...) {
  if (class(models) != "lm") {
    model_list <- models
  } else {
    model_list <- list()
    model_list[[1]] <- models
  }
  stargazer(model_list, type = type, header = FALSE, table.placement = "h!", 
            star.cutoffs = c(0.1, 0.05, 0.01, 0.001), 
            star.char = c("\\mathbf{\\cdot}", "*", "**", "***"), 
            notes.append = FALSE, notes.label = "", 
            notes = "$\\cdot$p<0.1; *p<0.05; **p<0.01; ***p<0.001", 
            se = lapply(model_list, function(m) coeftest(m, vcovHC(m))[, 2]), 
            p = lapply(model_list, function(m) coeftest(m, vcovHC(m))[, 4]), 
            add.lines = 
              list(c("F Statistic", unlist(lapply(model_list, function(m) 
                paste0(frmt(waldtest(m, vcov=vcovHC)[2,3]), 
                       sig_stars(waldtest(m, vcov=vcovHC)$`Pr(>F)`[2]))))), 
                c("df", unlist(lapply(model_list, function(m) 
                  paste0(abs(waldtest(m, vcov=vcovHC)$Df[2]), "; ", 
                         waldtest(m, vcov=vcovHC)$Res.Df[1]))))), 
            df = FALSE, no.space = TRUE, omit.stat = "f", ...)
}

# Another (2) function(s) to draw tables, based on stargazer
# also USING HETEROSKEDASTICTY-ROBUST STANDARD ERRORS AND F STATISTIC
createTexreg2 <- function(model, ...) {
  model_summary <- summary(model)
  coefs <- coeftest(model, vcovHC(model))
  f <- waldtest(model, vcov=vcovHC)
  createTexreg(coef = coefs[, 1], coef.names = rownames(coefs), 
               se = coefs[, 2], pvalues = coefs[, 4], 
               gof = c(model_summary$r.squared, model_summary$adj.r.squared, 
                       f$F[2], length(model$residuals)), 
               gof.names = c("R$^2$", "R$^2_{\\text{adj}}$", "F", "N"), 
               gof.decimal = c(rep(TRUE, 3), FALSE), ...)
}

texreg2 <- function(list, ...) {
  texreg(list, digits = 3, caption.above = TRUE, bold = 0.05, float.pos = 'h!', 
         stars = c(0.001, 0.01, 0.05, 0.1), symbol = "\\cdot", ...)
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



## @knitr Question1-1
# QUESTION 1 --------------------------------------------------------------
# Examine and summarize the dataset.
# How many observations and variables are there?
# Examine the variables of key interest: apps, bowl, btitle, and finfour.
# Path relative to W271.Rproj, never to be run by the .Rmd (conflict with knitr)
# setwd('HW4/data')
load("athletics.RData")
desc
summary(data)

## @knitr Question1-2
#exploring the number of applications
summary(data$apps)

ggplot(data = data, aes(apps)) + 
  geom_histogram(colour = 'black', fill = 'white', 
                 binwidth = 1000) +  
  labs(x = "Number of applications", y = "Count", 
       title = "Histogram of applications")

## @knitr Question1-3
#exploring bowl
summary(data$bowl)

ggplot(data = data, aes(bowl)) + 
  geom_histogram(colour = 'black', fill = 'white', 
                 binwidth = .5) +  
  labs(x = "bowl", y = "Count", 
       title = "Histogram of bowl")

## @knitr Question1-4
#exploring btitle
summary(data$btitle)

ggplot(data = data, aes(btitle)) + 
  geom_histogram(colour = 'black', fill = 'white', 
                 binwidth = .5) +  
  labs(x = "Number of applications", y = "Count", 
       title = "Histogram of finfour")


## @knitr Question1-5
#exploring finfour
summary(data$finfour)

ggplot(data = data, aes(finfour)) + 
  geom_histogram(colour = 'black', fill = 'white', 
                 binwidth = .5) +  
  labs(x = "Number of applications", y = "Count", 
       title = "Histogram of finfour")



## @knitr Question2-1
# QUESTION 2 --------------------------------------------------------------
# To prepare for a difference-in-difference analysis, transfer the dataset to 
# wide-format. Each school should have a single row of data, with separate 
# variables for 1992 and 1993.
# Create a new variable, clapps to represent the change in the log of the 
# number of applications from 1992 to 1993.
# Examine this variable and its distribution.
# Which schools had the greatest increase and the greatest decrease in number 
# of log applications?
library(reshape2)
wideData<- reshape(data, timevar="year"
                   idvar="school", v.names=c("apps", "bowl"."btitle", "finfour"), 
                   direction="wide")


## @knitr Question2-2
wideData$clapps<-wideData$lapps==1993-wideData$lapps==1992)

## @knitr Question2-3
summary(wideData$clapps)
ggplot(wideData = wideData, aes(clapps)) + 
  geom_histogram(colour = 'black', fill = 'white', 
                 binwidth = .5) +  
  labs(x = "Change in log applicants", y = "Counts", 
       title = "Histogram of log of college applications")


## @knitr Question2-4
wideData[wideData$clapps == max(wideData$clapps), 'school']
wideData[wideData$clapps == min(wideData$clapps), 'school']


## @knitr Question3
# QUESTION 3 --------------------------------------------------------------
# Create 3 variables, cperf, cbball, and cbowl to represent the changes in the 
# 3 athletic success variables.
# Which of these variables has the highest variance?

wideData$cperf<-wideData$btitle.1993-wideData$btitle.1992
wideData$cbball<-wideData$finfour.1993-wideData$finfour.1992
wideData$cbowl<-wideData$bowl.1993-wideData$bowl.1992
  
var(wideData$cperf)
var(wideData$cbball)
var(wideData$cbowl)


## @knitr Question4
# QUESTION 4 --------------------------------------------------------------
# ...



## @knitr Question5
# QUESTION 5 --------------------------------------------------------------
# Estimate the first-difference model given above.
# interpret the slope coefficients and comment on their statistical 
# significance and practical significance.

mod<-lm(clapps~cperf+cbball+cbowl, data=wideDate)
coeftest(mod, vcov=vcovHC)
stargazer(mod, title = "Regression Model Output")


## @knitr Question6
# QUESTION 6 --------------------------------------------------------------
# Test the joint signifance of the three indicator variables. 
# What impact does the result have on your conclusions?
  
