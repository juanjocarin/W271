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
# setwd('.\\HW4\\data')
load("athletics.RData")
desc
params <- c('apps', 'bowl', 'btitle', 'finfour')
round(stat.desc(data[,append(params, 'lapps')], desc = TRUE, basic = TRUE), 2)


## @knitr Question1-2
#Histogram off application variable
ggplot(data, aes(apps)) +
  geom_histogram(colour='black', fill = 'white', binwidth = 1000) +
  labs(x = 'Applicants',
       y = 'Frequency',
       title = 'Histogram of College Applications,\n 1992 - 1993')

## @knitr Question1-3
#Histogram of log of application variable
ggplot(data, aes(lapps)) +
  geom_histogram(colour='black', fill = 'white', binwidth = .1) +
  labs(x = 'log(Applicants)',
       y = 'Frequency',
       title = 'Histogram of Log of College Applications,\n 1992 - 1993')

## @knitr Question1-4
#Histogram of bowl appearance
ggplot(data, aes(factor(bowl, labels = c('No bowl game prev yr',
                                         'Bowl game prev yr')))) +
  geom_bar(colour='black', fill = 'white') + 
  labs(title = "Bowl Appearances, 1992 - 1993", x = element_blank())

## @knitr Question1-5
#Histogram of football conference titles
ggplot(data, aes(factor(btitle, labels = c('No conf. champ prev yr',
                                            'Conf. champ prev yr')))) +
  geom_bar(colour='black', fill = 'white') + 
  labs(title = "College Football Conference\n Championships, 1992 - 1993",
       x = element_blank())

## @knitr Question1-6
#Histogram of final four appearances
ggplot(data, aes(factor(finfour, labels = c('No final four prev yr',
                                           'Final four prev yr')))) +
  geom_bar(colour='black', fill = 'white') + 
  labs(title = "Final Four Appearancces, 1992 - 1993",
       x = element_blank())

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

#Transer dataset to wide-format
wide <-  reshape(data, 
                   v.names=c("apps", "top25", "ver500", "mth500", "stufac",
                             "bowl", "btitle", "finfour", "lapps", "avg500", "bball", "perf"),
                   idvar = "school", timevar = "year", direction = "wide")


## @knitr Question2-2
wide$clapps <- wide$lapps.1993 - wide$lapps.1992
params1 <- c('apps.1992', 'apps.1993','lapps.1992', 'lapps.1993', 'clapps',
             'bowl.1992', 'bowl.1993', 'btitle.1992', 'btitle.1993',
             'finfour.1992', 'finfour.1993')
round(stat.desc(wide[, params1]), 2)

## @knitr Question2-3
ggplot(wide, aes(clapps)) +
  geom_histogram(colour='black', fill = 'white', binwidth = .1) +
  labs(x = 'Change in Logged Applicants from 1992 to 1993',
       y = 'Frequency',
       title = 'Histogram of Change in Logged\n College Applications, 1992 - 1993')

## @knitr Question2-4
wide[wide$clapps == max(wide$clapps), 'school']
wide[wide$clapps == min(wide$clapps), 'school']

## @knitr Question3
# QUESTION 3 --------------------------------------------------------------
# Create 3 variables, cperf, cbball, and cbowl to represent the changes in the 
# 3 athletic success variables.
# Which of these variables has the highest variance?

## @knitr Question3-1
wide$cfinfour <-  wide$finfour.1993 - wide$finfour.1992
wide$cbtitle <- wide$btitle.1993 - wide$btitle.1992
wide$cbowl <- wide$bowl.1993 - wide$bowl.1992
c(var(wide$cfinfour), var(wide$cbtitle), var(wide$cbowl))


## @knitr Question4
# QUESTION 4 --------------------------------------------------------------
# ...



## @knitr Question5
# QUESTION 5 --------------------------------------------------------------
# Estimate the first-difference model given above.
# interpret the slope coefficients and comment on their statistical 
# significance and practical significance.

## @knitr Question5-1
regressor <- 'clapps'
params2 <- c('cfinfour', 'cbtitle', 'cbowl')
model <- lm(as.formula(paste(regressor, paste(params2, sep = "", 
                                              collapse = " + "), sep = " ~ ")),
            data =wide)
#coeftest(model, vcov=vcovHC)
stargazer2(model, title = "Regression summary", digits = 4, digits.extra = 6, 
           dep.var.labels = 'clapps')



## @knitr Question6
# QUESTION 6 --------------------------------------------------------------
# Test the joint signifance of the three indicator variables. 
# What impact does the result have on your conclusions?
  
