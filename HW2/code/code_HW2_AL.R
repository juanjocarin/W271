## MIDS W271-4 HW2            ##
## Carin, Davis, Levato, Song ##


## @knitr Libraries-Functions-Constants
# LIBRARIES, FUNCTIONS AND CONSTANTS -------------------------------------------------
# Load libraries
library(ggplot2)
library(knitr)
library(pastecs)

# Define functions
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
create_regtable <- function(model, df, params, causes, effect) {
  model_summary <- summary(model)
  model_coefs <- model_summary$coefficients
  estimate <- unlist(lapply(c(seq(2, 1+length(params)), 1), function(x) 
    paste0(frmt(model_coefs[x, 1]), sig_stars(model_coefs[x, 4]))))
  SE <- unlist(lapply(c(seq(2, 1+length(params)), 1), function(x) 
    paste0("(", frmt(model_coefs[x, 2]), ")  ")))
  N <- paste0(nrow(df), "   ")
  R2 <- paste0(frmt(model_summary$r.squared), "   ")
  Fstatistic <- paste0(frmt(model_summary$fstatistic[1]), "   ")
  pvalue <- paste0(frmt(1 - pf(model_summary$fstatistic[1], 2, 300)), "   ")
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
figCount <- c(`_` = 0)
tableCount <- c(`_` = 0)
# Define constants


## @knitr Load_Data
# LOAD DATA --------------------------------------------------------------
load("data\\401k_w271.Rdata")

## @knitr Question1-1
# QUESTION 1 --------------------------------------------------------------
# looking at the whole dataset
desc
str(data)
summary(data)
# examine the prate data
summary(data$prate)
quantile(data$prate, probs = c(1, 5, 10, 25, 50, 75, 90, 95, 99, 
                                     100)/100)
#check to see if there are outliers
data$prate[data$prate > 100]

## @knitr Question1-2
# Plots: histogram
ggplot(data = data, aes(prate)) + 
  geom_histogram(aes(y = (..count..)/sum(..count..)), colour = 'black', 
                 fill = 'white', binwidth = 5) + 
  labs(x = "Percentage of employees participating in 401K", 
       y = "Count", 
       title = "Histogram of Participation")

## @knitr Question1-3
ggplot(data = data[data$prate <= 100, ], aes(prate)) + 
  geom_histogram(aes(y = (..count..)/sum(..count..)), colour = 'black', 
                 fill = 'white', binwidth = 5) + 
  labs(x = "Percentage of employees participating in 401K", 
       y = "frequency", 
       title = "Histogram of Participation")


## @knitr Question2-1
# QUESTION 2 --------------------------------------------------------------
# omit the outliers in the prate data
data2 <- data[data$prate <= 100, ]
# examine mrate
summary(data2$mrate)
quantile(data2$mrate, probs = c(1, 5, 10, 25, 50, 75, 90, 95, 99, 
                                      100)/100)

## @knitr Question2-2
# Plots: histogram
ggplot(data = data, aes(mrate)) + 
  geom_histogram(aes(y = (..count..)/sum(..count..)), colour = 'black', 
                 fill = 'white', binwidth = .5) + 
  labs(x = "Percentage employees in 401K plan", 
       y = "Count", 
       title = "Histogram of match rate")


## @knitr Question3-1
# QUESTION 3 --------------------------------------------------------------
# Scatterplot of prate and mrate
ggplot(data = data2, aes(mrate, prate)) + 
  geom_point() + 
  labs(x = "Match rate (%)", 
       y = "Participation rate (%)", 
       title = "Participation rate vs Match rate") + 
  geom_smooth(method = "lm")

## @knitr Question3-2
model<-lm(prate~mrate, data2)
summary(model)
regression<-summary(model)
#slope coefficient
slope<-regression$coefficient[2,1]
slope


## @knitr Question4
# QUESTION 4 --------------------------------------------------------------
# use the output plots from lm to view the residuals vs fitted plot
plot(model)

## @knitr Question5
# QUESTION 5 --------------------------------------------------------------
# ...


## @knitr Question6
# QUESTION 6 --------------------------------------------------------------
# ...


## @knitr Question7
# QUESTION 7 --------------------------------------------------------------
# pull the standard error of the slope coefficient from the lm output
slopeSE<-regression$coefficient[2,2]
slopeSE

## @knitr Question8
# QUESTION 8 --------------------------------------------------------------
# pull the p-vale from the output of the lm
pVal<-regression$coefficient[2,4]
pVal
