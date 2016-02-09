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
library(sandwich)
library(lmtest)

# Define functions

# Functions to calculate Robust Standard Errors (NOT USED)
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
  model_coefs <- coeftest(model, vcovHC(model))
  estimate <- unlist(lapply(c(seq(2, 1+length(params)), 1), function(x) 
    paste0(frmt(model_coefs[x, 1]), sig_stars(model_coefs[x, 4]))))
  SE <- unlist(lapply(c(seq(2, 1+length(params)), 1), function(x) 
    paste0("(", frmt(model_coefs[x, 2]), ")  ")))
  N <- paste0(length(model_summary$residuals), "   ")
  R2 <- paste0(frmt(model_summary$r.squared), "   ")
  Fsttstc <- waldtest(model, vcov = vcovHC(model))
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



## @knitr Load_Data
# LOAD DATA --------------------------------------------------------------
# Load the 401K contributions dataset
# Path relative to W271.Rproj, never to be run by the .Rmd (conflict with knitr)
# setwd('HW2/data')
load("401k_w271.Rdata")



## @knitr Question1-1
# QUESTION 1 --------------------------------------------------------------
# Examine the prate variable and comment on the shape of its distribution
# Descriptive statistics of the whole dataset
desc
str(data)
summary(data)
# Descriptive statistics of prate
summary(data$prate)
round(stat.desc(data$prate, desc = TRUE, basic = TRUE, norm = TRUE), 2)
round(quantile(data$prate, probs = c(1, 5, 10, 25, 50, 75, 90, 95, 99, 
                                     100)/100), 1)
data$prate[data$prate > 100]

## @knitr Question1-2
# Plots: histogram
bin_width = 5
ggplot(data = data, aes(prate)) + 
  geom_histogram(aes(y = (..count..)/sum(..count..)), colour = 'black', 
                 fill = 'white', binwidth = bin_width) + 
  labs(x = "Percentage of a company's employees\nparticipating in its 401K plan", 
       y = "Relative frequency", 
       title = "Histogram of participation rate (%) in 401K plans")
figCount <- incCount(figCount, "hist-Q1")


## @knitr Question1-3
# Plots: density
ggplot(data = data, aes(prate)) + 
  geom_density() + 
  labs(x = "Percentage of a company's employees\nparticipating in its 401K plan", 
       y = "Density", 
       title = "Approximate density plot of\nparticipation rate (%) in 401K plans\n(incl. anomalous observations)")
figCount <- incCount(figCount, "density1-Q1")

## @knitr Question1-4
# Plots: density
ggplot(data = data[data$prate <= 100, ], aes(prate)) + 
  geom_density() + 
  labs(x = "Percentage of a company's employees\nparticipating in its 401K plan", 
       y = "Density", 
       title = "Approximate density plot of\nparticipation rate (%) in 401K plans")
figCount <- incCount(figCount, "density2-Q1")


## @knitr Question2-1
# QUESTION 2 --------------------------------------------------------------
# Examine the mrate variable and comment on the shape of its distribution
# First, discard anomalous observations of prate
data2 <- data[data$prate <= 100, ]
# Descriptive statistics of prate
summary(data2$mrate)
round(stat.desc(data2$mrate, desc = TRUE, basic = TRUE, norm = TRUE), 2)
round(quantile(data2$mrate, probs = c(1, 5, 10, 25, 50, 75, 90, 95, 99, 
                                      100)/100), 1)

## @knitr Question2-2
# Plots: histogram
bin_width = 0.1
ggplot(data = data, aes(mrate)) + 
  geom_histogram(aes(y = (..count..)/sum(..count..)), colour = 'black', 
                 fill = 'white', binwidth = bin_width) + 
  labs(x = "Percentage of a company's employees\nparticipating in its 401K plan", 
       y = "Relative frequency", 
       title = "Histogram of companies' match rate (%)\nto their employees' 401K contributions")
figCount <- incCount(figCount, "hist-Q2")

## @knitr Question2-3
# Plots: density
ggplot(data = data2, aes(mrate)) + 
  geom_density() + 
  labs(x = "Percentage of a company's employees\nparticipating in its 401K plan", 
       y = "Density", 
       title = "Approximate density plot of companies'\nmatch rate (%) to their employees'\n401K contributions")
figCount <- incCount(figCount, "density-Q2")



## @knitr Question3-1
# QUESTION 3 --------------------------------------------------------------
# Scatterplot of prate against mrate and linear regression of former on latter
ggplot(data = data2, aes(mrate, prate)) + 
  geom_point() + 
  labs(x = "Company match rate (%) to their\nemployees' contribution to 401K plans", 
       y = "Employees' participation rate (%)\nto 401K plans", 
       title = "Employee participation rate in 401K plan\nagainst company match rate") + 
  geom_smooth(method = "lm")
figCount <- incCount(figCount, "scatter-Q3")

## @knitr Question3-2
params <- "mrate" # regressor()
model <- lm(as.formula(paste("prate", paste(params, sep = "", 
                                            collapse = " + "), sep = " ~ ")), 
            data = data2)
summary(model)
# Create the table with the given parameters (using function in 1st section)
table <- create_regtable(model, params, c("Company match rate (%)"), 
                         "Employees' participation rate (%) to 401K plans")
# Print the table
kable(table, align = "r", 
      caption = paste("Effect of a company match rate to 401K plans", 
                      "on its employees'contribution", sep = " "))
tableCount <- incCount(tableCount, "table-Q3")


## @knitr Question4-1
# QUESTION 4 --------------------------------------------------------------
# Assumption of zero-conditional mean: E[u|x] = 0
df_aux <- data.frame(x = data2$mrate, fitted = model$fitted.values, 
                     residuals = model$residuals)
# Plot Residuals vs. Fitted Values
# Using ggfortify::autoplot (makes use of ggplot2)
autoplot(model, which = 1)
# The same using plot {graphics}
# plot(model, which = 1, main = "Residuals vs. Fitted Values", sub = "", 
#     caption = "")
# Using ggplot2 is worse: not clear which method to use to plot the smoother
# ggplot(data = df_aux, aes(fitted, residuals)) + 
#   geom_point() + 
#   labs(x = "Fitted values of the regressand", 
#        y = "Residuals", 
#        title = "Residuals vs. Fitted Values") + 
#   geom_smooth(method = "loess", se = FALSE, colour = "red") + 
#   geom_hline(aes(yintercept = 0), colour = "blue")
figCount <- incCount(figCount, "fitted-residuals-Q4")

## @knitr Question4-2
mean(model$residuals)
cov(model$residuals, data2$mrate)



## @knitr Question5-1
# QUESTION 5 --------------------------------------------------------------
# Assumption of homoskedasticity
# Using autoplot (ggfortify makes autoplot {ggplot2} support lm objects)
autoplot(model, which = 3)
# Using plot {graphics}
# plot(model, which = 3)
# Using ggplot2 (but the smoother is different, not sure which method to choose)
# Using ggplot2# df_aux$std_res <- rstandard(model)
# ggplot(data = df_aux, aes(fitted, sqrt(abs(std_res)))) + 
#   geom_point() + 
#   geom_smooth(method = "loess", se = FALSE, colour = "red") + 
#   geom_hline(aes(yintercept = 0), colour = "blue")
figCount <- incCount(figCount, "scale-location-Q5")

## @knitr Question5-2
bptest(model) # Breusch-Pagan test



## @knitr Question6-1
# QUESTION 6 --------------------------------------------------------------
# Assumption of normality of errors
autoplot(model, which = 2)
# Using just plot
# plot(model, which = 2, main = "Q-Q plot of the Standardized Residuals", 
#      sub = "", caption = "")
# Another way
# qqnorm(rstandard(model))
# abline(a = 0, b = 1, col = "red")
# And a final way with ggplot2 (this one works perfectc :))
df_aux$std_res <- rstandard(model)
# ggplot(data = df_aux, aes(sample = std_res)) + 
#   stat_qq() + geom_abline(intercept=0, slope=1) + 
#   geom_abline(slope = 1, intercept = 0, colour = "red") + 
#   labs(x = "Theoretical Quantiles", 
#        y = "Standardized residuals", 
#        title = "Q-Q plot of the Standardized Residuals")
figCount <- incCount(figCount, "QQplot-Q6")

## @knitr Question6-2
format(stat.desc(model$residuals, desc = TRUE, basic = TRUE, norm = TRUE), 
       digits = 3, drop0trailing = TRUE, scientific = TRUE, trim = TRUE)

## @knitr Question6-3
ggplot(df_aux, aes(residuals)) + geom_density() + 
  labs(x = "Residuals", 
       y = "Density", 
       title = "Density plot of the residuals")
figCount <- incCount(figCount, "density-Q6")

## @knitr Question6-4
shapiro.test(model$residuals)



## @knitr Question7-1
# QUESTION 7 --------------------------------------------------------------
# Standard error of slope coefficient
summary(model)$coefficients[2, 2]

## @knitr Question7-2
coeftest(model, vcovHC(model))[2, 2]
# Create the table with the given parameters (using function in 1st section)
table2 <- create_regtable_RSEs(model, params, c("Company match rate (%)"), 
                               "Employees' participation rate (%) to 401K plans")
# Print the table
kable(table2, align = "r", 
      caption = paste("Effect of a company match rate to 401K plans on its", 
                      "employees'participation rate (using", 
                      "heteroskedasticity-robust SEs)", sep = " "))
tableCount <- incCount(tableCount, "table-Q7")
