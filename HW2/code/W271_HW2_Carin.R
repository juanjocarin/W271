## MIDS W271-4 HW2            ##
## Carin, Davis, Levato, Song ##


## @knitr Libraries-Functions-Constants
# LIBRARIES, FUNCTIONS AND CONSTANTS -------------------------------------------------
# Load libraries
library(ggplot2)
library(knitr)
library(car)
# Define functions
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
# Define constants used in multiple chunks



## @knitr Load_Data
# LOAD DATA --------------------------------------------------------------
# Load the 401k contributions dataset
load("401k_w271.Rdata")



## @knitr Question1
# QUESTION 1 --------------------------------------------------------------
# ...



## @knitr Question2
# QUESTION 2 --------------------------------------------------------------
# ...



## @knitr Question3
# QUESTION 3 --------------------------------------------------------------
# ...



## @knitr Question4
# QUESTION 4 --------------------------------------------------------------
# ...



## @knitr Question5
# QUESTION 5 --------------------------------------------------------------
# ...



## @knitr Question6
# QUESTION 6 --------------------------------------------------------------
# ...



## @knitr Question7
# QUESTION 7 --------------------------------------------------------------
# ...



## @knitr Question8
# QUESTION 8 --------------------------------------------------------------
# ...
