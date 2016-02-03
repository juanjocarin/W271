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
# Load the 401k contributions dataset
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
  labs(x = "Percentage of a company's employees\nparticipating in its 401k plan", 
       y = "Relative frequency", 
       title = "Histogram of participation rate (%) in 401k plans")
figCount <- incCount(figCount, "hist-Q1")


## @knitr Question1-3
# Plots: density
ggplot(data = data, aes(prate)) + 
  geom_density() + 
  labs(x = "Percentage of a company's employees\nparticipating in its 401k plan", 
       y = "Density", 
       title = "Approximate density plot of\nparticipation rate (%) in 401k plans\n(incl. anomalous observations)")
figCount <- incCount(figCount, "density1-Q1")

## @knitr Question1-4
# Plots: density
ggplot(data = data[data$prate <= 100, ], aes(prate)) + 
  geom_density() + 
  labs(x = "Percentage of a company's employees\nparticipating in its 401k plan", 
       y = "Density", 
       title = "Approximate density plot of\nparticipation rate (%) in 401k plans")
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
  labs(x = "Percentage of a company's employees\nparticipating in its 401k plan", 
       y = "Relative frequency", 
       title = "Histogram of companies' match rate (%)\nto their employees' 401k contributions")
figCount <- incCount(figCount, "hist-Q2")



## @knitr Question2-3
# Plots: density
ggplot(data = data2, aes(mrate)) + 
  geom_density() + 
  labs(x = "Percentage of a company's employees\nparticipating in its 401k plan", 
       y = "Density", 
       title = "Approximate density plot of companies'\nmatch rate (%) to their employees'\n401k contributions")
figCount <- incCount(figCount, "density-Q2")



## @knitr Question3-1
# QUESTION 3 --------------------------------------------------------------
# Scatterplot of prate against mrate and linear regression of former on latter
ggplot(data = data2, aes(mrate, prate)) + 
  geom_point() + 
  labs(x = "Company match rate (%) to their\nemployees' contribution to 401k plans", 
       y = "Employees' participation rate (%)\nto 401k plans", 
       title = "Employees' participation rate to 401k plans\nagainst their company's match rate") + 
  geom_smooth(method = "lm")
figCount <- incCount(figCount, "scatter-Q3")

## @knitr Question3-2
params <- "mrate" # regressor()
# Excluding bwght == 0 (possible missing observations)
model1 <- lm(as.formula(paste("prate", paste(params, sep = "", 
                                             collapse = " + "), sep = " ~ ")), 
             data = data2)
summary(model1)
# Create the table with the given parameters (using function in 1st section)
table <- create_regtable(model1, data2, params, 
                         c("Company match rate (%)"), 
                         "Employees' participation rate (%) to 401k plans")
# Print the table
kable(table, align = "r", 
      caption = paste("Effect of a company match rate to 401k plans", 
                      "\non its employees'contribution", sep = ""))
tableCount <- incCount(tableCount, "table-Q3")


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
