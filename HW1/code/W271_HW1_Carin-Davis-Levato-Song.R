## MIDS W271-4 HW1            ##
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
# Define constants used in multiple chunks



## @knitr Question1
# QUESTION 1 --------------------------------------------------------------
# Load the birthweight dataset
load("birthweight_w271.rdata")



## @knitr Question2
# QUESTION 2 --------------------------------------------------------------
# Examine the basic structure of the data set using desc, str, and summary
desc
str(data)
summary(data)



## @knitr Question3-1
# QUESTION 3 --------------------------------------------------------------
# Summarize the variable bwght: summary(data$bwght)
summary(data$bwght)

## @knitr Question3-2
# List the following quantiles: 1%, 5%, 10%, 25%, 50%, 75%, 90%, 95%, 99%**
quantile(data$bwght, probs = c(1, 5, 10, 25, 50, 75, 90, 95, 99)/100)

## @knitr Question3-3-1
# Plot the histogram of bwght and comment on the shape of its distribution
# Use hist and bin width = 5
bin_width = 5
hist(data$bwght, breaks = seq(floor(min(data$bwght)/bin_width)*bin_width, 
                              ceiling(max(data$bwght)/bin_width)*bin_width, 
                              by = bin_width), 
     xlab = "Birth weight (ounces)", ylab = "Count", 
     main = "Histogram of birth weight")

## @knitr Question3-3-2
# Use ggplot and bin width = 5
ggplot(data = data, aes(bwght)) + 
  geom_histogram(colour = 'black', fill = 'white', 
                 binwidth = bin_width) +  
  labs(x = "Birth weight (ounces)", y = "Count", 
       title = "Histogram of birth weight")

# With density plot
# ggplot(data = data, aes(bwght)) + 
#   geom_histogram(aes(y = ..density..), colour = 'black', fill = 'white', 
#                  binwidth = bin_width) +  
#   labs(x = "Birth weight (ounces)", y = "Density", 
#        title = "Histogram of birth weight") + 
#   stat_function(fun = dnorm, args = list(mean = mean(data$bwght, 
#                                                      na.rm = TRUE), 
#                                          sd = sd(data$bwght, na.rm = TRUE)), 
#                 colour = 'black', size = 1)

## @knitr Question3-3-3
# Use ggplot and bin width = 10
bin_width = 10
ggplot(data = data, aes(bwght)) + 
  geom_histogram(colour = 'black', fill = 'white', 
                 binwidth = bin_width) +  
  labs(x = "Birth weight (ounces)", y = "Count", 
       title = "Histogram of birth weight")

## @knitr Question3-3-4
# Use ggplot and bin width = 20
bin_width = 20
ggplot(data = data, aes(bwght)) + 
  geom_histogram(colour = 'black', fill = 'white', 
                 binwidth = bin_width) +  
  labs(x = "Birth weight (ounces)", y = "Count", 
       title = "Histogram of birth weight")



## @knitr Question4-1
# QUESTION 4 --------------------------------------------------------------
# Summarize the variable cigs: summary(data$cigs)
summary(data$cigs)

## @knitr Question4-2
# List the following quantiles: 1%, 5%, 10%, 25%, 50%, 75%, 90%, 95%, 99%**
quantile(data$cigs, probs = c(1, 5, 10, 25, 50, 75, 90, 95, 99)/100)

## @knitr Question4-3-1
# Plot the histogram of bwght and comment on the shape of its distribution
# Use hist and bin width = 1
bin_width = 1
hist(data$cigs, breaks = seq(floor(min(data$cigs)/bin_width)*bin_width, 
                             ceiling(max(data$cigs)/bin_width)*bin_width, 
                             by = bin_width), 
     xlab = "Cigarettes smoked each day\nby the mother while pregnant", 
     ylab = "Count", 
     main = "Histogram of cigarettes smoked each day\nby the mother while pregnant")

## @knitr Question4-3-2
# Use ggplot and bin width = 1
bin_width = 1
ggplot(data = data, aes(cigs)) + 
  geom_histogram(colour = 'black', fill = 'white', 
                 binwidth = bin_width) +  
  labs(x = "Cigarettes smoked each day\nby the mother while pregnant", 
       y = "Count", 
       title = "Histogram of cigarettes smoked each day\nby the mother while pregnant")

## @knitr Question4-3-3
# Use ggplot and bin width = 5
bin_width = 5
ggplot(data = data, aes(cigs)) + 
  geom_histogram(colour = 'black', fill = 'white', 
                 binwidth = bin_width) +  
  labs(x = "Cigarettes smoked each day\nby the mother while pregnant", 
       y = "Count", 
       title = "Histogram of cigarettes smoked each day\nby the mother while pregnant")

## @knitr Question4-3-4
# Use ggplot and bin width = 10
bin_width = 10
ggplot(data = data, aes(cigs)) + 
  geom_histogram(colour = 'black', fill = 'white', 
                 binwidth = bin_width) +  
  labs(x = "Cigarettes smoked each day\nby the mother while pregnant", 
       y = "Count", 
       title = "Histogram of cigarettes smoked each day\nby the mother while pregnant")

## @knitr Question4-3-5
# Use ggplot and bin width = 5, smokers only
bin_width = 5
ggplot(data = data[data$cigs != 0, ], aes(cigs)) + 
  geom_histogram(colour = 'black', fill = 'white', 
                 binwidth = bin_width) +  
  labs(x = "Cigarettes smoked each day\nby the mother while pregnant", 
       y = "Count", 
       title = "Histogram of cigarettes smoked each day\nby the mother while pregnant. Smokers only")

## @knitr Question4-3-6
# Use ggplot and bin width = 0.5, smokers only and log(cigs)
bin_width = 0.5
ggplot(data = data[data$cigs != 0, ], aes(log(cigs))) + 
  geom_histogram(colour = 'black', fill = 'white', 
                 binwidth = bin_width) +  
  labs(x = "Log of Cigarettes smoked each day\nby the mother while pregnant", 
       y = "Count", 
       title = "Histogram of log of cigarettes smoked each day\nby the mother while pregnant. Smokers only")



## @knitr Question5
# QUESTION 5 --------------------------------------------------------------
# Generate a scatterplot of `bwght` against `cigs`
ggplot(data = data, aes(cigs, bwght)) + 
  geom_point() + 
  labs(x = "Cigarettes smoked each day by the mother while pregnant", 
       y = "Birth weight (ounces)", 
       title = "Cigarettes smoked by the mother\nagainst birth weight") + 
  geom_smooth(method = "lm")
  


## @knitr Question6
# QUESTION 6 --------------------------------------------------------------
# Regressor
params <- "cigs"
# Excluding bwght == 0 (possible missing observations)
model <- lm(as.formula(paste("bwght", paste(params, sep = "", 
                                            collapse = " + "), sep = " ~ ")), 
            data = data[data$bwght !=0, ])
summary(model)
# Create the table with the given parameters (using function in 1st section)
table <- create_regtable(model, params, 
                         c("Cigarettes smoked each day by the mother"), 
                         "Birth weight (ounces)")
# Print the table
kable(table, align = "r", 
      caption = paste("Effect of the number of cigarettes smoked each day", 
                      "\nby the mother while pregnant on the birth weight", 
                      sep = ""))



## @knitr Question7-1
# QUESTION 7 --------------------------------------------------------------
# Summarize the variable faminc
summary(data$faminc)

## @knitr Question7-2
# List the following quantiles: 1%, 5%, 10%, 25%, 50%, 75%, 90%, 95%, 99%**
quantile(data$faminc, probs = c(1, 5, 10, 25, 50, 75, 90, 95, 99)/100)

## @knitr Question7-3-2
# Plot the histogram of bwght and comment on the shape of its distribution
# Use ggplot and bin width = 2
bin_width = 2
ggplot(data = data, aes(faminc)) + 
  geom_histogram(colour = 'black', fill = 'white', 
                 binwidth = bin_width) +  
  labs(x = "Family income (thousands of dollars)", y = "Count", 
       title = "Histogram of family income")


## @knitr Question7-3-3
# Use ggplot and bin width = 5
bin_width = 5
ggplot(data = data, aes(faminc)) + 
  geom_histogram(colour = 'black', fill = 'white', 
                 binwidth = bin_width) +  
  labs(x = "Family income (thousands of dollars)", y = "Count", 
       title = "Histogram of family income")

## @knitr Question7-3-4
# Use ggplot and bin width = 10
bin_width = 10
ggplot(data = data, aes(faminc)) + 
  geom_histogram(colour = 'black', fill = 'white', 
                 binwidth = bin_width) +  
  labs(x = "Family income (thousands of dollars)", y = "Count", 
       title = "Histogram of family income")

## @knitr Question7-4
# Generate a scatterplot of `bwght` against `faminc`
ggplot(data = data, aes(faminc, bwght)) + 
  geom_point() + 
  labs(x = "Family income (thousands of dollars)", 
       y = "Birth weight (ounces)", 
       title = "Family income against birth weight") + 
  geom_smooth(method = "lm")

## @knitr Question7-5
# scatterplot.matrix has been deprecated, we used the new function instead
scatterplotMatrix(~ bwght + cigs + faminc, data[data$bwght !=0, ])


## @knitr Question8
# QUESTION 8 --------------------------------------------------------------
# New regressors
params <- c("cigs", "faminc")
model2 <- lm(as.formula(paste("bwght", paste(params, sep = "", 
                                             collapse = " + "), sep = " ~ ")), 
             data = data[data$bwght !=0, ])
summary(model2)

# Create summary table using function defined in QUESTION 7
table <- create_regtable(model2, params, 
                         c("Cigarettes smoked each day by the mother", 
                           "Family income (thousands of dollars)"), 
                         "Birth weight (ounces)")
kable(table, align = "r", 
      caption = paste("Effect of the number of cigarettes smoked each day", 
                      "\nby the mother while pregnant and the family income\n", 
                      "on the birth weight", sep = ""))



## @knitr Question10
# QUESTION 10 --------------------------------------------------------------
# Demonstration of the differences in the value of one coefficient
# when another variable is included or omitted
tilde_delta_1 <- summary(lm(faminc ~ cigs, 
                            data = data[data$bwght != 0, ]))$coefficients[2, 1]
hat_beta_1 <- summary(lm(bwght ~ cigs + faminc, 
                         data = data[data$bwght != 0, ]))$coefficients[2, 1]
hat_beta_2 <- summary(lm(bwght ~ cigs + faminc, 
                         data = data[data$bwght != 0, ]))$coefficients[3, 1]
hat_beta_1 + hat_beta_2 * tilde_delta_1
(tilde_beta_1 <- summary(lm(bwght ~ cigs, 
                            data = data[data$bwght != 0, ]))$coefficients[2, 1])
