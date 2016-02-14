## MIDS W271-4 HW3            ##
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
stargazer2 <- function(model_list, ...) {
  stargazer(model_list, type = 'latex', header = FALSE, table.placement = "h!", 
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
            df = FALSE, omit.stat = "f", no.space = TRUE, ...)
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
# Load the twoyear.RData dataset and describe the basic structure of the data
# Path relative to W271.Rproj, never to be run by the .Rmd (conflict with knitr)
# setwd('HW3/data')
load("twoyear.RData")
desc
str(data)
head(data)
#summary(data)
round(stat.desc(data, desc = TRUE, basic = TRUE), 2)
# kable(round(stat.desc(data[, 1:floor(dim(data)[2]/4)], 
#                       desc = TRUE, basic = TRUE), 2))

## @knitr Question1-2
# Assign each ID to a 500-range
id_range = cut(data$id, breaks = seq(1, (ceiling(max(data$id)/500) + 1)*500, 
                                     by = 500))
# Check unassigned ranges / levels
setdiff(levels(id_range), droplevels(id_range))
# Further (unnecessary) checking
# id_range_count <- data %>% 
#   select(id) %>% 
#   mutate(id_range = cut(data$id, 
#                         breaks = seq(1, (ceiling(max(data$id)/500) + 1)*500, 
#                                      by = 500))) %>% 
#   group_by(ID_range = id_range) %>% 
#   summarise(Count = n())
# data.frame(id_range_count)[125:135, ]
ggplot(data, aes(id)) + 
  geom_histogram(aes(y = (..count..)/sum(..count..)), colour = 'black', 
                 fill = 'white', binwidth = 2000) + 
  scale_y_continuous(labels = percent_format()) + 
  labs(x = "Percentage of ID numbers (in subsets of 2,000)\nin the sample", 
       y = "Relative frequency", 
       title = "Histogram of ID numbers in the sample")
figCount <- incCount(figCount, "hist-Q1")



## @knitr Question2-1
# QUESTION 2 --------------------------------------------------------------
# Interpret the coefficients beta4 and beta8.
# I.e., 'black' and 'exper*black'
# Set of independent variables
params <- c('jc', 'univ', 'exper', 'black', 'hispanic', 'AA', 'BA')
# Include interaction terms
params2 <- c(params, 'exper*black')
# Include dependent variable
var_of_interest <- c('lwage', params)
# (Reminder of) Meaning of each variable
subset(desc, variable %in% var_of_interest)
model1 <- lm(as.formula(paste(var_of_interest[!var_of_interest %in% params], 
                              paste(params2, sep = "", collapse = " + "), 
                              sep = " ~ ")), data = data)

## @knitr Question2-2
stargazer2(list(model1), title = "Regression summary", 
           dep.var.labels = "lwage", digits = 4, digits.extra = 6)
tableCount <- incCount(tableCount, "table-Q2")
# dep.var.caption = ""
# covariate.labels = c("XX", "ZZ", "(Intercept)"), 



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
#texreg(list(lm(speed~dist,data=cars)), float.pos = 'h')

x <- rnorm(100)
z <- rnorm(100)
y <- 2*x+.25*z
model <- lm(y ~ x)
model2 <- lm(y ~ z)

m1<- extract(model, include.rmse=F, include.fstatistic=T)
m2<- extract(model2, include.rmse=F, include.fstatistic=T)
#screenreg(list(m1,m2))

tableCount <- incCount(tableCount, "kk")

texreg2(list(createTexreg2(model), createTexreg2(model2)), 
        reorder.coef = c(2,3,1), caption = "Table caption", 
        custom.model.names = c("uno", "dos"))
tableCount <- incCount(tableCount, "table-Q8")

texreg(list(model, model2), digits = 3, caption = "Table", 
       caption.above = TRUE, bold = 0.05, float.pos = "h!")

## @knitr Question8-2
# stargazer(list(model, model2), title = "test stargzer", 
#           covariate.labels = c("XX", "ZZ"), 
#           dep.var.labels = c("YY"))

stargazer2(list(model, model2), title = "test stargzer", 
           covariate.labels = c("XX", "ZZ", "(Intercept)"), 
           dep.var.caption = "", dep.var.labels = "YY")
           
           
           
