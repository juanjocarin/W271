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
params_plus_interaction <- c(params, 'exper*black')
# Include dependent variable
vars_of_interest <- c('lwage', params)
# (Reminder of) Meaning of each variable
subset(desc, variable %in% vars_of_interest)
# Summary of variables of interest only
summary(data[, vars_of_interest])
# OLS model
model1 <- lm(as.formula(paste(vars_of_interest[!vars_of_interest %in% params], 
                              paste(params_plus_interaction, sep = "", 
                                    collapse = " + "), sep = " ~ ")), 
             data = data)

## @knitr Question2-2
stargazer2(model1, title = "Regression summary", digits = 4, digits.extra = 6, 
           dep.var.labels = 'lwage', order = c(1:4, 8, 5:7, 9), 
           covariate.labels = c("Junior college (2-yr credits)", 
                                "University (4-yr credits)", 
                                "Work experience (months)", 
                                "Black", "Experience * Black", "Hispanic", 
                                "Associate's degree", "Bachelor's degree", 
                                "Intercept (Constant)"))
tableCount <- incCount(tableCount, "table-Q2")

## @knitr Question2-3
autoplot(model1)
figCount <- incCount(figCount, "regression-Q2")


## @knitr Question2-4
# Center exper around its mean
data2 <- data[, vars_of_interest]
data2$exper_mean <- data2$exper - mean(data2$exper)
params2 <- gsub('exper', 'exper_mean', params)
params_plus_interaction2 <- gsub('exper', 'exper_mean', 
                                 params_plus_interaction)
vars_of_interest2 <- c('lwage', params2)
model2 <- lm(as.formula(paste(vars_of_interest2[!vars_of_interest2 %in% 
                                                  params2], 
                              paste(params_plus_interaction2, sep = "", 
                                    collapse = " + "), sep = " ~ ")), 
             data = data2)

## @knitr Question2-5
stargazer2(list(model1, model2),
           title = paste0("Regression summary using 0 and its mean (",
                          frmt(mean(data$exper), 1),
                          ") as the baselines values of exper"),
           digits = 4, digits.extra = 6, dep.var.labels = rep('lwage', 2), 
           order = c(1:5, 9:10, 6:8, 11), 
           covariate.labels = c("Junior college (2-yr credits)",
                                "University (4-yr credits)",
                                "Work experience (months)",
                                paste("Work experience (months) with",
                                      "respect to mean (122.4)"),
                                "Black", "Experience * Black",
                                "Experience with respect to mean * Black", 
                                "Hispanic", "Associate's degree",
                                "Bachelor's degree", "Intercept (Constant)"))
tableCount <- incCount(tableCount, "table-Q2-2")



## @knitr Question3-1
# QUESTION 3 --------------------------------------------------------------
# Test that the return to university education is 7%.
# New t statististic
(t <- (coeftest(model1, vcovHC(model1))[2+1, 1] - .07) / 
   coeftest(model1, vcovHC(model1))[2+1, 2])
(t_exact <- (coeftest(model1, vcovHC(model1))[2+1, 1] - log(0.07 + 1)) / 
  coeftest(model1, vcovHC(model1))[2+1, 2])
# New p value
2*pt(t, dim(data)[1] - 1, lower.tail = FALSE)
2*pt(t_exact, dim(data)[1] - 1, lower.tail = FALSE)

## @knitr Question3-2
data3 <- data[, vars_of_interest]
data3$lwage_minus7univ <- data3$lwage - .07 * data3$univ
vars_of_interest3 <- c('lwage_minus7univ', params)
model3 <- lm(as.formula(paste(vars_of_interest3[!vars_of_interest3 %in% 
                                                  params], 
                              paste(params_plus_interaction, sep = "", 
                                    collapse = " + "), sep = " ~ ")), 
             data = data3)
coeftest(model3, vcovHC(model3))

data4 <- data[, vars_of_interest]
data4$lwage_minus7univ <- data4$lwage - log(0.07 + 1) * data4$univ
vars_of_interest4 <- c('lwage_minus7univ', params)
model4 <- lm(as.formula(paste(vars_of_interest3[!vars_of_interest4 %in% 
                                                  params], 
                              paste(params_plus_interaction, sep = "", 
                                    collapse = " + "), sep = " ~ ")), 
             data = data4)
coeftest(model4, vcovHC(model4))[3, ]

## @knitr Question3-3
linearHypothesis(model1, c("univ = 0.07"), vcov = vcovHC(model1))$'Pr(>F)'[2]
linearHypothesis(model1, c(paste("univ = ", log(0.07 + 1))), 
                 vcov = vcovHC(model1))$'Pr(>F)'[2]



## @knitr Question4-1
# QUESTION 4 --------------------------------------------------------------
# Test that the return to junior college education is equal for black and 
# non-black.
linearHypothesis(model1, c("black = 0"), vcov = vcovHC(model1))

## @knitr Question4-2
coeftest(model1, vcov = vcovHC(model1))[4+1, 4]

## @knitr Question4-3
data$black_factor <- factor(data$black, labels = c("Non-black", "Black"))
ggplot(data = data, aes(jc, lwage, black_factor)) + 
  geom_point() +
  geom_smooth(method = "lm") + facet_wrap( ~ black_factor) +
  labs(x = "Junior college education (total 2-year credits)",
       y = "log(wage)",
       title = paste0("Scatterplot of junior college education against\n",
                      "log of hourly wage for black and non-black"))
# ggplot(data = data, aes(jc, lwage, colour = black_factor)) +
#   geom_point() +
#   geom_smooth(method = "lm", aes(fill = black_factor)) +
#   labs(x = "Junior college education (total 2-year credits)",
#        y = "log(wage)", fill = "Ethnicity", colour = "Ethnicity",
#        title = paste0("Scatterplot of junior college education against\n",
#                       "log of hourly wage for black and non-black"))
figCount <- incCount(figCount, "scatter-Q4")

## @knitr Question4-4
# New model incl. interaction term
params_plus_interaction5 <- c(params_plus_interaction, 'jc*black')
model5 <- lm(as.formula(paste(vars_of_interest[!vars_of_interest %in% params], 
                              paste(params_plus_interaction5, sep = "", 
                                    collapse = " + "), sep = " ~ ")), 
             data = data)

## @knitr Question4-5
stargazer2(list(model5, model1),
           title = paste0("Regression summary with and without including the ",
                          "interaction term between junior college and being ", 
                          "black"),
           digits = 4, digits.extra = 6, dep.var.labels = rep('lwage', 2), 
           order = c(1, 3, 4, 9, 8, 2, 5:7, 10), 
           covariate.labels = c("Junior college (2-yr credits)", 
                                "Work experience (months)", "Black", 
                                "Junior college * Black", 
                                "Experience * Black", 
                                "University (4-yr credits)", "Hispanic", 
                                "Associate's degree", "Bachelor's degree", 
                                "Intercept (Constant)"))
tableCount <- incCount(tableCount, "table-Q4")

## @knitr Question4-6
linearHypothesis(model5, c("black = 0", "jc:black = 0"), vcov = vcovHC)



## @knitr Question5-1
# QUESTION 5 --------------------------------------------------------------
# Test whether the return to university education is equal to the return to 
# 1 year of working experience.
linearHypothesis(model1, c("univ = 12*exper"), vcov = vcovHC)

## @knitr Question5-2
model6 <- lm(lwage ~ jc + I(12*univ + exper) + univ + black + hispanic + AA + 
               BA + exper * black, data)
coeftest(model6, vcovHC(model6))[1:4, ]

## @knitr Question5-3
linearHypothesis(model1, c("jc = 12*exper"), vcov = vcovHC)
linearHypothesis(model1, c("univ = jc"), vcov = vcovHC)



## @knitr Question6
# QUESTION 6 --------------------------------------------------------------
# Test the overall significance of this regression.
# Two ways of testing overal significance
waldtest(model1, vcov = vcovHC)
linearHypothesis(model1, names(coef(model1))[-1], vcov = vcovHC)



## @knitr Question7-1
# QUESTION 7 --------------------------------------------------------------
# Including a square term of working experience to the regression model,
# estimate the linear regression model again.
# What is the estimated return to work experience in this model?
params_plus_interaction_square <- c(params_plus_interaction, 'I(exper^2)')
model7 <- lm(as.formula(paste(vars_of_interest[!vars_of_interest %in% params], 
                              paste(params_plus_interaction_square, sep = "", 
                                    collapse = " + "), sep = " ~ ")), 
             data = data)
# coeftest(model7, vcov = vcovHC(model7))
100 * (coeftest(model7, vcov = vcovHC(model7))[3+1, 1] + 
         2*coeftest(model7, vcov = vcovHC(model7))[8+1, 1] * 
         mean(data$exper)) * 12
100 * coeftest(model1, vcov = vcovHC(model1))[3+1, 1] * 12


## @knitr Question7-2
stargazer2(list(model7, model1),
           title = paste0("Regression summary with and without including the ",
                          "square term of working experience"),
           digits = 4, digits.extra = 6, dep.var.labels = rep('lwage', 2), 
           order = c(1:3, 8, 4, 9, 5:7, 10), 
           covariate.labels = c("Junior college (2-yr credits)",
                                "University (4-yr credits)",
                                "Work experience (months)", 
                                "Work experience$^2$", "Black", 
                                "Experience * Black", "Hispanic", 
                                "Associate's degree", "Bachelor's degree", 
                                "Intercept (Constant)"))
tableCount <- incCount(tableCount, "table-Q7")



## @knitr Question8-1
# QUESTION 8 --------------------------------------------------------------
# Provide the diagnosis of the homoskedasticity assumption.
# Does this assumption hold?
# If so, how does it affect the testing of no effect of university education on
# salary change?
# If not, what potential remedies are available?
# Breusch-Pagan test
bptest(model1)
ncvTest(model1)

## @knitr Question8-2
autoplot(model1, which = 1)
figCount <- incCount(figCount, "residuals-fitted-Q8")

## @knitr Question8-3
df <- data.frame(x = data$univ, y = model1$residuals)
ggplot(df, aes(x, y)) + 
  geom_point() + geom_smooth() + 
  labs(x = "University (total 4-year credits)", y = "Residuals", 
       title = "Residuals vs. University")
figCount <- incCount(figCount, "residuals-univ-Q8")

## @knitr Question8-4
autoplot(model1, which = 3)
figCount <- incCount(figCount, "scale-location-Q8")
