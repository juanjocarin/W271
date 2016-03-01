## MIDS W271-4 Lab2           ##
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



## @knitr Question1-1-1
# QUESTION 1 --------------------------------------------------------------
# Find the conditional expectation of Y given X, E(Y|X)

## @knitr Question1-1-2

## @knitr Question1-3

## @knitr Question1-4

## @knitr Question1-5

## @knitr Question2
# QUESTION 2 --------------------------------------------------------------
# Find, the values of a, b, and c that minimize the variance of total payoff



## @knitr Question3
# QUESTION 3 --------------------------------------------------------------


## @knitr Question4
# QUESTION 4 --------------------------------------------------------------

## @knitr Question4-1-1
# Load data and summarize
data <- read.csv("WageData2.csv", header = T)
round(stat.desc(data, desc = TRUE, basic = TRUE), 2)

## @knitr Question4-1-2
ggplot(data, aes(wage)) + 
  geom_histogram(aes(y = (..count..)/sum(..count..)), colour = 'black',
                 fill = 'white', binwidth = 100) +
  scale_y_continuous(labels = percent_format()) + 
  labs(x = "Wage",
       y = "Relative frequency",
       title = "Histogram of Wage")
figCount <- incCount(figCount, "hist-Q4-1-1")

## @knitr Question4-1-3
ggplot(data, aes(log(wage))) + 
  geom_histogram(aes(y = (..count..)/sum(..count..)), colour = 'black',
                 fill = 'white', binwidth = .1) +
  scale_y_continuous(labels = percent_format()) + 
  labs(x = "Log(Wage)",
       y = "Relative frequency",
       title = "Histogram of Log(Wage)")
figCount <- incCount(figCount, "hist-Q4-1-2")

## @knitr Question4-1-4
ggplot(data, aes(education)) + 
  geom_histogram(aes(y = (..count..)/sum(..count..)), colour = 'black',
                 fill = 'white', binwidth = 1) +
  scale_y_continuous(labels = percent_format()) + 
  labs(x = "Education in Years",
       y = "Relative frequency",
       title = "Histogram of Education")
figCount <- incCount(figCount, "hist-Q4-1-3")

## @knitr Question4-1-5
ggplot(data, aes(experience)) + 
  geom_histogram(aes(y = (..count..)/sum(..count..)), colour = 'black',
                 fill = 'white', binwidth = 1) +
  scale_y_continuous(labels = percent_format()) + 
  labs(x = "Experience",
       y = "Relative frequency",
       title = "Histogram of Experience")
figCount <- incCount(figCount, "hist-Q4-1-4")

## @knitr Question4-1-6
ggplot(data, aes(experience^2)) + 
  geom_histogram(aes(y = (..count..)/sum(..count..)), colour = 'black',
                 fill = 'white', binwidth = 25) +
  scale_y_continuous(labels = percent_format()) + 
  labs(x = "Experience^2",
       y = "Relative frequency",
       title = "Histogram of Experience")
figCount <- incCount(figCount, "hist-Q4-1-5")

## @knitr Question4-1-7
ggplot(data, aes(age)) + 
  geom_histogram(aes(y = (..count..)/sum(..count..)), colour = 'black',
                 fill = 'white', binwidth = 1) +
  scale_y_continuous(labels = percent_format()) + 
  labs(x = "Age",
       y = "Relative frequency",
       title = "Histogram of Age")
figCount <- incCount(figCount, "hist-Q4-1-6")

## @knitr Question4-1-8
ggplot(data, aes(factor(raceColor, labels = c('White',
                                         'Non-White')))) +
  geom_bar(colour='black', fill = 'white') + 
  labs(title = "Race of Respondents", x = element_blank())
figCount <- incCount(figCount, "hist-Q4-1-7")

## @knitr Question4-1-9
ggplot(data, aes(dad_education)) + 
  geom_histogram(aes(y = (..count..)/sum(..count..)), colour = 'black',
                 fill = 'white', na.rm = T, binwidth = 1) +
  scale_y_continuous(labels = percent_format()) + 
  labs(x = "Father's Education in Years",
       y = "Relative frequency",
       title = "Histogram of Father's Education")
figCount <- incCount(figCount, "hist-Q4-1-8")

## @knitr Question4-1-10
ggplot(data, aes(mom_education)) + 
  geom_histogram(aes(y = (..count..)/sum(..count..)), colour = 'black',
                 fill = 'white', na.rm = T, binwidth = 1) +
  scale_y_continuous(labels = percent_format()) + 
  labs(x = "Mother's Education in Years",
       y = "Relative frequency",
       title = "Histogram of Mother's Education")
figCount <- incCount(figCount, "hist-Q4-1-9")

## @knitr Question4-1-11
ggplot(data, aes(factor(rural, labels = c('Non-rural',
                                              'Rural')))) +
  geom_bar(colour='black', fill = 'white') + 
  labs(title = "Respondant Location - Rural", x = element_blank())
figCount <- incCount(figCount, "hist-Q4-1-10")

## @knitr Question4-1-12
ggplot(data, aes(factor(city, labels = c('Non-Urban',
                                          'City')))) +
  geom_bar(colour='black', fill = 'white') + 
  labs(title = "Respondant Location - City", x = element_blank())
figCount <- incCount(figCount, "hist-Q4-1-11")

## @knitr Question4-1-13
ggplot(data, aes(factor(z1, labels = c('1',
                                         '0')))) +
  geom_bar(colour='black', fill = 'white') + 
  labs(title = "Indicator Variable 1", x = element_blank())
figCount <- incCount(figCount, "hist-Q4-1-12")

## @knitr Question4-1-14
ggplot(data, aes(factor(z2, labels = c('1',
                                       '0')))) +
  geom_bar(colour='black', fill = 'white') + 
  labs(title = "Indicator Variable 2", x = element_blank())
figCount <- incCount(figCount, "hist-Q4-1-13")

## @knitr Question4-1-15
ggplot(data, aes(IQscore)) + 
  geom_histogram(aes(y = (..count..)/sum(..count..)), colour = 'black',
                 fill = 'white', na.rm = T, binwidth = 5) +
  scale_y_continuous(labels = percent_format()) + 
  labs(x = "IQ Score",
       y = "Relative frequency",
       title = "Histogram of IQ Score")
figCount <- incCount(figCount, "hist-Q4-1-14")

data$experienceSquare <- data$experience^2

## @knitr Question4-2-1
cor(data$wage, data, use="pairwise.complete.obs")
cor(data$logWage, data, use="pairwise.complete.obs")

## @knitr Question4-2-2
ggplot(data, aes(wage, education)) + 
  geom_point() +
  labs(x = "Wage", 
       y = "Education", 
       title = "Scatterplot of Wage Against Education") + 
  geom_smooth(method = "lm")
figCount <- incCount(figCount, "scatter-Q4-2-1")

## @knitr Question4-2-3
ggplot(data, aes(log(wage), education)) + 
  geom_point() +
  labs(x = "Log(Wage)", 
       y = "Education", 
       title = "Scatterplot of Log(Wage) Against Education") + 
  geom_smooth(method = "lm")
figCount <- incCount(figCount, "scatter-Q4-2-2")

## @knitr Question4-2-4
ggplot(data, aes(wage, experience)) + 
  geom_point() +
  labs(x = "Wage", 
       y = "Experience", 
       title = "Scatterplot of Wage Against Experience") + 
  geom_smooth(method = "lm")
figCount <- incCount(figCount, "scatter-Q4-2-3")

## @knitr Question4-2-5
ggplot(data, aes(log(wage), experience)) + 
  geom_point() +
  labs(x = "Log(Wage)", 
       y = "Experience", 
       title = "Scatterplot of Log(Wage) Against Experience") + 
  geom_smooth(method = "lm")
figCount <- incCount(figCount, "scatter-Q4-2-4")

## @knitr Question4-2-6
ggplot(data, aes(wage, age)) + 
  geom_point() +
  labs(x = "Wage", 
       y = "Age", 
       title = "Scatterplot of Wage Against Age") + 
  geom_smooth(method = "lm")
figCount <- incCount(figCount, "scatter-Q4-2-5")

## @knitr Question4-2-7
ggplot(data, aes(log(wage), age)) + 
  geom_point() +
  labs(x = "Log(Wage)", 
       y = "Age", 
       title = "Scatterplot of Log(Wage) Against Age") + 
  geom_smooth(method = "lm")
figCount <- incCount(figCount, "scatter-Q4-2-6")

## @knitr Question4-2-8
ggplot(data, aes(wage, raceColor)) + 
  geom_point() +
  labs(x = "Wage", 
       y = "Race", 
       title = "Scatterplot of Wage Against Race") + 
  geom_smooth(method = "lm")
figCount <- incCount(figCount, "scatter-Q4-2-7")

## @knitr Question4-2-9
ggplot(data, aes(log(wage), raceColor)) + 
  geom_point() +
  labs(x = "Log(Wage)", 
       y = "Race", 
       title = "Scatterplot of Log(Wage) Against Race") + 
  geom_smooth(method = "lm")
figCount <- incCount(figCount, "scatter-Q4-2-8")

## @knitr Question4-2-10
ggplot(data, aes(wage, dad_education)) + 
  geom_point(na.rm = T) +
  labs(x = "Wage", 
       y = "Father's Education", 
       title = "Scatterplot of Wage Against Father's Edication") + 
  geom_smooth(method = "lm", na.rm = T)
figCount <- incCount(figCount, "scatter-Q4-2-9")

## @knitr Question4-2-11
ggplot(data, aes(log(wage), dad_education)) + 
  geom_point(na.rm = T) +
  labs(x = "Log(Wage)", 
       y = "Father's Education", 
       title = "Scatterplot of Log(Wage) Against Father's Edication") + 
  geom_smooth(method = "lm", na.rm = T)
figCount <- incCount(figCount, "scatter-Q4-2-10")

## @knitr Question4-2-12
ggplot(data, aes(wage, mom_education)) + 
  geom_point(na.rm = T) +
  labs(x = "Wage", 
       y = "Mother's Education", 
       title = "Scatterplot of Wage Against Mother's Edication") + 
  geom_smooth(method = "lm", na.rm = T)
figCount <- incCount(figCount, "scatter-Q4-2-11")

## @knitr Question4-2-13
ggplot(data, aes(log(wage), mom_education)) + 
  geom_point(na.rm = T) +
  labs(x = "Log(Wage)", 
       y = "Mother's Education", 
       title = "Scatterplot of Log(Wage) Against Mother's Edication") + 
  geom_smooth(method = "lm", na.rm = T)
figCount <- incCount(figCount, "scatter-Q4-2-12")

## @knitr Question4-2-14
ggplot(data, aes(wage, rural)) + 
  geom_point(na.rm = T) +
  labs(x = "Wage", 
       y = "Location - Rural", 
       title = "Scatterplot of Wage Against Location - Rural") + 
  geom_smooth(method = "lm", na.rm = T)
figCount <- incCount(figCount, "scatter-Q4-2-13")

## @knitr Question4-2-15
ggplot(data, aes(log(wage), rural)) + 
  geom_point(na.rm = T) +
  labs(x = "Log(Wage)", 
       y = "Location - Rural", 
       title = "Scatterplot of Log(Wage) Against Location - Rural") + 
  geom_smooth(method = "lm", na.rm = T)
figCount <- incCount(figCount, "scatter-Q4-2-14")

## @knitr Question4-2-16
ggplot(data, aes(wage, city)) + 
  geom_point(na.rm = T) +
  labs(x = "Wage", 
       y = "Location - City", 
       title = "Scatterplot of Wage Against Location - City") + 
  geom_smooth(method = "lm", na.rm = T)
figCount <- incCount(figCount, "scatter-Q4-2-15")

## @knitr Question4-2-17
ggplot(data, aes(log(wage), city)) + 
  geom_point(na.rm = T) +
  labs(x = "Log(Wage)", 
       y = "Location - City", 
       title = "Scatterplot of Log(Wage) Against Location - City") + 
  geom_smooth(method = "lm", na.rm = T)
figCount <- incCount(figCount, "scatter-Q4-2-16")

## @knitr Question4-2-18
ggplot(data, aes(wage, IQscore)) + 
  geom_point(na.rm = T) +
  labs(x = "Wage", 
       y = "IQ", 
       title = "Scatterplot of Wage Against IQ") + 
  geom_smooth(method = "lm", na.rm = T)
figCount <- incCount(figCount, "scatter-Q4-2-17")

## @knitr Question4-2-19
ggplot(data, aes(log(wage), IQscore)) + 
  geom_point(na.rm = T) +
  labs(x = "Log(Wage)", 
       y = "IQ", 
       title = "Scatterplot of Log(Wage) Against IQ") + 
  geom_smooth(method = "lm", na.rm = T)
figCount <- incCount(figCount, "scatter-Q4-2-18")

## @knitr Question5
# QUESTION 5 --------------------------------------------------------------

## @knitr Question
# QUESTION 6 --------------------------------------------------------------

