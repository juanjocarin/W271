## MIDS W271-4 Lab2           ##
## Carin, Davis, Levato, Song ##


## @knitr Libraries-Functions-Constants
# LIBRARIES, FUNCTIONS AND CONSTANTS -------------------------------------------------
# Load libraries
library(plot3D)
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
library(tidyr)
library(stargazer)
library(texreg)
library(GGally)

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
simulations <- 1e4 # number of simulations
set.seed(123)
x <- runif(simulations, min=0, max=1) # X ~ U(0,1)
y <- runif(simulations, min=0, max=x) # Y|X ~ U(0,X)
par(mfrow = c(1, 2))
hist(x, main = "pdf(x)", freq = FALSE)
lines(density(x), col = 'red')
hist(y, main = "pdf(y)", freq = FALSE)
lines(density(y), col = 'red')

## @knitr Question1-1-2
# y1 <- runif(simulations, min = 0, max = 0.2) # Fix X to 0.2
y1 <- y[x > 0.2 - 1e-2 & x < 0.2 + 1e-2] # Using previous simulation
hist(y1, main = 'pdf(y|x=0.2)', freq = FALSE)
lines(density(y1), xlim = c(0, 1), main = 'pdf(y|x=0.2)', col = 'red')
abline(v = mean(y1), col = 'green', lty = 2, lwd = 4)
# legend("topright", "E(Y|X=0.2)", lty = 1, bty="n", col = 'red')

## @knitr Question1-3
pdf_x <- function(x) ifelse(x<1 & x>0, 1, 0) # f(x)
integrate(pdf_x, -Inf, Inf) # integral
pdf_y_given_x <- function(x,y) ifelse(y<x & y>0 & x<1 & x>0, 1/x, 0) # f(y|x)
pdf_xy <- function(x,y) pdf_x(x)*pdf_y_given_x(x,y) # f(x,y)
# integral
integrate(function(y) sapply(y, function(y) integrate(function(x) 
  pdf_xy(x,y), 0, 1)$value), -Inf, Inf)
# Plot f(x,y)
x0 <- y0 <- seq(0, 1, by = 0.01)
grid <- mesh(x0, y0)
z0 <- with(grid, pdf_x(x)*pdf_y_given_x(x,y))
# contour(x0, y0, z0, asp=1)
# par(mfrow = c(1, 2))
persp3D(z = z0, x = x0, y = y0)
# Confirm that f(x,y) = 1/x
# z2 <- with(grid, ifelse(x<=y | x==0 | y == 0, 0, 1/x))
# persp3D(z = z2, x = x0, y = y0)

## @knitr Question1-4
# f(y)
pdf_y <- function(y) 
  sapply(y, function(y) integrate(function(x) 
    pdf_y_given_x(x,y)*pdf_x(x), 0, 1)$value)
integrate(pdf_y, -Inf, Inf) # integral
plot(sort(y), pdf_y(sort(y)), type = 'l', main = 'pdf(y)', xlab = 'y')
# Confirm that f(y) = log(1/y)
lines(sort(y), log(1/sort(y)), type = 'l', main = 'pdf(y)')


## @knitr Question1-5
# Confirm E(X|Y=0.5) (use values of Y around 0.5 in the previous simulation)
mean(x[y > 0.5 - 1e-2 & y < 0.5 + 1e-2])
1/(2*log(2))



## @knitr Question2
# QUESTION 2 --------------------------------------------------------------
# Find, the values of a, b, and c that minimize the variance of total payoff
payoff <- function(x) {
  a <- x[1]
  b <- x[2]
  c <- x[3]
  a^2 + b^2/2 + c^2/3
}
gradient_payoff <- function(x) {
  a <- x[1]
  b <- x[2]
  c <- x[3]
  c(2*a, b, 2*c/3)
}
sol <- constrOptim(theta = c(.3, .3, .4), f = payoff, grad = gradient_payoff, 
                   ui = rbind(c(1, 0, 0), c(0, 1, 0), c(0, 0, 1), 
                              c(-1, 0, 0), c(0, -1, 0), c(0, 0, -1), 
                              c(1, 1, 1), c(-1, -1, -1)), 
                   ci = c(0, 0, 0, -1, -1, -1, 1-1e-6, -1-1e-6))
sol$par



## @knitr Question3
# QUESTION 3 --------------------------------------------------------------
simulations <- 1e3 # number of simulations
theta <- 100 # an arbitrary value of theta
y <- runif(n = simulations, min = 0, max = theta) # Y ~ U(0,theta)
# any(y == theta); all(y < theta) # FALSE and TRUE, respectively
# No matter how large is the sample, Yi is always lower than 1
set.seed(1)
num_simulations <- sort(c(1, sample(c(2:simulations), 49)))
theta_mle <- unlist(lapply(num_simulations, function(n) 
  mean(max(runif(n = n, min = 0, max = theta)))))
plot(num_simulations, theta_mle, ylim = c(floor(min(theta_mle)), theta),
     xlab = "Number of simulations", ylab = "MLE of theta", pch = '*')
lines(num_simulations, theta_mle, lwd = 0.5, col = 'blue')
lines(num_simulations, theta*num_simulations/(num_simulations+1), col = 'green')
abline(h = theta, col = 'red', lty = 2)



## @knitr Question4-1
# QUESTION 4 --------------------------------------------------------------
# setwd('Lab2/data')
data <- read.csv('WageData2.csv')
summary(data)
round(stat.desc(data, desc = TRUE, basic = TRUE), 2)
# Lots of NAs, and some binary variables (raceColor, rural, city, z1, z2)
ggplot(data, aes(X)) + 
  geom_histogram(aes(y = ..count..), color = "black", fill = "white", 
                 bins = 20) + 
  labs(x = "Value", y = "Number of observations", 
       title = "Histogram of unnamed variable")
## @knitr Question4-2
data <- data %>% select(-X)
data_melt <- data %>% gather(variable, value)
ggplot(data_melt, aes(value)) + 
  geom_histogram(aes(y = ..count..), color = "black", fill = "white", 
                 bins = 20) + 
  facet_wrap(~ variable, scales = "free") + 
  labs(x = "Variable Value", y = "Number of observations", 
       title = "Histogram of all variables in the dataset")

## @knitr Question4-3
data_reduced <- data %>% 
  select(which(names(data) %in% names(data)[sapply(data, function(x) 
    length(levels(as.factor(x)))) > 2]))
pairs(data_reduced)

## @knitr Question4-4
ggpairs(data_reduced %>% na.omit()) + 
  theme(axis.ticks = element_blank(), axis.text =  element_blank()) 

## @knitr Question4-5
# Create two variables:
# (1) natural log of wage (name it `logWage`)
# (2) square of experience (name it `experienceSquare`)**
data <- data %>% 
  mutate(logWage = log(wage), experienceSquare = experience^2)



## @knitr Question5-1
# QUESTION 5 --------------------------------------------------------------
# setwd('Lab2/data')
data <- read.csv('wealthy_candidates.csv')
ggplot(data, aes(X)) + 
  geom_histogram(aes(y = ..count..), color = "black", fill = "white", 
                 bins = 20) + 
  labs(x = "Value", y = "Number of observations", 
       title = "Histogram of unnamed variable")
## @knitr Question5-2
data <- data %>% select(-X)
summary(data)
# Only 1 NA in absolute_wealth
data[is.na(data$absolute_wealth), ]
# The values of the other variables for that observations are not outliers
# We can omit that observation from our sample
data <- data %>% filter(!is.na(absolute_wealth))
# Region is categorical (3 possible values)
round(stat.desc(data[, names(sapply(data, 
                                    is.factor))[!sapply(data, is.factor)]], 
                desc = TRUE, basic = TRUE), 2)
data_melt <- data %>% gather(variable, value, - region)
ggplot(data_melt, aes(x=value)) + 
  geom_histogram(aes(y = ..count..), alpha=0.6, 
                 bins = 20, position = "dodge", fill = "white", color = "black") + 
  facet_wrap(~ variable, scales = "free") 
## @knitr Question5-3
ggplot(data_melt, aes(x=value, fill=region, color = region)) + 
  geom_histogram(aes(y = ..count..), alpha=0.6, 
                 bins = 20, position = "dodge") + 
  facet_wrap(~ variable, scales = "free") + 
  labs(x = "Variable Value", y = "Number of observations", 
       title = "Histogram of all variables in the dataset per Region")
## @knitr Question5-4
data_reduced <- data %>% 
  select(-region)
pairs(data_reduced)
## @knitr Question5-5
ggpairs(data_reduced %>% sample_n(500)) + 
  theme(axis.ticks = element_blank(), axis.text =  element_blank()) 



## @knitr Question6-1
# QUESTION 6 --------------------------------------------------------------
# setwd('Lab2/data')
load("retailSales.Rdata")
data <- retailSales
data <- data %>% 
  mutate(Year = as.factor(Year))
data_non_categorical <- data %>% 
  select(which(names(data) %in% names(data)[!sapply(data, is.factor)]))
data_melt <- data_non_categorical %>% gather(variable, value)
ggplot(data_melt, aes(value)) + 
  geom_histogram(aes(y = ..count..), color = "black", fill = "white", 
                 bins = 40) + 
  facet_wrap(~ variable, scales = "free", ncol = 4) + 
  labs(x = "Variable Value", y = "Number of observations", 
       title = "Histogram of all variables in the dataset")
## @knitr Question6-2
data_reduced <- data_non_categorical %>% 
  na.omit() %>% 
  sample_n(500)
pairs(data_reduced)
## @knitr Question6-3
ggpairs(data_reduced) + 
  theme(axis.ticks = element_blank(), axis.text =  element_blank()) 
