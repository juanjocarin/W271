## MIDS W271-4 HW7            ##
## Carin, Davis, Levato, Song ##


## @knitr Libraries-Functions-Constants
# LIBRARIES, FUNCTIONS AND CONSTANTS -------------------------------------------------
# Load libraries
# library(e1071)
library(ggplot2)
library(ggfortify)
library(scales)
library(knitr)
library(pastecs)
# library(car)
# library(sandwich)
# library(lmtest)
library(dplyr)
library(tidyr)
library(stargazer)
library(pander)
# library(texreg)
# library(weatherData)
library(scales)
library(xts)
library(reshape2)
library(lubridate)

# Define functions

# A function to apply format
frmt <- function(qty, digits = 3) {
  formatC(qty, digits = digits, format = "f", drop0trailing = FALSE, 
          big.mark = ",")
}

# A function to present descriptive statistics of 1 or more vectors
desc_stat <- function(x, variables, caption) {
  ds <- data.frame(x)
  names(ds) <- letters[1:dim(ds)[2]]
  
  ds <- ds %>%
    gather %>%
    group_by(Variable = key) %>%
    summarise(Mean = mean(value), 'St. Dev' = sd(value), 
              '1st Quartile' = quantile(value, 0.25), Median = median(value),
              '3rd Quartile' = quantile(value, 0.75), Min = min(value), 
              Max = max(value)) %>% 
    select(-Variable) %>% 
    t %>% data.frame
  kable(ds, digits = 2, caption = caption, col.names = variables)
  
  # ds <- ds %>%
  #   summarise_each(funs(Mean =mean, 'St. Dev' = sd, 
  #                       '1st Quartile' = quantile(., 0.25), Median = median, 
  #                       '3rd Quartile' = quantile(., 0.75), Min = min, 
  #                       Max = max)) %>% 
  #   gather
  # if (dim(ds)[1] > 7)
  #   ds <- ds %>%
  #   mutate(Statistic = gsub(".*\\_", "", key),
  #          variable = gsub("\\_.*", "", key)) %>%
  #   select(-key) %>%
  #   spread(key = variable, value = value) %>%
  #   mutate(order = c(3, 5, 7, 1, 3, 6, 2)) %>%
  #   arrange(order) %>%
  #   select(-order)
  # kable(ds, col.names = c('', variables), digits = 2, caption = caption)
}

# Define constants



## @knitr Question1-1
# QUESTION 1 --------------------------------------------------------------
# Load hw07_series1.csv
# setwd('./HW7/data')
hw07 <- read.csv('hw07_series1.csv', header = FALSE) # CSV has no headers
names(hw07)


## @knitr Question1-2
# Describe the basic structure of the data and 
# provide summary statistics of the series
str(hw07)
dim(hw07)
# See the definition of the function in ## @knitr Libraries-Functions-Constants
desc_stat(hw07, 'Time series', 'Descriptive statistics of the time series')

## @knitr Question1-3-1
# Plot histogram and time-series plot of the series
# Describe the patterns exhibited in histograrm and time-series plot
hw07.ts <- hw07[, 1]
hist(hw07.ts, breaks = 15, col="gray", freq = FALSE, 
     xlab = "Level / Amplitude", main = "Histogram of the time series")
lines(density(hw07.ts), col = 'blue', lty = 2)
leg.txt <- c("Estimated density plot")
legend("topleft", legend = leg.txt, lty = 2, col = "blue", bty = 'n', cex = .8)

## @knitr Question1-3-2
more_desc_stat <- as.matrix(round(stat.desc(hw07, desc = FALSE, basic = FALSE, 
                                            norm = TRUE), 3))
colnames(more_desc_stat) <- "Time series"
kable(more_desc_stat, 
      caption = "Descriptive statistics about normality of the time series")

## @knitr Question1-3-3
plot(1:length(hw07.ts), hw07.ts, col = 'blue', type = 'l', 
     xlab = "Observation / Time Period", ylab = "Level / Amplitude", 
     main = "Time-series plot")
abline(h = mean(hw07.ts), col = 'red', lty = 2)
lines(stats::filter(hw07.ts, sides=2, rep(1, 9)/9), lty = 1, lwd = 1.5, 
      col = "green")
leg.txt <- c("Time-series", "Mean value", 
             "9-Point Symmetric Moving Average")
legend("bottomright", legend = leg.txt, lty = c(1, 2, 1), 
       col = c("blue", "red", "green"), bty = 'n', cex = .8)


## @knitr Question1-4
# Plot the ACF and PACF of the series
par(mfrow=c(1, 2))
acf(hw07.ts, lag.max = 20, main = "ACF of the time series")
pacf(hw07.ts, lag.max = 20, main = "PACF of the time series")
par(mfrow=c(1, 1))


## @knitr Question1-5
# Estimate the series using the `ar()` function
(hw07.arfit <- ar(hw07.ts, method = "mle"))


## @knitr Question1-6-1
# Report the estimated AR parameters, the order of the model, and SEs
hw07.arfit$order # order of the AR model with lowest AIC

## @knitr Question1-6-2
hw07.arfit$ar # parameter estimates

## @knitr Question1-6-3
hw07.arfit$aic # AICs of the fit models (differences vs. best model)
hw07.arfit$x.mean; mean(hw07.ts) # mean of the fit model and the data
hw07.arfit$var.pred # prediction variance
hw07.arfit$asy.var.coef # asymptotic Covariance matrix

## @knitr Question1-6-4
Parameters <- cbind(hw07.arfit$ar, 
                    sqrt(diag(hw07.arfit$asy.var.coef)), 
                    matrix(sapply(c(-2,2), function(i) 
                      hw07.arfit$ar + i * sqrt(diag(hw07.arfit$asy.var.coef))), 
                      ncol = 2))
rownames(Parameters) <- c("lag 1", "lag 2")
colnames(Parameters) <- c("Coefficient", "SE", "95% CI lower", "95% CI upper")
kable(Parameters, digits = 4, 
      caption = "Coefficients, SEs, and 95% CIs of the estimated AR(2) model")

## @knitr Question1-6-5
hw07.arfit2 <- arima(hw07.ts, order = c(2, 0, 0), method = "ML")
Parameters2 <- cbind(hw07.arfit2$coef[1:2], 
                     sqrt(diag(hw07.arfit2$var.coef))[1:2], 
                     matrix(sapply(c(-2,2), function(i) 
                       hw07.arfit2$coef[1:2] + 
                         i * sqrt(diag(hw07.arfit2$var.coef))[1:2]), ncol = 2))
rownames(Parameters2) <- c("lag 1", "lag 2")
colnames(Parameters2) <- c("Coefficient", "SE", "95% CI lower", "95% CI upper")
kable(Parameters2, digits = 4, 
      caption = paste0("Coefficients, SEs, and 95% CIs of the estimated AR(2)", 
                       " model when using the `arima` function"))



## @knitr Question2-1
# QUESTION 2 --------------------------------------------------------------
# Simulate a time series of lenght 100 for the following model
# Name the series x
set.seed(12345) # Fix a seed to get same results every time
x <- arima.sim(model = list(ar = c(5/6, -1/6), ma = 0), n = 100)
par(mfrow=c(2, 1))
hist(x, breaks = 15, col="gray", freq = FALSE, 
     xlab = "Level / Amplitude", 
     main = "Histogram of the simulated time series")
lines(density(x), col = 'blue', lty = 2)
# leg.txt <- c("Estimated density plot")
# legend("topleft", legend = leg.txt, lty = 2, col = "blue", bty = 'n', cex = .6)
plot(1:length(x), x, col = 'blue', type = 'l', 
     xlab = "Observation / Time Period", ylab = "Level / Amplitude", 
     main = "Time-series plot")
abline(h = mean(x), col = 'red', lty = 2)
lines(stats::filter(x, sides=2, rep(1, 9)/9), lty = 1, lwd = 1.5, 
      col = "green")
# leg.txt <- c("Simulated time-series", "Mean value", 
#              "9-Point Symmetric Moving Average")
# legend("topleft", legend = leg.txt, lty = c(1, 2, 1), 
#        col = c("blue", "red", "green"), bty = 'n', cex = .6)
par(mfrow=c(1, 1))


## @knitr Question2-2
# Plot the correlogram and partial correlogram for the simulated series
par(mfrow=c(1, 2))
acf(x, lag.max = 30, main = "ACF of the simulated\ntime series")
pacf(x, lag.max = 30, main = "PACF of the simulated\ntime series")
par(mfrow=c(1, 1))


## @knitr Question2-3
# Estimate an AR model for this simulated series
# Report the estimated AR parameters, standard errors, and the order of the AR 
# model
x.arfit <- ar(x, method = "mle")
# Estimated AR parameters
x.arfit$ar
# Standard errors
sqrt(diag(x.arfit$asy.var.coef))
# Order of the AR model (with lowest AIC)
x.arfit$order
# x.arfit$aic # AICs of the fit models (differences vs. best model)
# x.arfit$asy.var.coef # asymptotic Covariance matrix
# x.arfit$x.mean; mean(x) # mean of the fit model and the data
# x.arfit$var.pred # prediction variance


## @knitr Question2-4
# Construct a 95% confidence intervals for the parameter estimates of the 
# estimated model
Parameters <- cbind(x.arfit$ar, sqrt(diag(x.arfit$asy.var.coef)), 
                    matrix(sapply(c(-2,2), function(i) 
                      x.arfit$ar + i * sqrt(diag(x.arfit$asy.var.coef))), 
                      ncol = 2))
rownames(Parameters) <- sapply(1:dim(Parameters)[1], function(i) 
  paste("lag", i))
colnames(Parameters) <- c("Coefficient", "SE", "95% CI lower", "95% CI upper")
kable(Parameters, digits = 4, 
      caption = "Coefficients, SEs, and 95% CIs of the estimated model")


## @knitr Question2-4-2
x.arfit2 <- arima(x, order = c(2, 0, 0), method = "ML")
Parameters2 <- cbind(x.arfit2$coef[1:2], sqrt(diag(x.arfit2$var.coef))[1:2], 
                     matrix(sapply(c(-2,2), function(i) 
                       x.arfit2$coef[1:2] + 
                         i * sqrt(diag(x.arfit2$var.coef))[1:2]), ncol = 2))
rownames(Parameters2) <- c("lag 1", "lag 2")
colnames(Parameters2) <- c("Coefficient", "SE", "95% CI lower", "95% CI upper")
kable(Parameters2, digits = 4, 
      caption = paste0("Coefficients, SEs, and 95% CIs of the estimated", 
                       " model when using the `arima` function"))



## @knitr Question2-5
# True model
pol_true <- c(1, -5/6, 1/6); (roots <- polyroot(pol_true))
Mod(roots); all(Mod(roots) > 1)
# Fitted model
(pol_fitted <- c(1, -x.arfit$ar)); (roots <- polyroot(pol_fitted))
Mod(roots); all(Mod(roots) > 1)


## @knitr Question2-6
# Plot the correlogram of the residuals of the estimated model
acf(na.omit(x.arfit$resid), lag.max = 20, 
    main = "ACF of the residuals of\nthe estimated model")

## @knitr Question2-6-2
qqnorm(na.omit(x.arfit$resid), main="Normal Q-Q Plot of the Residuals")
qqline(na.omit(x.arfit$resid), col="blue")
