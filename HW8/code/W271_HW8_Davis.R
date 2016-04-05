## MIDS W271-4 HW8            ##
## Carin, Davis, Levato, Song ##


## @knitr Libraries-Functions-Constants
# LIBRARIES, FUNCTIONS AND CONSTANTS -------------------------------------------------
# Load libraries
# library(e1071)
library(ggplot2)
library(ggfortify)
library(scales)
library(knitr)
# library(pastecs)
# library(car)
# library(sandwich)
# library(lmtest)
library(dplyr)
library(tidyr)
library(stargazer)
# library(pander)
# library(texreg)
# library(weatherData)
library(scales)
library(xts)
library(reshape2)
library(lubridate)
library(forecast)

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



## @knitr ex1-load
# EXERCISE 1 --------------------------------------------------------------
# setwd('./HW8/data')
hw08 <- read.csv('hw08_series.csv', header = TRUE)
str(hw08)
all(hw08$X == 1:dim(hw08)[1]) # check if 1st column is just an incremental index
hw08 <- hw08[, -1]

## @knitr ex1-desc_stats
# See the definition of the function in ## @knitr Libraries-Functions-Constants
desc_stat(hw08, 'Time series', 'Descriptive statistics of the time series.')

## @knitr ex1-hist
hist(hw08, breaks = 30, col="gray", freq = FALSE, 
     xlab = "Level / Amplitude", main = "Histogram of the time series")
lines(density(hw08), col = 'blue', lty = 2)
leg.txt <- c("Estimated density plot")
legend("topright", legend = leg.txt, lty = 2, col = "blue", bty = 'n', cex = .8)

## @knitr ex1-time_plot
hw08.ts <- ts(hw08, start = c(1980,1), frequency = 12)
plot.ts(hw08.ts, col = 'blue', type = 'l', 
        xlab = "Year (time period: month)", ylab = "Level / Amplitude", 
        main = "Time-series plot of the data")
abline(h = mean(hw08), col = 'red', lty = 2)
lines(stats::filter(hw08.ts, sides=2, rep(1, 7)/7), lty = 1, lwd = 2.5, 
      col = "green")
leg.txt <- c("Time-series", "Mean value", 
             "13-Point Symmetric Moving Average")
legend("topleft", legend = leg.txt, lty = c(1, 2, 1), lwd = c(1, 1, 1.5), 
       col = c("blue", "red", "green"), bty = 'n', cex = .8)

## @knitr ex1-time_plot_zoom
plot.ts(window(hw08.ts, 2005), col = 'blue', type = 'l', 
        xlab = "Year (time period: month)", ylab = "Level / Amplitude", 
        main = paste0("Detail of the last 72 observations"))
abline(v = seq(2005, 2011), lty = 2, col = "gray")
# lines(stats::filter(hw08.ts, sides=2, rep(1, 7)/7), lty = 1, lwd = 1.75, 
#       col = "green")
# leg.txt <- c("Time-series", "13-Point Symmetric Moving Average")
# legend("topleft", legend = leg.txt, lty = c(1, 1), lwd = c(1, 1.75), 
#        col = c("blue", "green"), bty = 'n', cex = .8)

## @knitr ex1-boxplot
boxplot(hw08 ~ factor(rep(1980:2010, each = 12)), 
        outcex = 0.4, medcol="red", lwd = 0.5, 
        xlab = 'Year', ylab = 'Level / Amplitude',
        main = 'Box-and-whisker plot of\nthe time series per year')

## @knitr ex1-var_table
hw08_df <- data.frame(Year = factor(rep(1980:2010, each = 12)), 
                      Level = hw08)
hw08_df_mv <- hw08_df %>% 
  group_by(Year) %>% summarise(Mean = mean(Level), Variance = var(Level))
kable(cbind(hw08_df_mv %>% filter(Year %in% factor(1980:1989)), 
            hw08_df_mv %>% filter(Year %in% factor(1990:1999)), 
            hw08_df_mv %>% filter(Year %in% factor(2000:2009))), digits = 2, 
      caption = paste("Variance of the time-series amplitude per year ", 
                      "(for the first 30 out of 31)."))

## @knitr ex1-decompose
plot(decompose(hw08.ts, type = 'additive'), col = 'blue', 
     xlab = "Year (time period: month)")
# acf(na.omit(decompose(hw08.ts, type = 'additive')$random), lag.max = 24)
# pacf(na.omit(decompose(hw08.ts, type = 'additive')$random), lag.max = 24)
# plot(stl(hw08.ts, s.window="periodic"), col = 'blue')

## @knitr ex1-acf_pacf
# Plot the ACF and PACF of the series
par(mfrow=c(1, 2))
acf(hw08.ts,lag.max = 20000, main = "ACF of the time series")
pacf(hw08.ts, lag.max = 24, main = "PACF of the time series")
par(mfrow=c(1, 1))

## @knitr ex1-AR_models
best.AR.order <- best.MA.order <- best.ARMA.order <- c(0, 0, 0)
best.AR.aic <- best.MA.aic <- best.ARMA.aic <- Inf
for (i in 0:8) {
  # Try the corresponding order, skip if estimating the model yields an error
  # due to non-stationarity
  # Not using try() would cause the loop to break
  try(fit.AR.aic <- AIC(arima(hw08.ts, order = c(i, 0, 0))), silent = TRUE)
  if (fit.AR.aic < best.AR.aic) {
    best.AR.order <- c(i, 0, 0)
    best.ar <- arima(hw08.ts, order = best.AR.order)
    best.AR.aic <- fit.AR.aic
  }
}
best.AR.aic
best.AR.order
best.ar

Parameters <- cbind(best.ar$coef, 
                    sqrt(diag(best.ar$var.coef)), 
                    matrix(sapply(c(-2,2), function(i) 
                      best.ar$coef + i * sqrt(diag(best.ar$var.coef))), 
                      ncol = 2))
colnames(Parameters) <- c("Coefficient", "SE", "95% CI lower", "95% CI upper")
kable(Parameters, digits = 4, 
      caption = "Coefficients, SEs, and 95% CIs of the estimated AR(3) model")

## @knitr ex1-AR_res_plots
summary(best.ar$resid)
par(mfrow = c(2, 2))
plot(best.ar$resid, main = "Residual Series of\nthe AR(3) model", col="blue", 
     xlab = "Year (time period: month)", ylab = "Residual level")
hist(best.ar$resid, col = "gray", breaks = 20, xlab = "Residual level", 
     main = "Histogram of the residuals\n(AR(3) model")
acf(best.ar$resid, main = "ACF of the Residual\nSeries (AR(3) model)")
pacf(best.ar$resid, main = "PACF of the Residual\nSeries (AR(3) model)")

## @knitr ex1-AR_boxtest
Box.test(best.ar$resid, type = "Ljung-Box")


## @knitr ex1-AR_in_sample_fit

par(mfrow=c(1,1))
plot(hw08.ts, col= "navy",
     main = "Original vs Estimated Series (AR(3))",
     ylab = "Simulated and Estimated Values", lty=2)
lines(fitted(best.ar), col="blue")
leg.txt <- c("Original Series", "Estimated Series (AR(3))")
legend("topleft", legend=leg.txt, lty=c(2,1), 
         col=c("navy","blue"), bty='n', cex=1)


## @knitr ex1-MA_models
for (j in 1:8) {
  # No need to include j = 0 (MA(0) = AR(0) <=> xt = wt)
  # Try the corresponding order, skip if estimating the model yields an error
  # due to non-stationarity
  # Not using try() would cause the loop to break
  try(fit.MA.aic <- AIC(arima(hw08.ts, order = c(0, 0, j))), silent = TRUE)
  if (fit.MA.aic < best.MA.aic) {
    best.MA.order <- c(0, 0, j)
    best.ma <- arima(hw08.ts, order = best.MA.order)
    best.MA.aic <- fit.MA.aic
  }
}
best.MA.aic
best.MA.order
best.ma
Parameters <- cbind(best.ma$coef, 
                    sqrt(diag(best.ma$var.coef)), 
                    matrix(sapply(c(-2,2), function(i) 
                      best.ma$coef + i * sqrt(diag(best.ma$var.coef))), 
                      ncol = 2))
colnames(Parameters) <- c("Coefficient", "SE", "95% CI lower", "95% CI upper")
kable(Parameters, digits = 4, 
      caption = "Coefficients, SEs, and 95% CIs of the estimated MA(4) model")

## @knitr ex1-MA_res_plots
summary(best.ma$resid)
par(mfrow=c(2, 2))
plot(best.ma$resid, main = "Residual Series of\nthe MA(4) model", col="blue", 
     xlab = "Year (time period: month)", ylab = "Residual level")
hist(best.ma$resid, col = "gray", breaks = 20, xlab = "Residual level", 
     main = "Histogram of the residuals\n(MA(4) model")
acf(best.ma$resid, main = "ACF of the Residual\nSeries (MA(4) model)")
pacf(best.ma$resid, main = "PACF of the Residual\nSeries (MA(4) model)")

## @knitr ex1-MA_boxtest
Box.test(best.ma$resid, type = "Ljung-Box")

## @knitr ex1-MA_in_sample_fit

par(mfrow=c(1,1))
plot(hw08.ts, col= "navy",
     main = "Original vs Estimated Series (MA(8))",
     ylab = "Simulated and Estimated Values", lty=2)
lines(fitted(best.ma), col="blue")
leg.txt <- c("Original Series", "Estimated Series(MA(8))")
legend("topleft", legend=leg.txt, lty=c(2,1), 
         col=c("navy","blue"), bty='n', cex=1)

## @knitr ex1-ARMA_models
for (i in 1:4)
  for (j in 1:4) {
    # No need to include i = 0 and j = 0 (AR and MA models, already checked)
    # due to non-stationarity    
    # Try the corresponding order, skip if estimating the model yields an error
    # Not using try() would cause the loop to break
    try(fit.ARMA.aic <- AIC(arima(hw08.ts, order = c(i, 0, j))), silent = TRUE)
    if (fit.ARMA.aic < best.ARMA.aic) {
      best.ARMA.order <- c(i, 0, j)
      best.arma <- arima(hw08.ts, order = best.ARMA.order)
      best.ARMA.aic <- fit.ARMA.aic
    }
  }
best.ARMA.aic
best.ARMA.order
best.arma
Parameters <- cbind(best.arma$coef, 
                    sqrt(diag(best.arma$var.coef)), 
                    matrix(sapply(c(-2,2), function(i) 
                      best.arma$coef + i * sqrt(diag(best.arma$var.coef))), 
                      ncol = 2))
colnames(Parameters) <- c("Coefficient", "SE", "95% CI lower", "95% CI upper")
kable(Parameters, digits = 4, 
      caption = paste0("Coefficients, SEs, and 95% CIs of the estimated", 
                       "ARMA(2,1) model"))

## @knitr ex1-ARMA_res_plots
summary(best.arma$resid)
par(mfrow=c(2, 2))
plot(best.arma$resid, main = "Residual Series of\nthe ARMA(3,4) model", 
     col="blue", xlab = "Year (time period: month)", ylab = "Residual level")
hist(best.arma$resid, col = "gray", breaks = 20, xlab = "Residual level", 
     main = "Histogram of the residuals\n(ARMA(3,4) model")
acf(best.arma$resid, main = "ACF of the Residual\nSeries (ARMA(3,4) model)")
pacf(best.arma$resid, main = "PACF of the Residual\nSeries (ARMA(3,4) model)")

## @knitr ex1-ARMA_boxtest
Box.test(best.arma$resid, type = "Ljung-Box")

## @knitr ex1-ARMA_in_sample_fit

par(mfrow=c(1,1))
plot(hw08.ts, col= "navy",
     main = "Original vs Estimated Series (ARMA(3,4))",
     ylab = "Simulated and Estimated Values", lty=2)
lines(fitted(best.arma), col="blue")
leg.txt <- c("Original Series", "Estimated Series(ARMA(3,4))")
legend("topleft", legend=leg.txt, lty=c(2,1), 
         col=c("navy","blue"), bty='n', cex=1)

  
## @ knitr ex1-Forecast

# Re-estimate the model withholding last 12 observations
hw08.ts_fc <- ts(hw08.ts[1:360], start = c(1980,1), frequency = 12)
arma34 <- arima(hw08.ts_fc, order = best.ARMA.order)
# Forecast 12 steps ahead
arma34.fcast <- forecast.Arima(arma34, 12)

par(mfrow =c(1,1))
plot(arma34.fcast, col="navy", lty=2,
     main="12-Step Ahead Forecast and Original & Estimated Series")
lines(ts(hw08.ts[360:372], start = c(2010,1), frequency = 12),
      col= "red", lty=1, lwd = 2)
lines(fitted(arma34), col= "darkcyan", lwd = 1)
leg.txt <- c("Forecasted Values", "Original Series",
             "Estimated Series", "Actual Values")
legend("topleft", legend=leg.txt, lty=c(1,2, 1, 1), lwd=c(2,1,1,2),
         col=c("blue","navy","darkcyan", "red"), bty='n', cex=1)


## @knitr ex1-120

best.MA.aic; best.MA.order; best.ma
best.ARMA.aic; best.ARMA.order; best.arma


## @knitr ex1-100
hw08.ts_out = window(hw08.ts, start = 1980, end = c(2008,12))
Imth <- factor(cycle(hw08.ts_out))
Time <- 1:length(hw08.ts_out)
hw08.lm <- lm(hw08.ts_out ~ Time + I(Time^2)+ Imth)
best.arma <- arima(resid(hw08.lm), order = c(2,0,1))
new.time <- seq(length(hw08.ts)-24+1, length = 24)
new.data <- data.frame(Time = new.time, Imth = factor(rep(1:12, 2)))
predict.lm <- predict(hw08.lm, new.data)
predict.arma <- predict(best.arma, n.ahead = 24)
hw08.pred <- ts(predict.lm + predict.arma$pred, start = 2009, freq=12)
ts.plot(cbind(hw08.ts_out, hw08.pred), ylim = c(30, 150))
par(new=T)
plot.ts(hw08.ts, ylim = c(30, 150), col = 'blue', lty = 2)

hw08.arma21.fit2 <- Arima(resid(hw08.lm), order=c(2,0,1))
hw08.arma21.fit2.fcast <- forecast.Arima(hw08.arma21.fit2, h = 24)
plot(hw08.arma21.fit2.fcast, ylim = c(-20, 20))
par(new=T)
plot.ts(c(hw08[1:348]-fitted(hw08.lm), hw08[349:372]-predict.lm), ylim = c(-20, 20))

plot(hw08.ts_out, ylim = c(30, 140), ylab = "", lwd = 2)
par(new = T)
plot.ts(ts(fitted(hw08.lm), start = 1980, freq = 12), ylim = c(30, 140), 
        col = 'blue')
polygon(c(time(hw08.ts_out), rev(time(hw08.ts_out))), 
        c(predict(hw08.lm, interval = "confidence")[, 3], 
          rev(predict(hw08.lm, interval = "confidence")[, 2])),
        col=rgb(0, 0, 0, 0.25), border = NA)


predict.lm <- predict(hw08.lm, new.data, interval = "confidence")
hw08.arma21.fit2.fcast
# plot.ts(window(hw08.ts, start = 2009, end = c(2010,12)), ylim = c(120, 160))
# par(new = TRUE)

plot.ts(hw08.ts, ylim = c(40, 160), lty = 2)
par(new = TRUE)
uno <- ts(c(hw08.ts_out, predict.lm[, 1]), start = 1980, freq = 12)
dos <- ts(c(rep(0, 348), hw08.arma21.fit2.fcast$mean), start = 1980, freq = 12)

plot(uno + dos, ylim = c(40, 160), col = 'blue', lty = 1, lwd = 2)
uno <- ts(predict.lm[, 1], start = 2009, freq = 12)
dos <- ts(hw08.arma21.fit2.fcast$mean, start = 2009, freq = 12)
polygon(c(time(uno), rev(time(dos))), 
        c(predict.lm[, 3] + hw08.arma21.fit2.fcast$upper[,'95%'], 
          rev(predict.lm[, 2] + hw08.arma21.fit2.fcast$lower[,'95%'])),
        col=rgb(0, 0, 0, 0.25), border = NA)


predict(hw08.lm, interval = "confidence")
arima(resid(hw08.lm), order = c(2, 0, 2))
acf(arima(resid(hw08.lm), order = c(2, 0, 2))$resid)


# library(forecast)
m2 <- tslm(hw08.ts~trend)
f <- forecast(m2, h = 12, level=c(80,95))
plot(f)
lines(fitted(m2))
summary(m2)
d <- data.frame(x = hw08, time = time(hw08.ts))
m <- lm(x~time, d)
plot(hw08.ts)
abline(m)

# library(car)
linearHypothesis(hw08.lm, sapply(c(2:12), function(i) paste0("Imth", i)))


(hw08.arma21.fit <- arima(hw08.ts, order=c(2,0,1)))
summary(hw08.arma21.fit)

## @knitr ex1-10000

# Diagnostics using residuals
# Visualization
summary(hw08.arma21.fit$resid)
par(mfrow=c(2, 2))
library(forecast)
plot(hw08.arma21.fit$resid, fitted(hw08.arma21.fit), 
     main = "Residuals vs Fitted Series", 
     xlab = "Residuals", ylab = "Fitted Values")
abline(lm(fitted(hw08.arma21.fit) ~ hw08.arma21.fit$resid))
plot.ts(hw08.arma21.fit$resid, main = "Residual Series", ylab = "Residuals")
acf(hw08.arma21.fit$resid , main = "ACF of the Residual Series")
pacf(hw08.arma21.fit$resid, main = "PACF of the Residual Series")
par(mfrow=c(1, 1))

# Check for conditional heteroscedasticity
acf(hw08.arma21.fit$resid^2 , main="ACF of the Residual Series")
pacf(hw08.arma21.fit$resid^2, main="PACF of the Residual Series")
# Observations: 
# 1. The residuals clearly show a relationship with the fitted series,
#    indicating that there are aspects of the series that are left unexplained
# 2. The residual series' t-plot does not look like a realization of a white 
#    noise
# 3. Both ACF and PACF show strong autocorrelations and partial autocorrelations

# Ljung-Box test of residual dynamics (or lack thereof)
# Reference: 
# https://stat.ethz.ch/R-manual/R-patched/library/stats/html/box.test.html
# These tests are sometimes applied to the residuals from an ARMA(p, q) fit,
# in which case the references suggest a better approximation to the 
# null-hypothesis distribution is obtained by setting fitdf = p+q, provided of 
# course that lag > fitdf.
Box.test(hw08.arma21.fit$resid, type="Ljung-Box") # Box-Pierce test
# Observations: the null hypothesis of independence of the residual series is 
#               strongly rejected.

# x_t = \phi_1 x_{t-1} + \phi_1 x_{t-2} + \omega_t + \theta_1 \omega_{t-1}
# \Phi_p(B)x_t = \Theta_q(B)\omega_t \ (p=2,q=1)
# \Phi_p(B) = 1 - \phi_1 B - \phi_2 B^2, \Theta(B) = 1 + \theta_1 B
all(Mod(polyroot(c(1, -hw08.arma21.fit$coef[1:2]))) > 1) # AR roots
all(Mod(polyroot(c(1, hw08.arma21.fit$coef[3]))) > 1) # MA roots


# 5. Model Performance Evaluation
summary(hw08.arma21.fit$resid)
par(mfrow=c(1,1))
plot(hw08.ts, 
     main = "Original vs a MA4 Estimated Series with Resdiauls",
     ylab = "Simulated and Estimated Values",
     ylim = c(40, 160))
leg.txt <- c("Original Series", "Estimated Series", "Residuals")
legend("topright", legend=leg.txt, lty=1, col=c("black","blue","green"),
       bty='n', cex=1)
lines(fitted(hw08.arma21.fit),col="blue")
# Observation: Surprisingly, the in-sample fit looks reasonable
par(new=T)
plot.ts(hw08.arma21.fit$resid,axes=F,xlab="",ylab="",col="green",
        ylim=c(-12, 8), lty=1, pch=1, col.axis="green")
axis(side=4, col="green")
mtext("Residuals", side=4, line=2,col="green")

df <- cbind(hw08, fitted(hw08.arma21.fit), hw08.arma21.fit$resid)
head(df)

# 6. Forecast / Statistical Inference
# As we noted above, the model's underlying assumption is incorrect.
# However, let's continue with the exercise anyway.

# rm(ma4.nzfit.fcast)

# 372 observations = 372/12 = 31 years
# 10% > 3 years (36 months) ==> 1:336, 337:372
hw08.arma21.fit.fcast <- forecast.Arima(hw08.arma21.fit, h = 24)
hw08.arma21.fit.fcast2 <- predict(hw08.arma21.fit, n.ahead = 24)
summary(hw08.arma21.fit.fcast2$pred)
plot(hw08.arma21.fit.fcast, main="24-Step Ahead Forecast and Original & Estimated Series",
     xlab="Simulated Time Period", ylab="Original, Estimated, and Forecasted Values",
     xlim=c())
lines(fitted(hw08.arma21.fit),col="blue")  
lines(hw08.ts, col='red')

# 7. Model Evaluation - An Altervative Method called Backtesting

# Step 1: Re-estimate the model leaving out the last 10% of the observations.
#         For this series, I leave out 54 observations, which is 4.5 years worth
#         of data
hw08.arma21.fit2 <- Arima(hw08[1:(length(hw08)-36)], order=c(13,0,13))
summary(hw08.arma21.fit2)
fitted(hw08.arma21.fit2)  
# cbind(hw08[1:(length(hw08)-36)], fitted(hw08.arma21.fit2), hw08.arma21.fit2$resid)
plot(hw08.arma21.fit2$resid)

# Step 2: Forecast
hw08.arma21.fit2.fcast <- forecast.Arima(hw08.arma21.fit2, h = 36)
plot(hw08.arma21.fit2.fcast, ylim = c(40, 160))
lines(hw08, col = 'navy')
cbind(hw08[337:length(hw08)], hw08.arma21.fit2.fcast$mean[1:36])
lines(c(fitted(hw08.arma21.fit2), hw08.arma21.fit2.fcast$mean[1:36]), col = 'red')
lines(fitted(hw08.arma21.fit2), xlim=c(1,336), col = 'blue')


plot(hw08.arma21.fit2.fcast, ylim = c(40, 160), lty = 2)
par(new = TRUE)
plot.ts(hw08.ts, col="navy",axes=F,ylab="", lty=1,ylim = c(40, 160))


z1 <- ts(hw08[337:length(hw08)])
z2 <- ts(hw08.arma21.fit2.fcast$mean[1:36])
ts.plot(z1,z2, gpars = list(col = c("black", "blue")))

library(reshape2)
z2b <- ts(rbind(melt(hw08[1:336]), melt(hw08.arma21.fit2.fcast$mean[1:36])))
ts.plot(hw08, z2b, gpars = list(col = c("black", "blue")))
ts.plot(z2b)
cbind(hw08,z2b)

library(reshape2)
z2b<- ts(rbind(melt(as.numeric(fitted(hw08.arma21.fit2))),melt(hw08.arma21.fit2.fcast$mean[1:36])))
ts.plot(hw08, z2b, gpars = list(col = c("black", "blue")))
cbind(hw08,z2b)

length(hw08)
length(z2b)

##########
fit <- Arima(hw08[1:(length(hw08)-36)], order=c(2,0,1))
summary(fit)
length(fitted(fit))
length(fit$resid)
cbind(hw08[1:(length(hw08)-36)], fitted(fit), fit$resid)

# Plot the original and estimate series 
par(mfrow=c(1,1))
plot.ts(hw08[1:(length(hw08)-36)], col="navy", 
        main="Original vs a MA5 Estimated Series with Resdiauls",
        ylab="Original and Estimated Values",
        ylim=c(40,160), pch=1)
par(new=T)
plot.ts(fitted(fit),col="blue",axes=T,xlab="",ylab="",
        ylim=c(40,160)) 
leg.txt <- c("Original Series", "Estimated Series", "Residuals")
legend("top", legend=leg.txt, lty=1, col=c("navy","blue","green"),
       bty='n', cex=1)
par(new=T)
plot.ts(fit$resid,axes=F,xlab="",ylab="",col="green",
        ylim=c(-6,6), pch=1)
axis(side=4, col="green")
mtext("Residuals", side=4, line=2,col="green")

# Step 2: Out-of-Sample Forecast
fit.fcast <- forecast.Arima(fit, h=36)
length(fit.fcast$mean)

plot(fit.fcast,lty=2,
     main="Out-of-Sample Forecast",
     ylab="Original, Estimated, and Forecast Values", ylim=c(40,160))
par(new=T)
plot.ts(hw08, col="navy",axes=F, ylab="", lty=1, ylim=c(40,160))
leg.txt <- c("Original Series", "Forecast series")
legend("top", legend=leg.txt, lty=1, col=c("black","blue"),
       bty='n', cex=1)
##########