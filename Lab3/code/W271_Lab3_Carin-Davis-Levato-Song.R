## Carin, Davis, Levato, Song ##


## @knitr Libraries-Functions-Constants
# LIBRARIES, FUNCTIONS AND CONSTANTS -------------------------------------------------
# Load libraries
# library(e1071)
library(gtools)
library(ggplot2)
library(ggfortify)
library(scatterplot3d)
library(cowplot)
library(scales)
library(knitr)
library(pastecs)
library(car)
library(sandwich)
library(lmtest)
library(plyr)
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
library(forecast)
library(zoo)

library(fGarch)
library(quantmod)
library(tseries)

library(GGally)
library(lattice)
library(corrgram)
#library(TSA)

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
par(cex.main = 1, cex.lab = 0.9, cex.axis = 0.9)
set.seed(1234)



## @knitr P1-load
# Lab 3 - Part 1 ----------------------------------------------------------
# setwd('./Lab3/data')
houseValue <- read.csv('houseValueData.csv', header = TRUE)
round(stat.desc(houseValue, desc = FALSE, norm = TRUE)[c("nbr.val", "nbr.na", 
                                                         "skewness", 
                                                         "kurtosis", 
                                                         "normtest.p"),], 3)

## @knitr P1-summary
stargazer(houseValue, header = FALSE, 
          title = "Summary statistics of house values and features", 
          summary.stat = c("mean", "sd", "min", "p25", "median", "p75", "max"))

## @knitr P1-CrimeRate
crime_plot <- ggplot(data=houseValue, aes(crimeRate_pc)) + 
  geom_histogram(aes(y = (..count..)/sum(..count..)), boundary = 0, 
                 binwidth = 2.5, colour = 'black', fill = 'white') + 
  labs(title = "Histogram of crimeRate", 
       x = "Number of crimes per 1,000 residents", 
       y = "Relative Frequency") + theme_gray()
logCrime_plot <- ggplot(data=houseValue, aes(log(crimeRate_pc))) + 
  geom_histogram(aes(y = (..count..)/sum(..count..)), 
                 binwidth = .25, colour = 'black', fill = 'white') +
  labs(title = "Histogram of log(crimeRate)",
       x = "Log(Number of crimes per 1,000 residents)", 
       y = "Relative Frequency") +  theme_gray()
plot_grid(crime_plot, logCrime_plot)

## @knitr P1-Business
business_plot <- ggplot(data=houseValue, aes(nonRetailBusiness)) + 
  geom_histogram(aes(y=(..count..)/sum(..count..)), boundary = 0, 
                 binwidth = .01, colour = 'black', fill = 'white')+
  labs(title="Histogram of nonRetailBusiness",
       x = "Proportion of acres dedicated to non-retail business", 
       y="Relative Frequency") + theme_gray()
logBusiness_plot <- ggplot(data=houseValue, aes(log(nonRetailBusiness))) + 
  geom_histogram(aes(y=(..count..)/sum(..count..)), binwidth = .15, 
                 colour='black', fill='white') + 
  labs(title="Histogram of log(nonRetailBusiness)",
       x = "Log(Prop. of acres dedicated to non-retail business)",
       y="Relative Frequency") + theme_gray()
plot_grid(business_plot, logBusiness_plot)

## @knitr P1-withWater
ggplot(data=houseValue, aes(factor(withWater, labels = c('Not near Water', 
                                                         'Near Water')))) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), colour='black', fill='white') + 
  labs(title = "Proportion of houses within 5 miles of a water body", 
       y = "Proportion", x = element_blank()) + theme_gray()

## @knitr P1-houseAge
house_plot <- ggplot(data=houseValue, aes(ageHouse)) +
  geom_histogram(aes(y=(..count..)/sum(..count..)),
                 boundary=0, binwidth = 5,
                 colour='black', fill='white') +
  labs(title="Histogram of ageHouse",
       x= "Proportion of house built before 1950",
       y= "Relative Frequency") + theme_gray()
logHouse_plot <- ggplot(data=houseValue, aes(log(ageHouse))) +
  geom_histogram(aes(y=(..count..)/sum(..count..)),
                 binwidth = 0.2,
                 colour='black', fill='white') +
  labs(title="Histogram of log(ageHouse)",
       x= "Log(proportion of house built before 1950)",
       y= "Relative Frequency") + theme_gray()
plot_grid(house_plot, logHouse_plot)

## @knitr ex1-cityDistance
city_plot <- ggplot(data=houseValue, aes(distanceToCity)) + 
  geom_histogram(aes(y=(..count..)/sum(..count..)),
                 boundary=0, binwidth = 1.5,
                 colour='black', fill='white') +
  labs(title="Histogram of distanceToCity",
       x = "Distance to nearest city (in miles)",
       y = "Relative Frequency") + theme_gray()
logCity_plot <- ggplot(data=houseValue, aes(log(distanceToCity))) +
  geom_histogram(aes(y=(..count..)/sum(..count..)),
                 binwidth = 0.2,
                 colour='black', fill='white') +
  labs(title="Histogram of log(distanceToCity)",
       x = "Log(Distance to nearest city, in miles)",
       y  = "Relative Frequency") + theme_gray()
plot_grid(city_plot, logCity_plot)



## @knitr P2-load
# Lab 3 - Part 2 ----------------------------------------------------------
# setwd('./Lab3/data')
financial <- read.csv('lab3_series02.csv', header = TRUE)
head(financial)
# Check if 1st column is just an incremental index
all(financial$X == 1:dim(financial)[1])
financial <- financial[, -1]
c(head(financial), tail(financial)) # 1st and last observations
summary(financial)
round(stat.desc(as.data.frame(financial), desc = TRUE, norm = TRUE), 2)

## @knitr P2-histogram
hist(financial, breaks = 20, freq = FALSE, 
     xlab = "Time period", 
     main = paste0("Histogram (and approximate density plot) of the\n", 
                   "financial time series"), cex.main = 0.8)
lines(density(financial), col = 'blue')

## @knitr P2-timeplot
par(mar = c(5, 5, 4, 2) + 0.1)
plot(1:length(financial), financial, type = 'l', xlab = "Time period", 
     ylab = "Value of the financial time series", 
     main = "Financial time series")
lines(1:length(financial), stats::filter(financial, sides=2, rep(1, 53)/53), 
      lwd = 2, col = rgb(0, 1, 0, 0.6))
leg.txt <- c("Original Series", "53-Point Symmetric Moving Average")
legend("topleft", legend=leg.txt, lty = c(1, 1), col=c("black", "green"), 
       bty = 'n', cex = .8, merge = TRUE, bg = 336)
par(mar = c(5, 4, 4, 2) + 0.1)

## @knitr P2-ACF_PACF
par(mfrow = c(1, 2), cex.main = 0.9)
stats::acf(financial, lag = 100, main = "ACF of the financial time series")
pacf(financial, lag = 100, main = "PACF of the financial time series")
par(mfrow = c(1, 1), cex.main = 1)

## @knitr P2-ACF_PACF_2
stats::acf(diff(financial), lag = 100, 
           main = "ACF of the differenced financial time series")

## @knitr P2-return
ret <- diff(financial) / financial[2:length(financial)]
diff_log <- diff(log(financial))
tail(cbind(ret, diff_log))
head(cbind(ret, diff_log))

## @knitr P2-timeplot_ret
par(mfrow = c(2, 1))
plot(1:length(financial), log(financial), type = 'l', xlab = "Time period", 
     ylab = "Log(value)", 
     main = "Log of the financial time series")
plot(1:length(diff_log), diff_log, type = 'l', xlab = "Time period", 
     ylab = "Log Return", 
     main = "Log Return of the financial time series")
par(mfrow = c(1, 1))

## @knitr P2-ACF_PACF_ret
par(mfrow = c(2, 2), cex.main = 0.9)
stats::acf(log(financial), lag = 100, 
           main = "ACF of the log of the financial time series")
pacf(log(financial), lag = 100, 
     main = "PACF of the log of thefinancial time series")
stats::acf(diff_log, lag = 100, 
           main = "ACF of the log return of the financial time series")
pacf(diff_log, lag = 100, 
     main = "PACF of the log return of the financial time series")
par(mfrow = c(1, 1), cex.main = 1)

## @knitr P2-forecast
arima010.fit <- Arima(log(financial), order = c(0, 1, 0))
arima010.fit.fcast <- forecast.Arima(arima010.fit, h = 36, level = .95)
# NO NEED TO APPLY EXP(): Arima() ADMITS A BOX-COX TRANSFORMATION
  # WHICH IS EQUAL TO LOG WHEN LAMBDA = 0
arima010.fit2 <- Arima(financial, order=c(0, 1, 0), lambda = 0)
arima010.fit.fcast2 <- forecast.Arima(arima010.fit2, h = 36)

## @knitr P2-forecast_2
# ts.plot(ts(c(financial[1:length(financial)], exp(arima010.fit.fcast$mean))), 
#         ts(financial[1:length(financial)]), ylim=c(0,105), 
#         gpars = list(col = c("blue", "black"), lty = c(2, 1)), 
#         xlab = "Time period", ylab = "Original and Forecasted Values", 
#         main = paste0("36-step ahead Forecast and Original Series\n", 
#                       "ARIMA(0,1,0) of log"))
# polygon(c((length(financial)+1):(length(financial)+36), 
#           rev((length(financial)+1):(length(financial)+36))), 
#         c(exp(arima010.fit.fcast$upper), rev(exp(arima010.fit.fcast$lower))), 
#         col=rgb(0, 0, 0, 0.25), border = NA)
# fc<-forecast(arima010.fit, h = 36)
# fc$mean<-exp(fc$mean)
# fc$upper<-exp(fc$upper)
# fc$lower<-exp(fc$lower)
# fc$x<-exp(fc$x)
# plot(fc, ylim=c(0,105), xlab = "Time period", 
#      ylab = "Original and Forecasted Values", 
#      main = paste0("36-step ahead Forecast and Original Series\n", 
#                    "ARIMA(0,1,0) of log"))
plot(arima010.fit.fcast2, ylim=c(0,105), xlab = "Time period", 
     ylab = "Original and Forecasted Values", 
     main = paste0("36-step ahead Forecast and Original Series\n", 
                   "ARIMA(0,1,0) of log"))
leg.txt <- c("Original series", "Forecasts (ARIMA(1,1,3)/GARCH(1,1))")
legend("topleft", legend = leg.txt, lty = c(1, 1), lwd = c(1, 1), 
       col = c("black", "blue"), bty = 'n', cex = 0.9)

## @knitr P2-cond_var
stats::acf(resid(arima010.fit2)^2, 
           main = paste0("ACF of the squared residuals of the ARIMA(0,1,0)\n", 
                         "model fitted to the log of the series"))

## @knitr P2-garch
financial.garch <- garch(resid(arima010.fit), trace = FALSE)

## @knitr P2-garch_2
par(mfrow = c(1, 2), cex.main = 0.9, par(mar = c(5, 4, 6, 2) + 0.1))
acf(financial.garch$res[-1], lag.max = 24, 
    main = paste0("ACF of the residuals of an\nARIMA(1,1,3)/GARCH(1,1) model", 
                  "\nfitted to the log of the series"))
acf(financial.garch$res[-1]^2, lag.max = 24, 
    main = paste0("ACF of the squared residuals of an\nARIMA(1,1,3)/GARCH(1,1) model", 
                  "\nfitted to the log of the series"))
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1, cex.main = 1)

## @knitr P2-garch_3
financial.garch11 <- garchFit(~ garch(1,1), data = resid(arima010.fit), 
                              trace = FALSE)
res.fcst <- predict(financial.garch11, n.ahead = 36, conf = .95)
# Compare the previous std. dev. with the (changing) new one
sd(resid(arima010.fit))
c(head(res.fcst$standardDeviation), tail(res.fcst$standardDeviation))
# Add the mean prediction of GARCH (close to zero) to the prediction of SARIMA
  # and subtract/add the previous CI / sigma * sigma_t
fcst.lower <- exp(arima010.fit.fcast$mean + res.fcst$meanForecast - 
                    c(arima010.fit.fcast$upper - arima010.fit.fcast$mean) / 
                    sd(resid(arima010.fit)) * res.fcst$standardDeviation)
fcst.upper <- exp(arima010.fit.fcast$mean + res.fcst$meanForecast + 
                    c(arima010.fit.fcast$upper - arima010.fit.fcast$mean) / 
                    sd(resid(arima010.fit)) * res.fcst$standardDeviation)

## @knitr P2-garch_4
plot(arima010.fit.fcast2, xlab = "Time period", 
     ylab = "Original and Forecasted Values", 
     main = paste0("36-step ahead Forecast and Original Series\n", 
                        "ARIMA(0,1,0)/GARCH(1,1) of log"))
polygon(c(time(arima010.fit.fcast2$mean), rev(time(arima010.fit.fcast2$mean))), 
        c(fcst.upper, rev(fcst.lower)), 
        col=rgb(0, 0, 0, 0.5), border = NA, ylim = c(0,105))
leg.txt <- c("Original series", "Forecasts (ARIMA(1,1,3)/GARCH(1,1))", 
             "95% Confidence Intervals")
legend("topleft", legend = leg.txt, lty = c(1, 1, 1), lwd = c(1, 1, 6), 
       col = c("black", "blue", "gray"), 
       bty = 'n', cex = 0.9)



## @knitr P3-load
# Lab 3 - Part 3 ----------------------------------------------------------
# setwd('./Lab3/data')
GW <- read.csv('globalWarming.csv', header = TRUE)
rbind(head(GW,4 ), tail(GW, 4))
GW$Date <- as.Date(as.character(GW$Date), '%m/%d/%y')
# Day of week of 1st observation
as.character(wday(GW$Date[1], label = TRUE, abbr = FALSE))
# Check that all observations correspond to same day of the week
all(wday(GW$Date, label = TRUE) == wday(GW$Date[1], label = TRUE))
# Check that all weeks between start and end dates appear in the dataset
identical(GW$Date, seq(min(GW$Date), max(GW$Date), by=7))
names(GW)[2] <- "DS"
summary(GW)
round(stat.desc(as.data.frame(GW$DS), desc = TRUE, norm = TRUE), 2)
# Create a time series object (weekly observations)
GW.ts <- ts(GW$DS, start = 2004 + day(min(GW$Date)) / 365.25, 
            freq = 365.25 / 7)

## @knitr P3-histogram
hist(GW$DS, breaks = 20, freq = FALSE, ylim = c(0, 4.5), 
     xlab = "Weekly level of interest in global warming in the news", 
     main = paste0("Histogram (and approximate density plot) of the weekly\n", 
                   "level of interest in global warming in the news"), 
     cex.main = 0.8)
lines(density(GW$DS), col = 'blue')

## @knitr P3-timeplot
# DS <- ts(GW$DS, start = 2004, freq = 52)
par(mar = c(5, 5, 4, 2) + 0.1)
plot(GW, type = 'l', xlab = "Year (time period: weeks)", 
     ylab = "Level of interest in global warming in the news", 
     main = paste0("Level of interest in global warming in the news\n", 
                   "from ", min(GW$Date), " to ", max(GW$Date)))
lines(GW$Date, stats::filter(GW$DS, sides=2, rep(1, 13)/13), lwd = 1.5, 
      col = rgb(0, 1, 0, 0.6))
leg.txt <- c("Original Series", "13-Point (~quarterly) Symmetric Moving Average")
legend("topleft", legend=leg.txt, lty = c(1, 1), col=c("black", "green"), 
       bty = 'n', cex = .8, merge = TRUE, bg = 336)
par(mar = c(5, 4, 4, 2) + 0.1)

## @knitr P3-ACF_PACF
par(mfrow = c(2, 1), cex.main = 0.9)
stats::acf(GW.ts, lag = 105, 
           main = paste0("Level of interest in global warming in the news\n", 
                         "from ", min(GW$Date), " to ", max(GW$Date)))
pacf(GW.ts, lag = 105, 
     main = paste0("Level of interest in global warming in the news\n", 
                   "from ", min(GW$Date), " to ", max(GW$Date)))
par(mfrow = c(1, 1), cex.main = 1)

## @knitr P3-decomposition_1
plot(decompose(GW.ts, type = 'additive'), col = 'blue', 
     xlab = "Year (time period: week)")

## @knitr P3-decomposition_2
plot(decompose(GW.ts, type = 'multiplicative'), col = 'blue', 
     xlab = "Year (time period: week)")

## @knitr P3-decomposition_3
(shock.position <- which(decompose(GW.ts, 
                                   type = 'multiplicative')[['random']] == 
                           max(decompose(GW.ts, 
                                         type = 'multiplicative')[['random']], 
                               na.rm = TRUE))) # 481
(shock.date <- GW$Date[shock.position]) # "2013-03-17"

## @knitr P3-timeplot_2
# DS <- ts(GW$DS, start = 2004, freq = 52)
par(mar = c(5, 5, 4, 2) + 0.1)
plot(GW[shock.position:length(GW$Date), ], type = 'l', 
     xlab = "Year (time period: weeks)", 
     ylab = "Level of interest in global warming in the news", 
     main = paste0("Level of interest in global warming in the news\n", 
                   "from ", shock.date, " to ", max(GW$Date)))
lines(GW$Date[shock.position:length(GW$Date)], 
      stats::filter(GW$DS[shock.position:length(GW$Date)], sides=2, 
                    rep(1, 13)/13), lwd = 1.5, col = rgb(0, 1, 0, 0.6))
leg.txt <- c("Original Series", "13-Point (~quarterly) Symmetric Moving Average")
legend("topleft", legend=leg.txt, lty = c(1, 1), col=c("black", "green"), 
       bty = 'n', cex = .8, merge = TRUE, bg = 336)
par(mar = c(5, 4, 4, 2) + 0.1)

## @knitr P3-Dataset_selection
# Whole dataset
GW.whole <- GW.ts
# Reduced dataset: last obsservations (from shock date)
GW.last <- window(GW.whole, start = year(shock.date) + (as.numeric(difftime(
  shock.date, as.Date(paste0(year(shock.date), "-1-1")))) + 1) / 365.25, 
  freq = 365.25/7)
# Fit the "best" ARIMA model for the whole dataset
(arima.whole.fit <- auto.arima(GW.whole, seasonal = TRUE))

## @knitr P3-Dataset_selection_2
# Fit the "best" ARIMA model for the reduced dataset
(arima.last.fit <- auto.arima(GW.last, seasonal = TRUE))
par(mfrow = c(2, 2), cex.main = 0.9)
stats::acf(resid(arima.whole.fit), lag = 53, 
           main = paste0("ACF of the residuals of SARIMA(0,1,1)(0,1,0)\n", 
                         "model fitted to data from ", min(GW$Date)))
pacf(resid(arima.whole.fit), lag = 53, 
     main = paste0("PACF of the residuals of SARIMA(0,1,1)(0,1,0)\n", 
                   "model fitted to data from ", shock.date))
stats::acf(resid(arima.last.fit), lag = 53, 
           main = paste0("ACF of the residuals of SARIMA(0,1,1)(0,1,0)\n", 
                         "model fitted to data from ", min(GW$Date)))
pacf(resid(arima.last.fit), lag = 53, 
     main = paste0("PACF of the residuals of SARIMA(0,1,1)(0,1,0)\n", 
                   "model fitted to data from ", shock.date))
par(mfrow = c(1, 1), cex.main = 1)


## @knitr P3-Dataset_selection_3
# Out-of-sample fit of both models
# Training sets (exclude last 15 observations, 10% of the reduced dataset)
GW.whole.train <- window(GW.whole, start = time(GW.whole)[1], 
                         end = time(GW.whole)[length(GW.whole)-15])
GW.last.train <- window(GW.last, start = time(GW.last)[1], 
                        end = time(GW.last)[length(GW.last)-15])
# Test set
GW.test <- window(GW.whole, start = time(GW.whole)[length(GW.whole)-15+1], 
                  end = time(GW.whole)[length(GW.whole)])
# Fit new models for the training sets using same coefficients
arima.whole.oos.fit <- Arima(GW.whole.train, 
                             order = arima.whole.fit$arma[c(1, 6, 2)], 
                             seas = list(order = arima.whole.fit$arma[c(3, 7, 
                                                                        4)], 
                                         freq = arima.whole.fit$arma[5]))
arima.last.oos.fit <- Arima(GW.last.train, 
                            order = arima.last.fit$arma[c(1, 6, 2)], 
                            seas = list(order = arima.last.fit$arma[c(3, 7, 
                                                                      4)], 
                                        freq = arima.last.fit$arma[5]))
# Predict next 15 observations based on each model
arima.whole.oos.fit.fcast <- forecast.Arima(arima.whole.oos.fit, h = 15)
arima.last.oos.fit.fcast <- forecast.Arima(arima.last.oos.fit, h = 15)

## @knitr P3-Dataset_selection_4
plot(arima.whole.oos.fit.fcast, ylim = c(-0.5, 4.5), 
     xlab = "Date", ylab = "Original, Esimated and Forecasted Values", 
     main = paste0("15-step out-of-sample Forecast and Original & Estimated ", 
                   "Series\nusing a SARIMA(", arima.whole.fit$arma[1], ",", 
                   arima.whole.fit$arma[6], ",", arima.whole.fit$arma[2], 
                   ")(", arima.whole.fit$arma[3], ",", arima.whole.fit$arma[7], 
                   ",", arima.whole.fit$arma[4], ")[", arima.whole.fit$arma[5], 
                   "] model from data from ", min(GW$Date)))
lines(fitted(arima.whole.oos.fit), col = 'blue', lty = 2)
par(new = TRUE)
plot(GW.whole, xaxt = 'n', xlab = '', ylab = '', ylim = c(-0.5, 4.5))
leg.txt <- c("Original series", "Esimated series (SARIMA(1,1,1)(0,1,1)[52])", 
             "Out-of-sample forecasts")
legend("topleft", legend = leg.txt, lty = c(1, 2, 1), 
       col = c("black", "blue", "blue"), 
       bty = 'n', cex = 0.9)

## @knitr P3-Dataset_selection_5
plot(arima.last.oos.fit.fcast, ylim = c(-0.5, 4.5), 
     xlab = "Date", ylab = "Original, Esimated and Forecasted Values", 
     main = paste0("15-step out-of-sample Forecast and Original & Estimated ", 
                   "Series\nusing a SARIMA(", arima.last.fit$arma[1], ",", 
                   arima.last.fit$arma[6], ",", arima.last.fit$arma[2], 
                   ")(", arima.last.fit$arma[3], ",", arima.last.fit$arma[7], 
                   ",", arima.last.fit$arma[4], ")[", arima.last.fit$arma[5], 
                   "] model from data from ", min(GW$Date)))
lines(fitted(arima.last.oos.fit), col = 'blue', lty = 2)
par(new = TRUE)
plot(GW.last, xaxt = 'n', xlab = '', ylab = '', ylim = c(-0.5, 4.5))
leg.txt <- c("Original series", "Esimated series (SARIMA(0,1,1)(0,1,0)[52])", 
             "Out-of-sample forecasts")
legend("topleft", legend = leg.txt, lty = c(1, 2, 1), 
       col = c("black", "blue", "blue"), 
       bty = 'n', cex = 0.9)

## @knitr P3-Dataset_selection_6
plot(GW.test, ylim = c(2, 4.5), xlab = "Date", 
     main = paste0("15-step out-of-sample Forecast (Detail)"), 
     ylab = "Original and Forecasted Values", lwd = 2)
par(new = TRUE)
plot(arima.last.oos.fit.fcast$mean, ylim = c(2, 4.5), , xaxt = 'n', xlab = '', 
     ylab = '', col = 'green', lwd = 2)
polygon(c(time(GW.test), rev(time(GW.test))), 
        c(arima.last.oos.fit.fcast$upper[,'95%'], 
          rev(arima.last.oos.fit.fcast$lower[,'95%'])), 
        col=rgb(0, 1, 0, 0.25), border = NA)
par(new = TRUE)
plot(arima.whole.oos.fit.fcast$mean, ylim = c(2, 4.5), xaxt = 'n', xlab = '', 
     ylab = '', col = 'red', lwd = 2)
polygon(c(time(GW.test), rev(time(GW.test))), 
        c(arima.whole.oos.fit.fcast$upper[,'95%'], 
          rev(arima.whole.oos.fit.fcast$lower[,'95%'])), 
        col=rgb(1, 0, 0, 0.25), border = NA)
leg.txt <- c("Original series", 
             paste0("Forecasts of model from data from ", shock.date, " on"), 
             paste0("Forecast of model from ", min(GW$Date), " on"))
legend("topleft", legend = leg.txt, lty = rep(1, 3), lwd = rep(1, 3), 
       col = c("black", "green", "red"), 
       bty = 'n', cex = 0.8)

## @knitr P3-conditional_var
par(mfrow = c(2,1))
acf(resid(arima.last.fit)^2, lag.max = 53, 
    main = "ACF of the squared residuals\nof the SARIMA(0,1,0)(0,1,0)model")
pacf(resid(arima.last.fit)^2, lag.max = 53, 
    main = "PACF of the squared residuals\nof the SARIMA(0,1,0)(0,1,0)model")
par(mfrow = c(1,1))

## @knitr P3-forecast
arima.last.fit.fcast <- forecast.Arima(arima.last.fit, h = 12)
plot(arima.last.fit.fcast, col = 'blue', ylim = c(-0.5, 5.5), 
     xlab = "Year (time period: month)", 
     main = paste0("12-step ahead Forecast and Original & Estimated ", 
                   "Series\n(SARIMA(0,1,1)(0,1,0)[52]"), 
     ylab="Original, Estimated, and Forecasted Values")
leg.txt <- c("Original series", "Esimated series (SARIMA(0,1,1)(0,1,0))", 
             "Forecasts")
legend("topleft", legend = leg.txt, lty = c(1, 2, 1), 
       col = c("black", "blue", "blue"), 
       bty = 'n', cex = 0.9)
lines(GW.last, col = "black")
lines(fitted(arima.last.fit), col = 'blue', lty = 2)



## @knitr P4-load
# Lab 3 - Part 4 ----------------------------------------------------------
# setwd('./Lab3/data')
load('gasOil.Rdata')
rbind(head(gasOil,4 ), tail(gasOil, 4))
gasOil$Date <- as.Date(as.character(gasOil$Date), '%Y-%m-%d')
summary(gasOil)
round(stat.desc(gasOil[, 2:3], desc = TRUE, norm = TRUE), 2)
# Check that all months between start and end dates appear in the dataset
identical(gasOil$Date, seq(min(gasOil$Date), max(gasOil$Date), by='month'))

## @knitr P4-timeseries
Production <- ts(data = gasOil$Production, start = year(gasOil$Date[1]), 
                 frequency = 12)
Price <- ts(data = gasOil$Price, start = year(gasOil$Date[1]), frequency = 12)

## @knitr P4-timeplot_prod
par(mar = c(5, 5, 4, 2) + 0.1)
plot(Production, xlab = "Year (time period: months)", 
     ylab = "U.S. oil production\n(in millions of barrels)", 
     main = paste0("U.S. oil production (in millions of barrels)\nfrom Jan. ", 
                  "1978 to Feb. 2012"))
lines(stats::filter(Production, sides=2, rep(1, 13)/13), lwd = 1.5, 
      col = "green")
leg.txt <- c("Original Series", "13-Point (~yearly) Symmetric Moving Average")
legend("bottomleft", legend=leg.txt, lty = c(1, 1), col=c("black", "green"), 
       bty = 'n', cex = .8, merge = TRUE, bg = 336)
par(mar = c(5, 4, 4, 2) + 0.1)

## @knitr P4-timeplot_price
par(mar = c(5, 5, 4, 2) + 0.1)
plot(Price, xlab = "Year (time period: months)", 
     ylab = "U.S. inflation-adjusted average\ngas prices (in dollars)", 
     main = paste0("U.S. inflation-adjusted average gas prices (in dollars)\n", 
                  "from Jan. 1978 to Feb. 2012"))
lines(stats::filter(Price, sides=2, rep(1, 13)/13), lwd = 1.5, 
      col = "green")
leg.txt <- c("Original Series", "13-Point (~yearly) Symmetric Moving Average")
legend("bottomleft", legend=leg.txt, lty = c(1, 1), col=c("black", "green"), 
       bty = 'n', cex = .8, merge = TRUE, bg = 336)
par(mar = c(5, 4, 4, 2) + 0.1)
# Production <- xts(gasOil$Production, 
#                   order.by = as.Date(as.character(gasOil$Date), '%Y-%m-%d'))
# Price <- xts(gasOil$Price, 
#              order.by = as.Date(as.character(gasOil$Date), '%Y-%m-%d'))
# par(mar = c(5, 5, 4, 2) + 0.1)
# par(mfrow = c(2, 1))
# plot(Production, xlab = "Month and Year (time period: months)", 
#      ylab = "U.S. oil production\n(in millions of barrels)", 
#      main = paste("U.S. oil production (in millions of barrels) from Jan.", 
#                   "1978 to Feb. 2012"))
# plot(Price, xlab = "Month and Year (time period = weeks)", 
#      ylab = "Inflation-adjusted average gas\nprices (in U.S. dollars)", 
#      main = paste("Inflation-adjusted average gas\nprices (in U.S. dollars)", 
#                   "from Jan. 1978 to Feb. 2012"))
# par(mar = c(5, 4, 4, 2) + 0.1)
# par(mfrow = c(1, 1))

## @knitr P4-timeplot_combined
par(mar = c(5, 4, 4, 5) + 0.1)
par(cex.main = 1, cex.lab = 0.9, cex.axis = 0.9)
plot(Production, col = 'blue', xlab = "Year (time period: month)", ylab = "", 
     main = paste0("U.S. oil production (in millions of barrels) and ", 
                   "inflation-adjusted\naverage gas prices (in dollars) ", 
                   "from Jan. 1978 to Feb. 2012"), 
     ylim = c(100, 300), lwd = 1)
axis(side = 2, col = "blue", col.axis = "blue", at = seq(100, 300, 25))
mtext("U.S. oil production (in millions of barrels)", side = 2, line = 2, 
      col = "blue", cex = 1)
leg.txt <- c("Production", "Price")
legend("bottomleft", legend = leg.txt, lty = 1, col = c("blue", "green"), 
       bty = 'n', cex = 0.8)
par(new = TRUE)
plot(Price, axes = FALSE, xlab = "", ylab = "", col = rgb(0, 1, 0), 
     ylim = c(1.25, 4.5), lty = 1, pch = 1, col.axis = "green", lwd = 1)
axis(side = 4, col = "green", col.axis = 'green')
mtext("U.S. inflation-adjusted average gas prices (in $)", side = 4, 
      line = 2, col = "green", cex = 1)
par(mar = c(5, 4, 4, 2) + 0.1)
par(cex.main = 1, cex.lab = 0.9, cex.axis = 0.9)

## @knitr P4-matrix
ggpairs(gasOil[, 2:3], title = paste("Scatterplot matrix of U.S. oil", 
                                     "production and inflation-adjusted", 
                                     "average gas prices"), 
        mapping = list(colour='red')) + 
  theme(plot.title = element_text(size=12))

## @knitr P4-correlation
# cor(gasOil$Production, gasOil$Price)
(ProdPrice.cor <- cor.test(Production, Price))

## @knitr P4-correlation_2
(linReg <- summary(lm(Price ~ Production)))
(ProdPrice.cor2 <- sqrt(linReg$r.squared))

## @knitr P4-ACF
par(mfrow = c(2, 2), cex.main = 0.9)
stats::acf(Production, lag = 24, 
           main = paste0("ACF of the U.S. oil production (in millions of\n", 
                 "barrels from Jan. 1978 to Feb 2012"))
pacf(Production, lag = 24, 
     main = paste0("PACF of the U.S. oil production (in millions of\nbarrels)", 
                  " from Jan. 1978 to Feb. 2012"))
stats::acf(Price, lag = 24, 
           main = paste0("ACF of the U.S. infl.-adj. average gas prices\n", 
                         "(in dollars) from Jan. 1978 to Feb. 2012"))
pacf(Price, lag = 24, 
     main = paste("PACF of the U.S. infl.-adj. average gas prices\n", 
                  "(in dollars) from Jan. 1978 to Feb. 2012"))
par(mfrow = c(1, 1), cex.main = 1)

## @knitr P4-unity_root
# Augmented Dickey-Fuller Test
adf.test(Production)
adf.test(Price)
# Phillips-Perron Unit Root Test
pp.test(Production)
pp.test(Price)

## @knitr P4-cointegration
po.test(gasOil[, 2:3])
# McLeod.Li.test

## @knitr P4-model_AIC_BIC
max_coef <- 3
orders <- data.frame(permutations(n = max_coef + 1, r = 3, v = 0:max_coef, 
                                  set = FALSE, repeats.allowed = TRUE))
dim(orders)[1] # Number of models up to max_coef
colnames(orders) <- c("p", "d", "q")
orders <- orders %>% dplyr::filter(d >= 1)
dim(orders)[1] # Number of models considered
orders %>% sample_n(10) # A 10-sample of the possible orders
model_list <- orders %>% rowwise() %>% 
  mutate(aic = try_default(AIC(Arima(Price, order = c(p, d, q))), default = NA, 
                           quiet = TRUE))
model_list <- model_list %>% dplyr::filter(!is.na(aic))
dim(model_list)[1] # Number of models estimated
model_list <- model_list %>% 
  mutate(bic = BIC(Arima(Price, order = c(p, d, q))))
kable(model_list %>% arrange(aic) %>% top_n(-5, aic), 
      digits = 1, caption = "Top 5 models  based on the (lowest) AIC value")
kable(model_list %>% arrange(bic) %>% top_n(-5, bic), 
      digits = 1, caption = "Top 5 models based on the (lowest) BIC value")

## @knitr P4-model_AIC_BIC_2
auto.arima(Price, seasonal = TRUE, ic = "aic") # same result using AICc
auto.arima(Price, seasonal = TRUE, ic = "bic")

## @knitr P4-model_candidates
orders_AIC <- model_list %>% arrange(aic) %>% top_n(-3, aic) %>% select(p, d, q)
orders_BIC <- model_list %>% arrange(bic) %>% top_n(-3, bic) %>% select(p, d, q)
orders <- rbind_list(orders_AIC, orders_BIC) %>% unique()
models <- apply(orders, 1, function(arima_order) 
  Arima(Price, order = c(arima_order[1], arima_order[2], arima_order[3])))

## @knitr P4-model_ACF_PACF
par(mfrow=c(5,2))
for (i in 1:5) {
  stats::acf(resid(models[[i]]), 
             main = paste0("ACF of the residuals of the\nARIMA(", orders[i, 1], 
                           ",", orders[i, 2], ",", orders[i, 3], ") model"))
  pacf(resid(models[[i]]), main = paste0("ACF of the residuals of the\nARIMA(", 
                                         orders[i, 1], ",", orders[i, 2], ",", 
                                         orders[i, 3], ") model"))
}
par(mfrow=c(1,1))

## @knitr P4-model_ACF_PACF_2
sum_acf <- function(model) {
  # Get the ACFs of first 24 lags
  ACF <- stats::acf(model$residuals, plot = FALSE, lag.max = 24)$acf
  # Exclude (assign 0) to those not significant
  significant_ACF <- ifelse(abs(ACF) < qnorm(.975) / sqrt(model$nobs), 0, 
                            abs(ACF))
  # Sum absolute values (exluding lag 0)
  return(sum(significant_ACF[-1]))
}
sum_pacf <- function(model) {
  # Get the PACFs of first 24 lags
  PACF <- pacf(model$residuals, plot = FALSE, lag.max = 24)$acf
  # Exclude (assign 0) to those not significant
  significant_PACF <- ifelse(abs(PACF) < qnorm(.975) / sqrt(model$nobs), 0, 
                             abs(PACF))
  # Sum absolute values
  return(sum(significant_PACF))
}
model_list <- join(orders, model_list, by=c("p","d","q"), type="inner") %>% 
  rowwise() %>% 
  mutate(ACF = sum_acf(Arima(Price, order = c(p, d, q))), 
         PACF = sum_pacf(Arima(Price, order = c(p, d, q))))
kable(model_list %>% arrange(ACF) %>% top_n(-5, ACF), digits = 1, 
      caption = paste0("Top 5 models based on the (lowest) sum of the ", 
                       "absolute value of their (significant) ", 
                       "auto-correlations"))

## @knitr P4-OOS_fit_ARIMA113
models <- models[c(1,4)]
orders <- orders[c(1,4), ]
Price.train <- window(Price, start = 1978, end=c(2008, 12))
Price.test <- window(Price, start = 2009)
(arima113.oos.fit <- Arima(Price.train, order = as.numeric(orders[1, ])))
kable(head(cbind("Time" = as.character(as.yearmon(time(Price.train), "%b %Y")), 
                 "Original series" = frmt(as.numeric(Price.train), 2), 
                 "Estimated series" = 
                   frmt(as.numeric(fitted(arima113.oos.fit)), 2), 
                 "Residuals" = frmt(as.numeric(arima113.oos.fit$resid), 2))), 
      row.names = FALSE, align = "r")
arima113.oos.fit.fcast <- forecast.Arima(arima113.oos.fit, h = 38)
kable(accuracy(arima113.oos.fit.fcast, Price.test)[, 1:7], 
      caption = paste("Goodness-of-fit parameters for the training and", 
                      "test sets (ARIMA(1,1,3)"))

## @knitr P4-OOS_fit_ARIMA113_2
plot(arima113.oos.fit.fcast, col = 'blue', ylim = c(-0.5, 4.5), 
     xlab = "Year (time period: month)", 
     main = paste0("38-step out-of-sample Forecast and Original & Estimated ", 
                   "Series\n(ARIMA(1,1,3)"), 
     ylab="Original, Estimated, and Forecasted Values")
leg.txt <- c("Original series", "Esimated series (ARIMA(1,1,3))", 
             "Out-of-sample forecasts")
legend("bottomleft", legend = leg.txt, lty = c(1, 2, 1), 
       col = c("black", "blue", "blue"), 
       bty = 'n', cex = 0.9)
lines(Price, col = "black")
lines(fitted(arima113.oos.fit), col = 'blue', lty = 2)

## @knitr P4-OOS_fit_ARIMA012
(arima012.oos.fit <- Arima(Price.train, order = as.numeric(orders[2, ])))
kable(head(cbind("Time" = as.character(as.yearmon(time(Price.train), "%b %Y")), 
                 "Original series" = frmt(as.numeric(Price.train), 2), 
                 "Estimated series" = 
                   frmt(as.numeric(fitted(arima012.oos.fit)), 2), 
                 "Residuals" = frmt(as.numeric(arima012.oos.fit$resid), 2))), 
      row.names = FALSE, align = "r")
arima012.oos.fit.fcast <- forecast.Arima(arima012.oos.fit, h = 38)
kable(accuracy(arima012.oos.fit.fcast, Price.test)[, 1:7], 
      caption = paste("Goodness-of-fit parameters for the training and", 
                      "test sets (ARIMA(0,1,2)"))

## @knitr P4-OOS_fit_ARIMA012_2
plot(arima012.oos.fit.fcast, col = 'blue', ylim = c(-0.5, 4.5), 
     xlab = "Year (time period: month)", 
     main = paste0("38-step out-of-sample Forecast and Original & Estimated ", 
                   "Series\n(ARIMA(0,1,2)"), 
     ylab="Original, Estimated, and Forecasted Values")
leg.txt <- c("Original series", "Esimated series (ARIMA(0,1,2))", 
             "Out-of-sample forecasts")
legend("bottomleft", legend = leg.txt, lty = c(1, 2, 1), 
       col = c("black", "blue", "blue"), 
       bty = 'n', cex = 0.9)
lines(Price, col = "black")
lines(fitted(arima012.oos.fit), col = 'blue', lty = 2)

## @knitr P4-ARIMA113_forecast
(arima113.fit <- models[[1]])
Parameters <- cbind(arima113.fit$coef, sqrt(diag(arima113.fit$var.coef)), 
                    matrix(sapply(c(-2,2), function(i) 
                      arima113.fit$coef + 
                        i * sqrt(diag(arima113.fit$var.coef))), ncol = 2))
colnames(Parameters) <- c("Coefficient", "SE", "95% CI lower", "95% CI upper")
kable(Parameters, digits = 4, 
      caption = "Coefficients, SEs, and 95% CIs of the estimated ARIMA(1,1,3) model")
arima113.fit.fcast <- forecast.Arima(arima113.fit, h = 58)
pander(predict(arima113.fit, n.ahead = 58)$pred)

## @knitr P4-ARIMA113_forecast_2
plot(arima113.fit.fcast, col = 'blue', ylim = c(1, 5.5), 
     xlab = "Year (time period: month)", 
     main = paste0("58-step ahead Forecast and Original & Estimated ", 
                   "Series\n(ARIMA(1,1,3)"), 
     ylab="Original, Estimated, and Forecasted Values")
leg.txt <- c("Original series", "Esimated series (ARIMA(1,1,3))", 
             "Forecasts")
legend("topleft", legend = leg.txt, lty = c(1, 2, 1), 
       col = c("black", "blue", "blue"), 
       bty = 'n', cex = 0.9)
lines(Price, col = "black")
lines(fitted(arima113.fit), col = 'blue', lty = 2)

## @knitr P4-GARCH_1
acf(resid(arima113.fit)^2, lag.max = 24, 
    main = paste0("ACF of the squared residuals of the\nARIMA(1,1,3) model ", 
                  "fitted to the U.S.\ninflation-adjusted average gas prices"))

## @knitr P4-GARCH_2
(Price.garch11 <- garch(resid(arima113.fit), trace = FALSE))
Price.garch11.res <- Price.garch11$res[-1]
t(confint(Price.garch11))

## @knitr P4-GARCH_3
par(mfrow = c(1, 2), cex.main = 0.9, par(mar = c(5, 4, 6, 2) + 0.1))
acf(Price.garch11.res, lag.max = 24, 
    main = paste0("ACF of the residuals and squared of an\nARIMA(1,1,3)/GARCH(1,1) model ", 
                  "fitted to the\nU.S. inflation-adjusted average gas prices"))
acf(Price.garch11.res^2, lag.max = 24, 
    main = paste0("ACF of the squared residuals and squared of an\nARIMA(1,1,3)/GARCH(1,1) model ", 
                  "fitted to the\nU.S. inflation-adjusted average gas prices"))
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1, cex.main = 1)

## @knitr P4-GARCH_4
ht <- Price.garch11$fit[,1]^2 # conditional variance
plot(ht, main = paste0("Estimated conditional variance of the\nARIMA(1,1,3)/", 
                       "GARCH(1,1) model fitted to the\nU.S. inflation-", 
                       "adjusted average gas prices"))

## @knitr P4-GARCH_5
res.CI.halfwidth <- qnorm(.975) * sqrt(ht) # CI of epsilon_t
# Variation of Price during observation period
Price.lower <- fitted.values(arima113.fit) - res.CI.halfwidth
Price.upper <- fitted.values(arima113.fit) + res.CI.halfwidth
# Forecasts
# Initialize h_t (cond. variance) and epsilon_t (residuals or error term)
  # 58 elements (as many as forecasts)
ht.fcst <- res.fcst <- rep(0, 58) 
for (i in 1:58) {
  if (i == 1) { # use last observation
    ht.fcst[i] <- Price.garch11$coef[1] + 
      Price.garch11$coef[2] * resid(arima113.fit)[length(Price)]^2 + 
      Price.garch11$coef[3] * ht[length(Price)]
  } else { # use previous predictions
    ht.fcst[i] <- Price.garch11$coef[1] + 
      Price.garch11$coef[2] * res.fcst[i-1]^2 + 
      Price.garch11$coef[3] * ht.fcst[i-1]
  }
  res.fcst[i] <- sqrt(ht.fcst[i]) # epsilon_t = omega_t * sqrt(h_t)
}
# Compare the previous std. dev. with the (changing) new one
sd(resid(arima113.fit))
c(head(sqrt(ht.fcst)), tail(sqrt(ht.fcst)))
# Lower & upper limits of the Price forecasts CI
Price.fcst.lower <- as.numeric(arima113.fit.fcast$mean) - 
  c(arima113.fit.fcast$upper[, '95%'] - arima113.fit.fcast$mean) / 
  sd(resid(arima113.fit.fcast)) * sqrt(ht.fcst)
Price.fcst.upper <- as.numeric(arima113.fit.fcast$mean) + 
  c(arima113.fit.fcast$upper[, '95%'] - arima113.fit.fcast$mean) / 
  sd(resid(arima113.fit.fcast)) * sqrt(ht.fcst)

## @knitr P4-GARCH_6
plot(arima113.fit.fcast, ylim = c(1, 6), 
     xlab = "Year (time period: month)", 
     main = paste0("58-step ahead Forecast and Original Series\n", 
                   "with confidence intervals (ARIMA(1,1,3)/GARCH(1,1)"), 
     ylab = "Original and Forecasted Values")
polygon(c(time(Price), rev(time(Price))), 
        c(Price.upper, rev(Price.lower)), col=rgb(0, 0, 0, 0.25), border = NA)
polygon(c(time(arima113.fit.fcast$mean), 
          rev(time(arima113.fit.fcast$mean))), 
        c(Price.fcst.upper, rev(Price.fcst.lower)), col=rgb(0, 0, 0, 0.25), 
        border = NA)
leg.txt <- c("Original series", "Forecasts (ARIMA(1,1,3)/GARCH(1,1))", 
             "95% Confidence Intervals")
legend("topleft", legend = leg.txt, lty = c(1, 1, 1), lwd = c(1, 1, 6), 
       col = c("black", "blue", "gray"), 
       bty = 'n', cex = 0.9)

## @knitr P4-GARCH_7
(Price.garch11.2 <- garchFit(~ garch(1,1), data = resid(arima113.fit), 
                             trace = FALSE))
# res.fcst2 <- predict(Price.garch11.2, n.ahead=58, plot = TRUE, conf = .95)
res.fcst.2 <- predict(Price.garch11.2, n.ahead=58, conf = .95)
Price.fcst.lower.2 <- arima113.fit.fcast$mean + res.fcst.2$meanForecast - 
  c(arima113.fit.fcast$upper[, '95%'] - arima113.fit.fcast$mean) / 
  sd(resid(arima113.fit.fcast)) * res.fcst.2$standardDeviation
Price.fcst.upper.2 <- arima113.fit.fcast$mean + res.fcst.2$meanForecast + 
  c(arima113.fit.fcast$upper[, '95%'] - arima113.fit.fcast$mean) / 
  sd(resid(arima113.fit.fcast)) * res.fcst.2$standardDeviation


## @knitr P4-GARCH_8
plot(arima113.fit.fcast, ylim = c(1, 6), 
     xlab = "Year (time period: month)", 
     main = paste0("58-step ahead Forecast and Original Series\n", 
                   "with confidence intervals (ARIMA(1,1,3)/GARCH(1,1)"), 
     ylab = "Original and Forecasted Values")
polygon(c(time(Price), rev(time(Price))), 
        c(Price.upper, rev(Price.lower)), col=rgb(0, 0, 0, 0.25), border = NA)
polygon(c(time(arima113.fit.fcast$mean), 
          rev(time(arima113.fit.fcast$mean))), 
        c(Price.fcst.upper.2, rev(Price.fcst.lower.2)), col=rgb(0, 0, 0, 0.25), 
        border = NA)
leg.txt <- c("Original series", "Forecasts (ARIMA(1,1,3)/GARCH(1,1))", 
             "95% Confidence Intervals")
legend("topleft", legend = leg.txt, lty = c(1, 1, 1), lwd = c(1, 1, 6), 
       col = c("black", "blue", "gray"), 
       bty = 'n', cex = 0.9)

## @knitr P4-GARCH_9
(Price.garch11.3 <- garchFit(~ arma(1,3) + garch(1,1), data = diff(Price), 
                             trace = FALSE))
res.fcst.3 <- predict(Price.garch11.3, n.ahead=58, conf = .95)
# Add the mean prediction of GARCH (close to zero) to the prediction of SARIMA
  # and subtract/add the previous CI / sigma * sigma_t
Price.fcst.lower.3 <- arima113.fit.fcast$mean + res.fcst.3$meanForecast - 
  c(arima113.fit.fcast$upper[, '95%'] - arima113.fit.fcast$mean) / 
  sd(resid(arima113.fit.fcast)) * res.fcst.3$standardDeviation
Price.fcst.upper.3 <- arima113.fit.fcast$mean + res.fcst.3$meanForecast + 
  c(arima113.fit.fcast$upper[, '95%'] - arima113.fit.fcast$mean) / 
  sd(resid(arima113.fit.fcast)) * res.fcst.3$standardDeviation

## @knitr P4-GARCH_10
plot(arima113.fit.fcast, ylim = c(1, 6), 
     xlab = "Year (time period: month)", 
     main = paste0("58-step ahead Forecast and Original Series\n", 
                   "with confidence intervals (ARIMA(1,1,3)/GARCH(1,1)"), 
     ylab = "Original and Forecasted Values")
polygon(c(time(Price), rev(time(Price))), 
        c(Price.upper, rev(Price.lower)), col=rgb(0, 0, 0, 0.25), border = NA)
polygon(c(time(arima113.fit.fcast$mean), 
          rev(time(arima113.fit.fcast$mean))), 
        c(Price.fcst.upper.3, rev(Price.fcst.lower.3)), col=rgb(0, 0, 0, 0.25), 
        border = NA)
leg.txt <- c("Original series", "Forecasts (ARIMA(1,1,3)/GARCH(1,1))", 
             "95% Confidence Intervals")
legend("topleft", legend = leg.txt, lty = c(1, 1, 1), lwd = c(1, 1, 6), 
       col = c("black", "blue", "gray"), 
       bty = 'n', cex = 0.9)

## @knitr P4-GARCH_11
head(cbind(sqrt(ht.fcst), res.fcst.2$standardDeviation, 
           res.fcst.3$standardDeviation))
plot(sqrt(ht.fcst), ylim=c(0,.2), type = 'l', col = 'red', 
     xlab = "Steps ahead", ylab = expression(h[t]), 
     main=expression(paste("Standard deviation (", h[t], ") of the forecasts")))
lines(res.fcst.2$standardDeviation, col = 'blue')
lines(res.fcst.3$standardDeviation, col = 'green')
leg.txt <- c("GARCH(1,1) using own code on residuals of ARIMA(1,1,3)", 
             "GARCH(1,1) using fGarch on residuals of ARIMA(1,1,3)", 
             "ARMA(1,3)/GARCH(1,1) using fGarch on differenced Price series")
legend("bottomright", legend = leg.txt, lty = c(1, 1, 1), lwd = c(1, 1, 1), 
       col = c("red", "blue", "green"), bty = 'n', cex = 0.9)

