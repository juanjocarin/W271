## MIDS W271-4 Lab3           ##
## Carin, Davis, Levato, Song ##


## @knitr Libraries-Functions-Constants
# LIBRARIES, FUNCTIONS AND CONSTANTS -------------------------------------------------
# Load libraries
# library(e1071)
library(gtools)
library(ggplot2)
library(ggfortify)
library(scatterplot3d)
library(scales)
library(knitr)
library(pastecs)
library(car)
# library(sandwich)
# library(lmtest)
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
library(TSA)

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
# Loading the Data --------------------------------------------------------
# setwd('./Lab3/data')
houseValue <- read.csv('houseValueData.csv', header = TRUE)
head(houseValue)

## @knitr P2-load
# Loading the Data --------------------------------------------------------
# setwd('./Lab3/data')
financial <- read.csv('lab3_series02.csv', header = TRUE)
head(financial)
# Check if 1st column is just an incremental index
all(financial$X == 1:dim(financial)[1])
financial <- financial[, -1]
head(financial)


## @knitr P3-load
# Loading the Data --------------------------------------------------------
# setwd('./Lab3/data')
globalWarming <- read.csv('globalWarming.csv', header = TRUE)
head(globalWarming)


## @knitr P4-load
# Loading the Data --------------------------------------------------------
# setwd('./Lab3/data')
load('gasOil.Rdata')
rbind(head(gasOil,4 ), tail(gasOil, 4))
summary(gasOil)

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
leg.txt <- c("Original Series", "13-Point Symmetric Moving Average")
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
leg.txt <- c("Original Series", "13-Point Symmetric Moving Average")
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
arima113.fit.fcast <- forecast.Arima(arima113.fit, h = 58)
pander(predict(arima113.fit, n.ahead = 58)$pred)

## @knitr P4-ARIMA113_forecast_2
plot(arima113.fit.fcast, col = 'blue', ylim = c(1, 5.5), 
     xlab = "Year (time period: month)", 
     main = paste0("58-step ahead Forecast and Original & Estimated ", 
                   "Series\n(ARIMA(1,1,3)"), 
     ylab="Original, Estimated, and Forecasted Values")
leg.txt <- c("Original series", "Esimated series (ARIMA(1,1,3))", 
             "Out-of-sample forecasts")
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
Price.garch11 <- garch(resid(arima113.fit), trace = FALSE)
Price.garch11.res <- Price.garch11$res[-1]
t(confint(Price.garch11))

## @knitr P4-GARCH_3
acf(Price.garch11.res^2, lag.max = 24, 
    main = paste0("ACF of the residuals of an\nARIMA(1,1,3)/GARCH(1,1) model ", 
                  "fitted to the\nU.S. inflation-adjusted average gas prices"))

## @knitr P4-GARCH_4
ht <- Price.garch11$fit[,1]^2
plot(ht, main = paste0("Estimated conditional variance of the\nARIMA(1,1,3)/", 
                       "GARCH(1,1) model fitted to the\nU.S. inflation-", 
                       "adjusted average gas prices"))

## @knitr P4-GARCH_5
low <- fitted.values(arima113.fit) - qnorm(.975) * sqrt(ht)
high <- fitted.values(arima113.fit) + qnorm(.975) * sqrt(ht)
new.ht <- new.res <- new.low <- new.high <- rep(0, 58)
for (i in 1:58) {
  if (i == 1) {
    new.ht[i] <- Price.garch11$coef[1] + 
      Price.garch11$coef[2] * resid(arima113.fit)[length(Price)]^2 + 
      Price.garch11$coef[3] * ht[length(Price)]
  } else {
    new.ht[i] <- Price.garch11$coef[1] + 
      Price.garch11$coef[2] * new.res[i-1]^2 + 
      Price.garch11$coef[3] * new.ht[i-1]
  }
  new.res[i] <- rnorm(1) * sqrt(new.ht[i])
  new.low[i] <- as.numeric(forecast(arima113.fit, 58)$mean)[i] - 
    qnorm(.975) * sqrt(new.ht[i])
  new.high[i] <- as.numeric(forecast(arima113.fit, 58)$mean)[i] + 
    qnorm(.975) * sqrt(new.ht[i])
}

## @knitr P4-GARCH_6
plot(arima113.fit.fcast, ylim = c(1, 5), ylab = "", 
     main = "Forecasts from ARIMA(1,1,3)/GARCH(1,1)")
polygon(c(time(Price), rev(time(Price))), 
        c(high, rev(low)), col=rgb(0, 0, 0, 0.25), border = NA)
polygon(c(time(arima113.fit.fcast$mean), 
          rev(time(arima113.fit.fcast$mean))), 
        c(new.high, rev(new.low)), col=rgb(0, 0, 0, 0.25), border = NA)

## @knitr hola

plot(rbind(Price, arima113.fit.fcast$mean), ylim = c(0, 5), ylab = "")

plot(arima113.fit.fcast)

# https://talksonmarkets.files.wordpress.com/2012/09/time-series-analysis-with-arima-e28093-arch013.pdf
plot(Price, ylim = c(0, 5), ylab = "")
polygon(c(time(Price), rev(time(Price))), 
        c(high, rev(low)), col=rgb(0, 0, 0, 0.25), border = NA)


plot(Price, ylim = c(0, 5), ylab = "")
polygon(c(time(Price), rev(time(Price))), 
        c(high, rev(low)), col=rgb(0, 0, 0, 0.25), border = NA)


archres=resid(arima113.fit)/sqrt(ht.arch08)
qqnorm(archres,main='ARIMA-ARCH Residuals')
qqline(archres)

## @knitr ex1-desc_stats
# Exploratory Data Analysis -----------------------------------------------
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
lines(stats::filter(hw08.ts, sides=2, rep(1, 7)/7), lty = 1, lwd = 1.5, 
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



## @knitr ex1-models_analysis
# Candidate Models --------------------------------------------------------
max_coef <- 3
orders <- data.frame(permutations(n = max_coef + 1, r = 3, v = 0:max_coef, 
                                  set = FALSE, repeats.allowed = TRUE))
dim(orders)[1] # Number of models up to max_coef
colnames(orders) <- c("p", "d", "q")
orders <- orders %>% dplyr::filter(d >= 1)
dim(orders)[1] # Number of models considered
orders %>% sample_n(10) # A 10-sample of the possible orders
aic_list <- orders %>% rowwise() %>% 
  mutate(aic = try_default(AIC(Arima(Price, order = c(p, d, q))), default = NA, 
                           quiet = TRUE))
aic_list <- aic_list %>% dplyr::filter(!is.na(aic))
dim(aic_list)[1] # Number of models estimated
aic_list <- aic_list %>% 
  mutate(bic = BIC(Arima(Price, order = c(p, d, q))))
kable(aic_list %>% arrange(aic) %>% top_n(-5, aic), 
      digits = 1, caption = "Top 5 models  based on their AIC value")
kable(aic_list %>% arrange(bic) %>% top_n(-5, bic), 
      digits = 1, caption = "Top 5 models based on their BIC value")

## @knitr ex1-models_analysis-2
par(mfrow = c(1, 2))
boxplot(aic_list$aic ~ aic_list$family, xlab = "Model family", ylab = "AIC", 
        main = "Boxplot of the AIC value per model family")
boxplot(aic_list$bic ~ aic_list$family, xlab = "Model family", ylab = "BIC", 
        main = "Boxplot of the BIC value per model family")
par(mfrow = c(1, 1))


## @knitr ex1-models_analysis-3
par(mar = c(5, 4, 4, 5) + 0.1)
par(mfrow = c(2, 2))
par(mar = c(5, 4, 4, 2) + 0.1)
par(mfrow = c(1, 1))

par(mar = c(5, 4, 4, 5) + 0.1)
par(mfrow = c(2, 2))
plot(aic_list %>% filter(family == "AR") %>% select(p, bic), col = "blue", 
     main = "BIC of AR(p) model vs. p", ylab = "BIC", type = "o", lty = 2, 
     pch = 1)
plot(aic_list %>% filter(family == "MA") %>% select(q, bic), col = "blue", 
     main = "BIC of MA(q) model vs. q", ylab = "BIC", type = "o", lty = 2, 
     pch = 1)
par(mar = c(5, 4, 4, 2) + 0.1)
par(mfrow = c(1, 1))


## @knitr ex1-models_analysis-4
aic_plot <- data.frame(aic_list %>% select(p, q, aic))
scatterplot3d(aic_plot, pch = 4, lwd = 3, type = "h", 
              color = grey(dim(aic_list)[1]:1 / 
                             (ceiling(dim(aic_plot)[1] / 15) * 15)), 
              label.tick.marks = TRUE, lty.axis = 2, angle = 60, 
              main = "AIC of the models depending on the order (p and q)")

## @knitr ex1-models_analysis-5
aic_plot <- data.frame(aic_list %>% filter(family == "ARMA") %>% 
                         select(p, q, aic))
scatterplot3d(aic_plot, pch = 4, lwd = 3, type = "h", 
              color = grey(dim(aic_plot)[1]:1 / 
                             (ceiling(dim(aic_plot)[1] / 15) * 15)), 
              label.tick.marks = TRUE, lty.axis = 2, angle = 75, 
              main = "AIC of the ARMA models depending on the order (p and q)")


## @knitr ex1-models_analysis-6
kable(aic_list %>% arrange(aic) %>% group_by(family) %>% top_n(-4, aic), 
      digits = 1, caption = "Top 4 models per family, based on their AIC value")
kable(aic_list %>% arrange(bic) %>% group_by(family) %>% top_n(-4, bic), 
      digits = 1, caption = "Top 4 models per family, based on their BIC value")


## @knitr ex1-models_analysis-7
sum_acf <- function(model) {
  # Get the ACFs of first 24 lags
  ACF <- acf(model$residuals, plot = FALSE, lag.max = 24)$acf
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
aic_list <- aic_list %>% 
  mutate(ACF = sum_acf(Arima(hw08, order = c(p, 0, q))), 
         PACF = sum_pacf(Arima(hw08, order = c(p, 0, q)))) %>% 
  select(p, q, aic, bic, ACF, PACF, family)
kable(aic_list %>% arrange(ACF) %>% top_n(-5, ACF), digits = 1, 
      caption = paste0("Top 5 models based on the sum of the absolute value ", 
                       "of their (significant) auto-correlations"))

## @knitr ex1-models_analysis-7-2
kable(aic_list %>% arrange(PACF) %>% top_n(-5, PACF), digits = 1, 
      caption = paste0("Top 5 models based on the sum of the absolute value ", 
                       "of their (significant) partial auto-correlations"))


## @knitr ex1-models_analysis-8
ar_models_coefs <- data.frame(aic_list %>% arrange(aic) %>% 
                                filter(family == "AR") %>% 
                                top_n(-2, aic))[, 1]
ma_models_coefs <- data.frame(aic_list %>% arrange(bic) %>% 
                                filter(family == "MA") %>% 
                                top_n(-2, bic))[, 2]
arma_models_coefs <- data.frame(aic_list %>% arrange(bic) %>% 
                                  filter(family == "ARMA") %>% 
                                  top_n(-4, bic))[, 1:2] %>% 
  rbind(data.frame(aic_list %>% arrange(ACF))[1, 1:2]) %>% 
  rbind(data.frame(aic_list %>% arrange(PACF))[1, 1:2])
ar_models <- lapply(ar_models_coefs, function(p) 
  Arima(hw08.ts, order = c(p, 0, 0)))
ma_models <- lapply(ma_models_coefs, function(q) 
  Arima(hw08.ts, order = c(0, 0, q)))
arma_models <- apply(arma_models_coefs, 1, function(arma_coef) 
  Arima(hw08.ts, order = c(arma_coef[1], 0, arma_coef[2])))

## @knitr ex1-AR_models-3
# AR Models ----------------------------------------------------------------
ar3 <- ar_models[[2]]
Parameters <- cbind(ar3$coef, sqrt(diag(ar3$var.coef)), 
                    matrix(sapply(c(-2,2), function(i) 
                      ar3$coef + i * sqrt(diag(ar3$var.coef))), ncol = 2))
colnames(Parameters) <- c("Coefficient", "SE", "95% CI lower", "95% CI upper")
kable(Parameters, digits = 4, 
      caption = "Coefficients, SEs, and 95% CIs of the estimated AR(3) model")

## @knitr ex1-AR_models-3-2
(roots_ar <- polyroot(c(1, -ar3$coef[1:(length(ar3$coef)-1)])))
all(Mod(roots_ar) > 1) # Stationarity condition

## @knitr ex1-AR_models-9
ar9 <- ar_models[[1]]
Parameters <- cbind(ar9$coef, sqrt(diag(ar9$var.coef)), 
                    matrix(sapply(c(-2,2), function(i) 
                      ar9$coef + i * sqrt(diag(ar9$var.coef))), ncol = 2))
colnames(Parameters) <- c("Coefficient", "SE", "95% CI lower", "95% CI upper")
kable(Parameters, digits = 4, 
      caption = "Coefficients, SEs, and 95% CIs of the estimated AR(9) model")

## @knitr ex1-AR_models-9-2
(roots_ar <- polyroot(c(1, -ar9$coef[1:(length(ar9$coef)-1)])))
all(Mod(roots_ar) > 1) # Stationarity condition


## @knitr ex1-AR-3_res_plots
summary(ar9$resid)
par(mfrow = c(2, 2))
par(cex.main = 1, cex.lab = 0.9, cex.axis = 0.9)
plot(ar3$resid, main = "Residual Series of\nthe AR(3) model", col="blue", 
     xlab = "Year (time period: month)", ylab = "Residual level")
hist(ar3$resid, col = "gray", breaks = 20, xlab = "Residual level", 
     main = "Histogram of the residuals\n(AR(3) model")
acf(ar3$resid, main = "ACF of the Residual\nSeries (AR(3) model)")
pacf(ar3$resid, main = "PACF of the Residual\nSeries (AR(3) model)")
par(mfrow = c(1, 1))
par(cex.main = 1, cex.lab = 0.9, cex.axis = 0.9)

## @knitr ex1-AR-3_boxtest
Box.test(ar3$resid, type = "Ljung-Box")


## @knitr ex1-AR-9_res_plots
summary(ar9$resid)
par(mfrow = c(2, 2))
par(cex.main = 1, cex.lab = 0.9, cex.axis = 0.9)
plot(ar9$resid, main = "Residual Series of\nthe AR(9) model", col="blue", 
     xlab = "Year (time period: month)", ylab = "Residual level")
hist(ar9$resid, col = "gray", breaks = 20, xlab = "Residual level", 
     main = "Histogram of the residuals\n(AR(9) model")
acf(ar9$resid, main = "ACF of the Residual\nSeries (AR(9) model)")
pacf(ar9$resid, main = "PACF of the Residual\nSeries (AR(9) model)")
par(mfrow = c(1, 1))
par(cex.main = 1, cex.lab = 0.9, cex.axis = 0.9)

## @knitr ex1-AR-9_boxtest
Box.test(ar9$resid, type = "Ljung-Box")


## @knitr ex1-AR-3_in-sample-fit
kable(tail(cbind("Time" = as.character(as.yearmon(time(hw08.ts), "%b %Y")), 
                 "Original series" = frmt(as.numeric(hw08.ts), 1), 
                 "Estimated series" = frmt(as.numeric(fitted(ar3)), 1), 
                 "Residuals" = frmt(as.numeric(ar3$resid), 1))), 
      row.names = FALSE, align = "r")
par(mar = c(5, 4, 4, 5) + 0.1)
par(cex.main = 1, cex.lab = 0.9, cex.axis = 0.9)
plot(hw08.ts, xlab = "Year (time period: month)", 
     main = "Original vs. an AR(3) Estimated Series with Residuals",
     ylab = "Original and Estimated Values", 
     ylim = c(40, 160), lwd = 0.5)
leg.txt <- c("Original Series", "Estimated Series", "Residuals")
legend("topleft", legend = leg.txt, lty = 1, col = c("black", "blue", "green"), 
       bty = 'n', cex = 0.9)
lines(fitted(ar3), col = "blue", lwd = 0.5)
par(new = TRUE)
plot.ts(ar3$resid, axes = FALSE, xlab = "", ylab = "", 
        col = rgb(0, 1, 0, 0.5), ylim = c(-12, 12), lty = 1, pch = 1, 
        col.axis = "green", lwd = 0.5)
axis(side = 4, col = "green")
mtext("Residuals", side = 4, line = 2, col = "green")
par(mar = c(5, 4, 4, 2) + 0.1)
par(cex.main = 1, cex.lab = 0.9, cex.axis = 0.9)


## @knitr ex1-AR-9_in-sample-fit
kable(tail(cbind("Time" = as.character(as.yearmon(time(hw08.ts), "%b %Y")), 
                 "Original series" = frmt(as.numeric(hw08.ts), 1), 
                 "Estimated series" = frmt(as.numeric(fitted(ar9)), 1), 
                 "Residuals" = frmt(as.numeric(ar9$resid), 1))), 
      row.names = FALSE, align = "r")
par(mar = c(5, 4, 4, 5) + 0.1)
par(cex.main = 1, cex.lab = 0.9, cex.axis = 0.9)
plot(hw08.ts, xlab = "Year (time period: month)", 
     main = "Original vs. an AR(9) Estimated Series with Residuals",
     ylab = "Original and Estimated Values", 
     ylim = c(40, 160), lwd = 0.5)
leg.txt <- c("Original Series", "Estimated Series", "Residuals")
legend("topleft", legend = leg.txt, lty = 1, col = c("black", "blue", "green"), 
       bty = 'n', cex = 0.9)
lines(fitted(ar9), col = "blue", lwd = 0.5)
par(new = TRUE)
plot.ts(ar9$resid, axes = FALSE, xlab = "", ylab = "", 
        col = rgb(0, 1, 0, 0.5), ylim = c(-12, 12), lty = 1, pch = 1, 
        col.axis = "green", lwd = 0.5)
axis(side = 4, col = "green")
mtext("Residuals", side = 4, line = 2, col = "green")
par(mar = c(5, 4, 4, 2) + 0.1)
par(cex.main = 1, cex.lab = 0.9, cex.axis = 0.9)


## @knitr ex1-AR-3_out-of-sample-fit
hw08.ts_train <- window(hw08.ts, start = 1980, end=c(2007,12))
hw08.ts_test <- window(hw08.ts, start = 2008)
(ar3.oos.fit <- Arima(hw08.ts_train, order = c(ar_models_coefs[2], 0, 0)))
kable(head(cbind("Time" = as.character(as.yearmon(time(hw08.ts_train), 
                                                  "%b %Y")), 
                 "Original series" = frmt(as.numeric(hw08.ts_train), 1), 
                 "Estimated series" = frmt(as.numeric(fitted(ar3.oos.fit)), 1), 
                 "Residuals" = frmt(as.numeric(ar3.oos.fit$resid), 1))), 
      row.names = FALSE, align = "r")
# Forecast / Backtesting
ar3.oos.fit.fcast <- forecast.Arima(ar3.oos.fit, h = 36)
(acc_ar3 <- accuracy(ar3.oos.fit.fcast, hw08.ts_test))

## @knitr ex1-AR-3_out-of-sample-fit-2
plot(ar3.oos.fit.fcast, col = 'blue', ylim = c(40, 160), 
     xlab = "Year (time period: month)", 
     main = "Original vs. an AR(3) model Forecasts",
     ylab = "Original and Forecasted Values")
leg.txt <- c("Original Series", "Forecasts from AR(3) model")
legend("topleft", legend = leg.txt, lty = 1, col = c("black", "blue"), 
       bty = 'n', cex = 0.9)
lines(hw08.ts, col = "black")


## @knitr ex1-AR-9_out-of-sample-fit
(ar9.oos.fit <- Arima(hw08.ts_train, order = c(ar_models_coefs[1], 0, 0)))
ar9.oos.fit.fcast <- forecast.Arima(ar9.oos.fit, h = 36)
(acc_ar9 <- accuracy(ar9.oos.fit.fcast, hw08.ts_test))

## @knitr ex1-AR-9_out-of-sample-fit-2
plot(ar9.oos.fit.fcast, col = 'blue', ylim = c(40, 160), 
     xlab = "Year (time period: month)", 
     main = "Original vs. an AR(9) model Forecasts",
     ylab = "Original and Forecasted Values")
leg.txt <- c("Original Series", "Forecasts from AR(9) model")
legend("topleft", legend = leg.txt, lty = 1, col = c("black", "blue"), 
       bty = 'n', cex = 0.9)
lines(hw08.ts, col = "black")


## @knitr ex1-MA_models-12
# MA Models ----------------------------------------------------------------
ma12 <- ma_models[[1]]
Parameters <- cbind(ma12$coef, sqrt(diag(ma12$var.coef)), 
                    matrix(sapply(c(-2,2), function(i) 
                      ma12$coef + i * sqrt(diag(ma12$var.coef))), ncol = 2))
colnames(Parameters) <- c("Coefficient", "SE", "95% CI lower", "95% CI upper")
kable(Parameters, digits = 4, 
      caption = "Coefficients, SEs, and 95% CIs of the estimated MA(12) model")


## @knitr ex1-MA_models-12-2
(roots_ma <- polyroot(c(1, ma12$coef[1:(length(ma12$coef)-1)])))
all(Mod(roots_ma) > 1) # Invertibility condition


## @knitr ex1-MA_models-10
ma10 <- ma_models[[2]]
Parameters <- cbind(ma10$coef, sqrt(diag(ma10$var.coef)), 
                    matrix(sapply(c(-2,2), function(i) 
                      ma10$coef + i * sqrt(diag(ma10$var.coef))), ncol = 2))
colnames(Parameters) <- c("Coefficient", "SE", "95% CI lower", "95% CI upper")
kable(Parameters, digits = 4, 
      caption = "Coefficients, SEs, and 95% CIs of the estimated MA(10) model")


## @knitr ex1-MA_models-10-2
(roots_ma <- polyroot(c(1, -ma10$coef[1:(length(ma10$coef)-1)])))
all(Mod(roots_ma) > 1) # Stationarity condition


## @knitr ex1-MA-12_res_plots
summary(ma12$resid)
par(mfrow=c(2, 2))
par(cex.main = 1, cex.lab = 0.9, cex.axis = 0.9)
plot(ma12$resid, main = "Residual Series of\nthe MA(12) model", col="blue", 
     xlab = "Year (time period: month)", ylab = "Residual level")
hist(ma12$resid, col = "gray", breaks = 20, xlab = "Residual level", 
     main = "Histogram of the residuals\n(MA(12) model")
acf(ma12$resid, main = "ACF of the Residual\nSeries (MA(12) model)")
pacf(ma12$resid, main = "PACF of the Residual\nSeries (MA(12) model)")
par(mfrow=c(1, 1))
par(cex.main = 1, cex.lab = 0.9, cex.axis = 0.9)

## @knitr ex1-MA-12_boxtest
Box.test(ma12$resid, type = "Ljung-Box")


## @knitr ex1-MA-10_res_plots
summary(ma10$resid)
par(mfrow=c(2, 2))
par(cex.main = 1, cex.lab = 0.9, cex.axis = 0.9)
plot(ma10$resid, main = "Residual Series of\nthe MA(10) model", col="blue", 
     xlab = "Year (time period: month)", ylab = "Residual level")
hist(ma10$resid, col = "gray", breaks = 20, xlab = "Residual level", 
     main = "Histogram of the residuals\n(MA(10) model")
acf(ma10$resid, main = "ACF of the Residual\nSeries (MA(10) model)")
pacf(ma10$resid, main = "PACF of the Residual\nSeries (MA(10) model)")
par(mfrow=c(1, 1))
par(cex.main = 1, cex.lab = 0.9, cex.axis = 0.9)

## @knitr ex1-MA-10_boxtest
Box.test(ma10$resid, type = "Ljung-Box")


## @knitr ex1-MA-12_in-sample-fit
kable(head(cbind("Time" = as.character(as.yearmon(time(hw08.ts), "%b %Y")),
                 "Original series" = frmt(as.numeric(hw08.ts), 1), 
                 "Estimated series" = frmt(as.numeric(fitted(ma12)), 1),
                 "Residuals" = frmt(as.numeric(ma12$resid), 1))),
      row.names = FALSE, align = "r")
par(mar = c(5, 4, 4, 5) + 0.1)
par(cex.main = 1, cex.lab = 0.9, cex.axis = 0.9)
plot(hw08.ts, xlab = "Year (time period: month)", 
     main = "Original vs. a MA(12) Estimated Series with Residuals",
     ylab = "Original and Estimated Values",
     ylim = c(40, 160), lwd = 0.5)
leg.txt <- c("Original Series", "Estimated Series", "Residuals")
legend("topleft", legend = leg.txt, lty = 1, col = c("black", "blue", "green"), 
       bty = 'n', cex = 0.9)
lines(fitted(ma12), col = "blue", lwd = 0.5)
par(new = TRUE)
plot.ts(ma12$resid, axes = FALSE, xlab = "", ylab = "", 
        col = rgb(0, 1, 0, 0.5), ylim = c(-10, 10), lty = 1, pch = 1, 
        col.axis = "green", lwd = 0.5)
axis(side = 4, col = "green")
mtext("Residuals", side = 4, line = 2, col = "green")
par(mar = c(5, 4, 4, 2) + 0.1)
par(cex.main = 1, cex.lab = 0.9, cex.axis = 0.9)


## @knitr ex1-MA-10_in-sample-fit
kable(head(cbind("Time" = as.character(as.yearmon(time(hw08.ts), "%b %Y")),
                 "Original series" = frmt(as.numeric(hw08.ts), 1), 
                 "Estimated series" = frmt(as.numeric(fitted(ma10)), 1),
                 "Residuals" = frmt(as.numeric(ma10$resid), 1))),
      row.names = FALSE, align = "r")
par(mar = c(5, 4, 4, 5) + 0.1)
par(cex.main = 1, cex.lab = 0.9, cex.axis = 0.9)
plot(hw08.ts, xlab = "Year (time period: month)", 
     main = "Original vs. a MA(10) Estimated Series with Residuals",
     ylab = "Original and Estimated Values",
     ylim = c(40, 160), lwd = 0.5)
leg.txt <- c("Original Series", "Estimated Series", "Residuals")
legend("topleft", legend = leg.txt, lty = 1, col = c("black", "blue", "green"), 
       bty = 'n', cex = 0.9)
lines(fitted(ma10), col = "blue", lwd = 0.5)
par(new = TRUE)
plot.ts(ma10$resid, axes = FALSE, xlab = "", ylab = "", 
        col = rgb(0, 1, 0, 0.5), ylim = c(-10, 10), lty = 1, pch = 1, 
        col.axis = "green", lwd = 0.5)
axis(side = 4, col = "green")
mtext("Residuals", side = 4, line = 2, col = "green")
par(mar = c(5, 4, 4, 2) + 0.1)
par(cex.main = 1, cex.lab = 0.9, cex.axis = 0.9)


# @knitr ex1-MA-12_out-of-sample-fit
(ma12.oos.fit <- Arima(hw08.ts_train, order = c(0, 0, ma_models_coefs[1])))
kable(head(cbind("Time" = as.character(as.yearmon(time(hw08.ts_train), 
                                                  "%b %Y")),
                 "Original series" = frmt(as.numeric(hw08.ts_train), 1), 
                 "Estimated series" = frmt(as.numeric(fitted(ma12.oos.fit)), 
                                           1), 
                 "Residuals" = frmt(as.numeric(ma12.oos.fit$resid), 1))), 
      row.names = FALSE, align = "r")
# Forecast / Backtesting
ma12.oos.fit.fcast <- forecast.Arima(ma12.oos.fit, h = 36)
(acc_ma12 <- accuracy(ma12.oos.fit.fcast, hw08.ts_test))

# @knitr ex1-MA-12_out-of-sample-fit-2
plot(ma12.oos.fit.fcast, col = 'blue', ylim = c(40, 160), 
     xlab = "Year (time period: month)", 
     main = "Original vs. an MA(12) model Forecasts",
     ylab = "Original and Forecasted Values")
leg.txt <- c("Original Series", "Forecasts from MA(12) model")
legend("topleft", legend = leg.txt, lty = 1, col = c("black", "blue"), 
       bty = 'n', cex = 0.9)
lines(hw08.ts, col = "black")


# @knitr ex1-MA-10_out-of-sample-fit
(ma10.oos.fit <- Arima(hw08.ts_train, order = c(0, 0, ma_models_coefs[2])))
# Forecast / Backtesting
ma10.oos.fit.fcast <- forecast.Arima(ma10.oos.fit, h = 36)
(acc_ma10 <- accuracy(ma10.oos.fit.fcast, hw08.ts_test))

# @knitr ex1-MA-10_out-of-sample-fit-2
plot(ma10.oos.fit.fcast, col = 'blue', ylim = c(40, 160), 
     xlab = "Year (time period: month)", 
     main = "Original vs. an MA(10) model Forecasts",
     ylab = "Original and Forecasted Values")
leg.txt <- c("Original Series", "Forecasts from MA(10) model")
legend("topleft", legend = leg.txt, lty = 1, col = c("black", "blue"), 
       bty = 'n', cex = 0.9)
lines(hw08.ts, col = "black")


## @knitr ex1-ARMA_models-83
# ARMA Models --------------------------------------------------------------
arma83 <- arma_models[[1]]
Parameters <- cbind(arma83$coef, sqrt(diag(arma83$var.coef)), 
                    matrix(sapply(c(-2,2), function(i) 
                      arma83$coef + i * sqrt(diag(arma83$var.coef))), ncol = 2))
colnames(Parameters) <- c("Coefficient", "SE", "95% CI lower", "95% CI upper")
kable(Parameters, digits = 4, 
      caption = "Coefficients, SEs, and 95% CIs of the estimated ARMA(8,3) model")

## @knitr ex1-ARMA_models-83-2
(roots_ar <- polyroot(c(1, -arma83$coef[1:arma_models_coefs[1, 1]])))
all(Mod(roots_ar) > 1) # Stationarity condition
(roots_ma <- polyroot(c(1, arma83$coef[(arma_models_coefs[1, 1] + 1):
                                         (arma_models_coefs[1, 1] + 
                                            arma_models_coefs[1, 2])])))
all(Mod(roots_ma) > 1) # Invertibility condition


## @knitr ex1-ARMA_models-74
arma74 <- arma_models[[2]]
Parameters <- cbind(arma74$coef, sqrt(diag(arma74$var.coef)), 
                    matrix(sapply(c(-2,2), function(i) 
                      arma74$coef + i * sqrt(diag(arma74$var.coef))), ncol = 2))
colnames(Parameters) <- c("Coefficient", "SE", "95% CI lower", "95% CI upper")
kable(Parameters, digits = 4, 
      caption = "Coefficients, SEs, and 95% CIs of the estimated ARMA(7,4) model")

## @knitr ex1-ARMA_models-74-2
(roots_ar <- polyroot(c(1, -arma74$coef[1:arma_models_coefs[2, 1]])))
all(Mod(roots_ar) > 1) # Stationarity condition
(roots_ma <- polyroot(c(1, arma74$coef[(arma_models_coefs[2, 1] + 1):
                                         (arma_models_coefs[2, 1] + 
                                            arma_models_coefs[2, 2])])))
all(Mod(roots_ma) > 1) # Invertibility condition


## @knitr ex1-ARMA_models-39
arma39 <- arma_models[[3]]
Parameters <- cbind(arma39$coef, sqrt(diag(arma39$var.coef)), 
                    matrix(sapply(c(-2,2), function(i) 
                      arma39$coef + i * sqrt(diag(arma39$var.coef))), ncol = 2))
colnames(Parameters) <- c("Coefficient", "SE", "95% CI lower", "95% CI upper")
kable(Parameters, digits = 4, 
      caption = "Coefficients, SEs, and 95% CIs of the estimated ARMA(3,9) model")

## @knitr ex1-ARMA_models-39-2
(roots_ar <- polyroot(c(1, -arma39$coef[1:arma_models_coefs[3, 1]])))
all(Mod(roots_ar) > 1) # Stationarity condition
(roots_ma <- polyroot(c(1, arma39$coef[(arma_models_coefs[3, 1] + 1):
                                         (arma_models_coefs[3, 1] + 
                                            arma_models_coefs[3, 2])])))
all(Mod(roots_ma) > 1) # Invertibility condition


## @knitr ex1-ARMA_models-52
arma52 <- arma_models[[4]]
Parameters <- cbind(arma52$coef, sqrt(diag(arma52$var.coef)), 
                    matrix(sapply(c(-2,2), function(i) 
                      arma52$coef + i * sqrt(diag(arma52$var.coef))), ncol = 2))
colnames(Parameters) <- c("Coefficient", "SE", "95% CI lower", "95% CI upper")
kable(Parameters, digits = 4, 
      caption = "Coefficients, SEs, and 95% CIs of the estimated ARMA(5,2) model")

## @knitr ex1-ARMA_models-52-2
(roots_ar <- polyroot(c(1, -arma52$coef[1:arma_models_coefs[4, 1]])))
all(Mod(roots_ar) > 1) # Stationarity condition
(roots_ma <- polyroot(c(1, arma52$coef[(arma_models_coefs[4, 1] + 1):
                                         (arma_models_coefs[4, 1] + 
                                            arma_models_coefs[4, 2])])))
all(Mod(roots_ma) > 1) # Invertibility condition


## @knitr ex1-ARMA_models-110
arma110 <- arma_models[[5]]
Parameters <- cbind(arma110$coef, sqrt(diag(arma110$var.coef)), 
                    matrix(sapply(c(-2,2), function(i) 
                      arma110$coef + i * sqrt(diag(arma110$var.coef))), 
                      ncol = 2))
colnames(Parameters) <- c("Coefficient", "SE", "95% CI lower", "95% CI upper")
kable(Parameters, digits = 4, 
      caption = "Coefficients, SEs, and 95% CIs of the estimated ARMA(1,10) model")

## @knitr ex1-ARMA_models-110-2
(roots_ar <- polyroot(c(1, -arma110$coef[1:arma_models_coefs[4, 1]])))
all(Mod(roots_ar) > 1) # Stationarity condition
(roots_ma <- polyroot(c(1, arma110$coef[(arma_models_coefs[4, 1] + 1):
                                          (arma_models_coefs[4, 1] + 
                                             arma_models_coefs[4, 2])])))
all(Mod(roots_ma) > 1) # Invertibility condition


## @knitr ex1-ARMA_models-15
arma15 <- arma_models[[6]]
Parameters <- cbind(arma15$coef, sqrt(diag(arma15$var.coef)), 
                    matrix(sapply(c(-2,2), function(i) 
                      arma15$coef + i * sqrt(diag(arma15$var.coef))), ncol = 2))
colnames(Parameters) <- c("Coefficient", "SE", "95% CI lower", "95% CI upper")
kable(Parameters, digits = 4, 
      caption = "Coefficients, SEs, and 95% CIs of the estimated ARMA(1,5) model")

## @knitr ex1-ARMA_models-15-2
(roots_ar <- polyroot(c(1, -arma15$coef[1:arma_models_coefs[4, 1]])))
all(Mod(roots_ar) > 1) # Stationarity condition
(roots_ma <- polyroot(c(1, arma110$coef[(arma_models_coefs[4, 1] + 1):
                                          (arma_models_coefs[4, 1] + 
                                             arma_models_coefs[4, 2])])))
all(Mod(roots_ma) > 1) # Invertibility condition


## @knitr ex1-ARMA-83_res_plots
summary(arma83$resid)
par(mfrow=c(2, 2))
par(cex.main = 1, cex.lab = 0.9, cex.axis = 0.9)
plot(arma83$resid, main = "Residual Series of\nthe ARMA(8,3) model", 
     col="blue", xlab = "Year (time period: month)", ylab = "Residual level")
hist(arma83$resid, col = "gray", breaks = 20, xlab = "Residual level", 
     main = "Histogram of the residuals\n(ARMA(8,3) model")
acf(arma83$resid, main = "ACF of the Residual\nSeries (ARMA(8,3) model)")
pacf(arma83$resid, main = "PACF of the Residual\nSeries (ARMA(8,3) model)")
par(mfrow=c(1, 1))
par(cex.main = 1, cex.lab = 0.9, cex.axis = 0.9)

## @knitr ex1-ARMA-83_boxtest
Box.test(arma83$resid, type = "Ljung-Box")

## @knitr ex1-ARMA-74_res_plots
summary(arma74$resid)
par(mfrow=c(2, 2))
par(cex.main = 1, cex.lab = 0.9, cex.axis = 0.9)
plot(arma74$resid, main = "Residual Series of\nthe ARMA(7,4) model", 
     col="blue", xlab = "Year (time period: month)", ylab = "Residual level")
hist(arma74$resid, col = "gray", breaks = 20, xlab = "Residual level", 
     main = "Histogram of the residuals\n(ARMA(7,4) model")
acf(arma74$resid, main = "ACF of the Residual\nSeries (ARMA(7,4) model)")
pacf(arma74$resid, main = "PACF of the Residual\nSeries (ARMA(7,4) model)")
par(mfrow=c(1, 1))
par(cex.main = 1, cex.lab = 0.9, cex.axis = 0.9)

## @knitr ex1-ARMA-74_boxtest
Box.test(arma74$resid, type = "Ljung-Box")


## @knitr ex1-ARMA-39_res_plots
summary(arma39$resid)
par(mfrow=c(2, 2))
par(cex.main = 1, cex.lab = 0.9, cex.axis = 0.9)
plot(arma39$resid, main = "Residual Series of\nthe ARMA(3,9) model", 
     col="blue", xlab = "Year (time period: month)", ylab = "Residual level")
hist(arma39$resid, col = "gray", breaks = 20, xlab = "Residual level", 
     main = "Histogram of the residuals\n(ARMA(3,9) model")
acf(arma39$resid, main = "ACF of the Residual\nSeries (ARMA(3,9) model)")
pacf(arma39$resid, main = "PACF of the Residual\nSeries (ARMA(3,9) model)")
par(mfrow=c(1, 1))
par(cex.main = 1, cex.lab = 0.9, cex.axis = 0.9)

## @knitr ex1-ARMA-39_boxtest
Box.test(arma39$resid, type = "Ljung-Box")


## @knitr ex1-ARMA-52_res_plots
summary(arma52$resid)
par(mfrow=c(2, 2))
par(cex.main = 1, cex.lab = 0.9, cex.axis = 0.9)
plot(arma52$resid, main = "Residual Series of\nthe ARMA(5,2) model", 
     col="blue", xlab = "Year (time period: month)", ylab = "Residual level")
hist(arma52$resid, col = "gray", breaks = 20, xlab = "Residual level", 
     main = "Histogram of the residuals\n(ARMA(5,2) model")
acf(arma52$resid, main = "ACF of the Residual\nSeries (ARMA(5,2) model)")
pacf(arma52$resid, main = "PACF of the Residual\nSeries (ARMA(5,2) model)")
par(mfrow=c(1, 1))
par(cex.main = 1, cex.lab = 0.9, cex.axis = 0.9)

## @knitr ex1-ARMA-52_boxtest
Box.test(arma52$resid, type = "Ljung-Box")


## @knitr ex1-ARMA-110_res_plots
summary(arma110$resid)
par(mfrow=c(2, 2))
par(cex.main = 1, cex.lab = 0.9, cex.axis = 0.9)
plot(arma110$resid, main = "Residual Series of\nthe ARMA(1,10) model", 
     col="blue", xlab = "Year (time period: month)", ylab = "Residual level")
hist(arma110$resid, col = "gray", breaks = 20, xlab = "Residual level", 
     main = "Histogram of the residuals\n(ARMA(1,10) model")
acf(arma110$resid, main = "ACF of the Residual\nSeries (ARMA(1,10) model)")
pacf(arma110$resid, main = "PACF of the Residual\nSeries (ARMA(1,10) model)")
par(mfrow=c(1, 1))
par(cex.main = 1, cex.lab = 0.9, cex.axis = 0.9)

## @knitr ex1-ARMA-110_boxtest
Box.test(arma110$resid, type = "Ljung-Box")


## @knitr ex1-ARMA-15_res_plots
summary(arma15$resid)
par(mfrow=c(2, 2))
par(cex.main = 1, cex.lab = 0.9, cex.axis = 0.9)
plot(arma15$resid, main = "Residual Series of\nthe ARMA(1,5) model", 
     col="blue", xlab = "Year (time period: month)", ylab = "Residual level")
hist(arma15$resid, col = "gray", breaks = 20, xlab = "Residual level", 
     main = "Histogram of the residuals\n(ARMA(1,5) model")
acf(arma15$resid, main = "ACF of the Residual\nSeries (ARMA(1,5) model)")
pacf(arma15$resid, main = "PACF of the Residual\nSeries (ARMA(1,5) model)")
par(mfrow=c(1, 1))
par(cex.main = 1, cex.lab = 0.9, cex.axis = 0.9)

## @knitr ex1-ARMA-15_boxtest
Box.test(arma15$resid, type = "Ljung-Box")


## @knitr ex1-ARMA-83_in-sample-fit
kable(tail(cbind("Time" = as.character(as.yearmon(time(hw08.ts), "%b %Y")),
                 "Original series" = frmt(as.numeric(hw08.ts), 1), 
                 "Estimated series" = frmt(as.numeric(fitted(arma83)), 1),
                 "Residuals" = frmt(as.numeric(arma83$resid), 1))),
      row.names = FALSE, align = "r")
par(mar = c(5, 4, 4, 5) + 0.1)
par(cex.main = 1, cex.lab = 0.9, cex.axis = 0.9)
plot(hw08.ts, xlab = "Year (time period: month)", 
     main = "Original vs. an ARMA(8,3) Estimated Series with Residuals",
     ylab = "Original and Estimated Values",
     ylim = c(40, 160), lwd = 0.5)
leg.txt <- c("Original Series", "Estimated Series", "Residuals")
legend("topleft", legend = leg.txt, lty = 1, col = c("black", "blue", "green"), 
       bty = 'n', cex = 0.9)
lines(fitted(arma83), col = "blue", lwd = 0.5)
par(new = TRUE)
plot.ts(arma83$resid, axes = FALSE, xlab = "", ylab = "", 
        col = rgb(0, 1, 0, 0.5), ylim = c(-10, 10), lty = 1, pch = 1, 
        col.axis = "green", lwd = 0.5)
axis(side = 4, col = "green")
mtext("Residuals", side = 4, line = 2, col = "green")
par(mar = c(5, 4, 4, 2) + 0.1)
par(cex.main = 1, cex.lab = 0.9, cex.axis = 0.9)


## @knitr ex1-ARMA-74_in-sample-fit
kable(tail(cbind("Time" = as.character(as.yearmon(time(hw08.ts), "%b %Y")),
                 "Original series" = frmt(as.numeric(hw08.ts), 1), 
                 "Estimated series" = frmt(as.numeric(fitted(arma74)), 1),
                 "Residuals" = frmt(as.numeric(arma74$resid), 1))),
      row.names = FALSE, align = "r")
par(mar = c(5, 4, 4, 5) + 0.1)
par(cex.main = 1, cex.lab = 0.9, cex.axis = 0.9)
plot(hw08.ts, xlab = "Year (time period: month)", 
     main = "Original vs. an ARMA(7,4) Estimated Series with Residuals",
     ylab = "Original and Estimated Values",
     ylim = c(40, 160), lwd = 0.5)
leg.txt <- c("Original Series", "Estimated Series", "Residuals")
legend("topleft", legend = leg.txt, lty = 1, col = c("black", "blue", "green"), 
       bty = 'n', cex = 0.9)
lines(fitted(arma74), col = "blue", lwd = 0.5)
par(new = TRUE)
plot.ts(arma74$resid, axes = FALSE, xlab = "", ylab = "", 
        col = rgb(0, 1, 0, 0.5), ylim = c(-10, 10), lty = 1, pch = 1, 
        col.axis = "green", lwd = 0.5)
axis(side = 4, col = "green")
mtext("Residuals", side = 4, line = 2, col = "green")
par(mar = c(5, 4, 4, 2) + 0.1)
par(cex.main = 1, cex.lab = 0.9, cex.axis = 0.9)


## @knitr ex1-ARMA-39_in-sample-fit
kable(tail(cbind("Time" = as.character(as.yearmon(time(hw08.ts), "%b %Y")),
                 "Original series" = frmt(as.numeric(hw08.ts), 1), 
                 "Estimated series" = frmt(as.numeric(fitted(arma39)), 1),
                 "Residuals" = frmt(as.numeric(arma39$resid), 1))),
      row.names = FALSE, align = "r")
par(mar = c(5, 4, 4, 5) + 0.1)
par(cex.main = 1, cex.lab = 0.9, cex.axis = 0.9)
plot(hw08.ts, xlab = "Year (time period: month)", 
     main = "Original vs. an ARMA(3,9) Estimated Series with Residuals",
     ylab = "Original and Estimated Values",
     ylim = c(40, 160), lwd = 0.5)
leg.txt <- c("Original Series", "Estimated Series", "Residuals")
legend("topleft", legend = leg.txt, lty = 1, col = c("black", "blue", "green"), 
       bty = 'n', cex = 0.9)
lines(fitted(arma39), col = "blue", lwd = 0.5)
par(new = TRUE)
plot.ts(arma39$resid, axes = FALSE, xlab = "", ylab = "", 
        col = rgb(0, 1, 0, 0.5), ylim = c(-10, 10), lty = 1, pch = 1, 
        col.axis = "green", lwd = 0.5)
axis(side = 4, col = "green")
mtext("Residuals", side = 4, line = 2, col = "green")
par(mar = c(5, 4, 4, 2) + 0.1)
par(cex.main = 1, cex.lab = 0.9, cex.axis = 0.9)


## @knitr ex1-ARMA-52_in-sample-fit
kable(tail(cbind("Time" = as.character(as.yearmon(time(hw08.ts), "%b %Y")),
                 "Original series" = frmt(as.numeric(hw08.ts), 1), 
                 "Estimated series" = frmt(as.numeric(fitted(arma52)), 1),
                 "Residuals" = frmt(as.numeric(arma52$resid), 1))),
      row.names = FALSE, align = "r")
par(mar = c(5, 4, 4, 5) + 0.1)
par(cex.main = 1, cex.lab = 0.9, cex.axis = 0.9)
plot(hw08.ts, xlab = "Year (time period: month)", 
     main = "Original vs. an ARMA(5,2) Estimated Series with Residuals",
     ylab = "Original and Estimated Values",
     ylim = c(40, 160), lwd = 0.5)
leg.txt <- c("Original Series", "Estimated Series", "Residuals")
legend("topleft", legend = leg.txt, lty = 1, col = c("black", "blue", "green"), 
       bty = 'n', cex = 0.9)
lines(fitted(arma52), col = "blue", lwd = 0.5)
par(new = TRUE)
plot.ts(arma52$resid, axes = FALSE, xlab = "", ylab = "", 
        col = rgb(0, 1, 0, 0.5), ylim = c(-10, 10), lty = 1, pch = 1, 
        col.axis = "green", lwd = 0.5)
axis(side = 4, col = "green")
mtext("Residuals", side = 4, line = 2, col = "green")
par(mar = c(5, 4, 4, 2) + 0.1)
par(cex.main = 1, cex.lab = 0.9, cex.axis = 0.9)


## @knitr ex1-ARMA-110_in-sample-fit
kable(tail(cbind("Time" = as.character(as.yearmon(time(hw08.ts), "%b %Y")),
                 "Original series" = frmt(as.numeric(hw08.ts), 1), 
                 "Estimated series" = frmt(as.numeric(fitted(arma110)), 1),
                 "Residuals" = frmt(as.numeric(arma110$resid), 1))),
      row.names = FALSE, align = "r")
par(mar = c(5, 4, 4, 5) + 0.1)
par(cex.main = 1, cex.lab = 0.9, cex.axis = 0.9)
plot(hw08.ts, xlab = "Year (time period: month)", 
     main = "Original vs. an ARMA(1,10) Estimated Series with Residuals",
     ylab = "Original and Estimated Values",
     ylim = c(40, 160), lwd = 0.5)
leg.txt <- c("Original Series", "Estimated Series", "Residuals")
legend("topleft", legend = leg.txt, lty = 1, col = c("black", "blue", "green"), 
       bty = 'n', cex = 0.9)
lines(fitted(arma110), col = "blue", lwd = 0.5)
par(new = TRUE)
plot.ts(arma110$resid, axes = FALSE, xlab = "", ylab = "", 
        col = rgb(0, 1, 0, 0.5), ylim = c(-10, 10), lty = 1, pch = 1, 
        col.axis = "green", lwd = 0.5)
axis(side = 4, col = "green")
mtext("Residuals", side = 4, line = 2, col = "green")
par(mar = c(5, 4, 4, 2) + 0.1)
par(cex.main = 1, cex.lab = 0.9, cex.axis = 0.9)


## @knitr ex1-ARMA-15_in-sample-fit
kable(tail(cbind("Time" = as.character(as.yearmon(time(hw08.ts), "%b %Y")),
                 "Original series" = frmt(as.numeric(hw08.ts), 1), 
                 "Estimated series" = frmt(as.numeric(fitted(arma15)), 1),
                 "Residuals" = frmt(as.numeric(arma15$resid), 1))),
      row.names = FALSE, align = "r")
par(mar = c(5, 4, 4, 5) + 0.1)
par(cex.main = 1, cex.lab = 0.9, cex.axis = 0.9)
plot(hw08.ts, xlab = "Year (time period: month)", 
     main = "Original vs. an ARMA(1,5) Estimated Series with Residuals",
     ylab = "Original and Estimated Values",
     ylim = c(40, 160), lwd = 0.5)
leg.txt <- c("Original Series", "Estimated Series", "Residuals")
legend("topleft", legend = leg.txt, lty = 1, col = c("black", "blue", "green"), 
       bty = 'n', cex = 0.9)
lines(fitted(arma15), col = "blue", lwd = 0.5)
par(new = TRUE)
plot.ts(arma15$resid, axes = FALSE, xlab = "", ylab = "", 
        col = rgb(0, 1, 0, 0.5), ylim = c(-10, 10), lty = 1, pch = 1, 
        col.axis = "green", lwd = 0.5)
axis(side = 4, col = "green")
mtext("Residuals", side = 4, line = 2, col = "green")
par(mar = c(5, 4, 4, 2) + 0.1)
par(cex.main = 1, cex.lab = 0.9, cex.axis = 0.9)


## @knitr ex1-ARMA-83_out-of-sample-fit
(arma83.oos.fit <- Arima(hw08.ts_train, order = c(arma_models_coefs[1, 1], 0, 
                                                  arma_models_coefs[1, 2])))
kable(head(cbind("Time" = as.character(as.yearmon(time(hw08.ts_train), 
                                                  "%b %Y")), 
                 "Original series" = frmt(as.numeric(hw08.ts_train), 1), 
                 "Estimated series" = frmt(as.numeric(fitted(arma83.oos.fit)), 
                                           1), 
                 "Residuals" = frmt(as.numeric(arma83.oos.fit$resid), 1))), 
      row.names = FALSE, align = "r")
# Forecast / Backtesting
arma83.oos.fit.fcast <- forecast.Arima(arma83.oos.fit, h = 36)
(acc_arma83 <- accuracy(arma83.oos.fit.fcast, hw08.ts_test))

## @knitr ex1-ARMA-83_out-of-sample-fit-2
plot(arma83.oos.fit.fcast, col = 'blue', ylim = c(40, 160), 
     xlab = "Year (time period: month)", 
     main = "Original vs. an ARMA(8,3) model Forecasts",
     ylab = "Original and Forecasted Values")
leg.txt <- c("Original Series", "Forecasts from ARMA(8,3) model")
legend("topleft", legend = leg.txt, lty = 1, col = c("black", "blue"), 
       bty = 'n', cex = 0.9)
lines(hw08.ts, col = "black")


## @knitr ex1-ARMA-74_out-of-sample-fit
hw08.ts_train <- window(hw08.ts, start = 1980, end=c(2005,12))
hw08.ts_test <- window(hw08.ts, start = 2006)
(arma74.oos.fit <- Arima(hw08.ts_train, order = c(arma_models_coefs[2, 1], 0, 
                                                  arma_models_coefs[2, 2])))
kable(head(cbind("Time" = as.character(as.yearmon(time(hw08.ts_train), 
                                                  "%b %Y")), 
                 "Original series" = frmt(as.numeric(hw08.ts_train), 1), 
                 "Estimated series" = frmt(as.numeric(fitted(arma74.oos.fit)), 
                                           1), 
                 "Residuals" = frmt(as.numeric(arma74.oos.fit$resid), 1))), 
      row.names = FALSE, align = "r")
# Forecast / Backtesting
arma74.oos.fit.fcast <- forecast.Arima(arma74.oos.fit, h = 60)
(acc_arma74 <- accuracy(arma74.oos.fit.fcast, hw08.ts_test))

## @knitr ex1-ARMA-74_out-of-sample-fit-2
plot(arma74.oos.fit.fcast, col = 'blue', ylim = c(40, 160), 
     xlab = "Year (time period: month)", 
     main = "Original vs. an ARMA(7,4) model Forecasts",
     ylab = "Original and Forecasted Values")
leg.txt <- c("Original Series", "Forecasts from ARMA(7,4) model")
legend("topleft", legend = leg.txt, lty = 1, col = c("black", "blue"), 
       bty = 'n', cex = 0.9)
lines(hw08.ts, col = "black")


## @knitr ex1-ARMA-39_out-of-sample-fit
hw08.ts_train <- window(hw08.ts, start = 1980, end=c(2007,12))
hw08.ts_test <- window(hw08.ts, start = 2008)
(arma39.oos.fit <- Arima(hw08.ts_train, order = c(arma_models_coefs[3, 1], 0, 
                                                  arma_models_coefs[3, 2])))
kable(head(cbind("Time" = as.character(as.yearmon(time(hw08.ts_train), 
                                                  "%b %Y")), 
                 "Original series" = frmt(as.numeric(hw08.ts_train), 1), 
                 "Estimated series" = frmt(as.numeric(fitted(arma39.oos.fit)), 
                                           1), 
                 "Residuals" = frmt(as.numeric(arma39.oos.fit$resid), 1))), 
      row.names = FALSE, align = "r")
# Forecast / Backtesting
arma39.oos.fit.fcast <- forecast.Arima(arma39.oos.fit, h = 36)
(acc_arma39 <- accuracy(arma39.oos.fit.fcast, hw08.ts_test))

## @knitr ex1-ARMA-39_out-of-sample-fit-2
plot(arma39.oos.fit.fcast, col = 'blue', ylim = c(40, 160), 
     xlab = "Year (time period: month)", 
     main = "Original vs. an ARMA(3,9) model Forecasts",
     ylab = "Original and Forecasted Values")
leg.txt <- c("Original Series", "Forecasts from ARMA(3,9) model")
legend("topleft", legend = leg.txt, lty = 1, col = c("black", "blue"), 
       bty = 'n', cex = 0.9)
lines(hw08.ts, col = "black")


## @knitr ex1-ARMA-52_out-of-sample-fit
(arma52.oos.fit <- Arima(hw08.ts_train, order = c(arma_models_coefs[4, 1], 0, 
                                                  arma_models_coefs[4, 2])))
kable(head(cbind("Time" = as.character(as.yearmon(time(hw08.ts_train), 
                                                  "%b %Y")), 
                 "Original series" = frmt(as.numeric(hw08.ts_train), 1), 
                 "Estimated series" = frmt(as.numeric(fitted(arma52.oos.fit)), 
                                           1), 
                 "Residuals" = frmt(as.numeric(arma52.oos.fit$resid), 1))), 
      row.names = FALSE, align = "r")
# Forecast / Backtesting
arma52.oos.fit.fcast <- forecast.Arima(arma52.oos.fit, h = 36)
(acc_arma52 <- accuracy(arma52.oos.fit.fcast, hw08.ts_test))

## @knitr ex1-ARMA-52_out-of-sample-fit-2
plot(arma52.oos.fit.fcast, col = 'blue', ylim = c(40, 160), 
     xlab = "Year (time period: month)", 
     main = "Original vs. an ARMA(5,2) model Forecasts",
     ylab = "Original and Forecasted Values")
leg.txt <- c("Original Series", "Forecasts from ARMA(5,2) model")
legend("topleft", legend = leg.txt, lty = 1, col = c("black", "blue"), 
       bty = 'n', cex = 0.9)
lines(hw08.ts, col = "black")


## @knitr ex1-ARMA-110_out-of-sample-fit
(arma110.oos.fit <- Arima(hw08.ts_train, order = c(arma_models_coefs[5, 1], 0, 
                                                  arma_models_coefs[5, 2])))
kable(head(cbind("Time" = as.character(as.yearmon(time(hw08.ts_train), 
                                                  "%b %Y")), 
                 "Original series" = frmt(as.numeric(hw08.ts_train), 1), 
                 "Estimated series" = frmt(as.numeric(fitted(arma110.oos.fit)), 
                                           1), 
                 "Residuals" = frmt(as.numeric(arma110.oos.fit$resid), 1))), 
      row.names = FALSE, align = "r")
# Forecast / Backtesting
arma110.oos.fit.fcast <- forecast.Arima(arma110.oos.fit, h = 36)
(acc_arma110 <- accuracy(arma110.oos.fit.fcast, hw08.ts_test))

## @knitr ex1-ARMA-110_out-of-sample-fit-2
plot(arma110.oos.fit.fcast, col = 'blue', ylim = c(40, 160), 
     xlab = "Year (time period: month)", 
     main = "Original vs. an ARMA(1,10) model Forecasts",
     ylab = "Original and Forecasted Values")
leg.txt <- c("Original Series", "Forecasts from ARMA(1,10) model")
legend("topleft", legend = leg.txt, lty = 1, col = c("black", "blue"), 
       bty = 'n', cex = 0.9)
lines(hw08.ts, col = "black")


## @knitr ex1-ARMA-15_out-of-sample-fit
(arma15.oos.fit <- Arima(hw08.ts_train, order = c(arma_models_coefs[6, 1], 0, 
                                                  arma_models_coefs[6, 2])))
kable(head(cbind("Time" = as.character(as.yearmon(time(hw08.ts_train), 
                                                  "%b %Y")), 
                 "Original series" = frmt(as.numeric(hw08.ts_train), 1), 
                 "Estimated series" = frmt(as.numeric(fitted(arma15.oos.fit)), 
                                           1), 
                 "Residuals" = frmt(as.numeric(arma15.oos.fit$resid), 1))), 
      row.names = FALSE, align = "r")
# Forecast / Backtesting
arma15.oos.fit.fcast <- forecast.Arima(arma15.oos.fit, h = 36)
(acc_arma15 <- accuracy(arma15.oos.fit.fcast, hw08.ts_test))

## @knitr ex1-ARMA-15_out-of-sample-fit-2
plot(arma15.oos.fit.fcast, col = 'blue', ylim = c(40, 160), 
     xlab = "Year (time period: month)", 
     main = "Original vs. an ARMA(1,5) model Forecasts",
     ylab = "Original and Forecasted Values")
leg.txt <- c("Original Series", "Forecasts from ARMA(1,5) model")
legend("topleft", legend = leg.txt, lty = 1, col = c("black", "blue"), 
       bty = 'n', cex = 0.9)
lines(hw08.ts, col = "black")


## @knitr ex1-comparison
kable(aic_list %>% arrange(aic) %>% 
        filter((p == 3 & q == 0) | (p == 9 & q == 0) | (p == 0 & q == 12) | 
                 (p == 0 & q == 10) | (p == 8 & q == 3) | (p == 7 & q == 4) | 
                 (p == 3 & q == 9) | (p == 5 & q == 2) | (p == 1 & q == 10) | 
                 (p == 1 & q == 5)), digits = 1, 
      caption = "AIC of the models under study (in increasing order)")
kable(aic_list %>% arrange(bic) %>% 
        filter((p == 3 & q == 0) | (p == 9 & q == 0) | (p == 0 & q == 12) | 
                 (p == 0 & q == 10) | (p == 8 & q == 3) | (p == 7 & q == 4) | 
                 (p == 3 & q == 9) | (p == 5 & q == 2) | (p == 1 & q == 10) | 
                 (p == 1 & q == 5)), digits = 1, 
      caption = "BIC of the models under study (in increasing order)")

## @knitr ex1-comparison-2
acc_comp <- frmt(rbind(acc_ar9[, 2:3], acc_ar3[, 2:3], acc_ma12[, 2:3], 
                       acc_ma10[, 2:3], acc_arma83[, 2:3], acc_arma74[, 2:3], 
                       acc_arma39[, 2:3], acc_arma52[, 2:3], 
                       acc_arma110[, 2:3], acc_arma15[, 2:3]), 1)
acc_rows <- bind_rows(aic_list %>% arrange(aic) %>% filter(family == "AR") %>% 
                        top_n(-2, aic) %>% select(p, q, family), 
                      aic_list %>% arrange(bic) %>% filter(family == "MA") %>% 
                        top_n(-2, bic) %>% select(p, q, family), 
                      aic_list %>% arrange(bic) %>% 
                        filter(family == "ARMA") %>% top_n(-4, bic) %>% 
                        select(p, q, family), 
                      aic_list %>% arrange(ACF) %>% top_n(-1, ACF) %>% 
                        select(p, q, family), 
                      aic_list %>% arrange(PACF) %>% top_n(-1, PACF) %>% 
                        select(p, q, family))
acc_rows <- as.matrix(acc_rows[rep(seq_len(nrow(acc_rows)), each=2), ])
rownames(acc_rows) <- rownames(acc_comp)
kable(cbind(acc_rows, acc_comp), align = 'r', 
      caption = paste0("RMSE and MAE of the models under study for the ", 
                       "training and test sets in the out-of-sample fit"))


## @knitr ex1-forecast-arma39
arma39.fcast <- forecast.Arima(arma39, h = 12)
arma39.fcast2 <- predict(arma39, n.ahead = 12)
pander(arma39.fcast2$pred)
plot(arma39.fcast, 
     main="12-Step Ahead Forecast and Original\n& Estimated Series (ARMA(3,9) model)",
     xlab = "Year (time period: month)", 
     ylab="Original, Estimated, and Forecasted Values")
lines(fitted(arma39),col="blue")  
leg.txt <- c("Original Series", 
             "Estimated series and ahead\nforecasts from ARMA(3,9) model")
legend("topleft", legend = leg.txt, lty = 1, col = c("black", "blue"), 
       bty = 'n', cex = 0.9)


## @knitr ex1-forecast-ar9
ar9.fcast <- forecast.Arima(ar9, h = 12)
ar9.fcast2 <- predict(ar9, n.ahead = 12)
pander(ar9.fcast2$pred)
plot(ar9.fcast, 
     main="12-Step Ahead Forecast and Original\n& Estimated Series (AR(9) model)",
     xlab = "Year (time period: month)", 
     ylab="Original, Estimated, and Forecasted Values")
lines(fitted(ar9),col="blue")  
leg.txt <- c("Original Series", 
             "Estimated series and ahea\nforecasts from AR(9) model")
legend("topleft", legend = leg.txt, lty = 1, col = c("black", "blue"), 
       bty = 'n', cex = 0.9)


## @knitr ex1-forecast-arma15
arma15.fcast <- forecast.Arima(arma15, h = 12)
arma15.fcast2 <- predict(arma15, n.ahead = 12)
pander(arma15.fcast2$pred)
plot(arma15.fcast, 
     main="12-Step Ahead Forecast and Original\n& Estimated Series (ARMA(1,5) model)",
     xlab = "Year (time period: month)", 
     ylab="Original, Estimated, and Forecasted Values")
lines(fitted(arma39),col="blue")  
leg.txt <- c("Original Series", 
             "Estimated series and ahead\nforecasts from ARMA(1,5) model")
legend("topleft", legend = leg.txt, lty = 1, col = c("black", "blue"), 
       bty = 'n', cex = 0.9)


## @knitr ex1-forecast-ma12
ma12.fcast <- forecast.Arima(ma12, h = 12)
ma12.fcast2 <- predict(ma12, n.ahead = 12)
pander(ma12.fcast2$pred)
plot(ma12.fcast, 
     main="12-Step Ahead Forecast and Original\n& Estimated Series (MA(12) model)",
     xlab = "Year (time period: month)", 
     ylab="Original, Estimated, and Forecasted Values")
lines(fitted(arma39),col="blue")  
leg.txt <- c("Original Series", 
             "Estimated series and ahead\nforecasts from MA(12) model")
legend("topleft", legend = leg.txt, lty = 1, col = c("black", "blue"), 
       bty = 'n', cex = 0.9)
