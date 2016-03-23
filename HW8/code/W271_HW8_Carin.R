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
library(pastecs)
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



## @knitr ex1-1
# EXERCISE 1 --------------------------------------------------------------
# setwd('./HW8/data')
hw08 <- read.csv('hw08_series.csv', header = TRUE)
str(hw08)
all(hw08$X == 1:dim(hw08)[1]) # check if 1st column is just an incremental index
hw08 <- hw08[, -1]

## @knitr ex1-2
# See the definition of the function in ## @knitr Libraries-Functions-Constants
desc_stat(hw08, 'Time series', 'Descriptive statistics of the time series.')

## @knitr ex1-3
hist(hw08, breaks = 30, col="gray", freq = FALSE, 
     xlab = "Level / Amplitude", main = "Histogram of the time series")
lines(density(hw08), col = 'blue', lty = 2)
leg.txt <- c("Estimated density plot")
legend("topright", legend = leg.txt, lty = 2, col = "blue", bty = 'n', cex = .8)

## @knitr ex1-4-1
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

## @knitr ex1-4-2
plot.ts(window(hw08.ts, 2005), col = 'blue', type = 'l', 
     xlab = "Year (time period: month)", ylab = "Level / Amplitude", 
     main = paste0("Detail of the last 72 observations"))
abline(v = seq(2005, 2011), lty = 2, col = "gray")
# lines(stats::filter(hw08.ts, sides=2, rep(1, 7)/7), lty = 1, lwd = 1.75, 
#       col = "green")
# leg.txt <- c("Time-series", "13-Point Symmetric Moving Average")
# legend("topleft", legend = leg.txt, lty = c(1, 1), lwd = c(1, 1.75), 
#        col = c("blue", "green"), bty = 'n', cex = .8)

## @knitr ex1-5
boxplot(hw08 ~ factor(rep(1980:2010, each = 12)), 
        outcex = 0.4, medcol="red", lwd = 0.5, 
        xlab = 'Year', ylab = 'Level / Amplitude',
        main = 'Box-and-whisker plot of\nthe time series per year')

## @knitr ex1-6
hw08_df <- data.frame(Year = factor(rep(1980:2010, each = 12)), 
                      Level = hw08)
kable(cbind(hw08_df %>% filter(Year %in% factor(1980:1989)) %>% 
              group_by(Year) %>% summarise(Variance = var(Level)) %>% 
              arrange(Year), 
            hw08_df %>% filter(Year %in% factor(1990:1999)) %>% 
              group_by(Year) %>% summarise(Variance = var(Level)) %>% 
              arrange(Year), 
            hw08_df %>% filter(Year %in% factor(2000:2009)) %>% 
              group_by(Year) %>% summarise(Variance = var(Level)) %>% 
              arrange(Year)), digits = 2, 
      caption = paste("Variance of the time-series amplitude per year ", 
                      "(for the first 30 out of 31)."))

## @knitr ex1-7
plot(decompose(hw08.ts, type = 'additive'), col = 'blue', 
     xlab = "Year (time period: month)")
# pacf(na.omit(decompose(hw08.ts, type = 'additive')$random), lag.max = 24)
# plot(stl(hw08.ts, s.window="periodic"), col = 'blue')

## @knitr Question2-a
# QUESTION 2 --------------------------------------------------------------
# Generate a zero-drift random walk model using 500 simulation
set.seed(123)
N <- 500 # number of simulations / time periods
wn <- rnorm(n = N, mean = 0, sd = 1) # white noise (can use any mean and sd)
rw <- cumsum(wn)
# Another way
# rw2 <- wn; for (t in 2:N) rw2[t] <- rw2[t-1] + wn[t]
# Check that both ways are equivalent
# all(round(rw,4)==round(rw2,4))


## @knitr Question2-b
# Provide the descriptive statistics of the simulated realizations
# mean, standard deviation, 25th, 50th, and 75th quantiles, minimum, and maximum
# desc_stat(cbind(wn, rw), c(' White noise', 'Random walk'),
#           paste('Descriptive statistics of the simulated random walk',
#                 'and the white noise that generates it'))
# See the definition of the function in ## @knitr Libraries-Functions-Constants
desc_stat(rw, 'Random walk', 
          'Descriptive statistics of the simulated random walk')


## @knitr Question2-e
# c. Plot the time-series plot of the simulated realizations
# d. Plot the autocorrelation graph
# e. Plot the partial autocorrelation graphpar(mfrow=c(2, 2))
par(mfrow=c(2, 2))
plot.ts(rw, 
        main = "Time plot of a 500 simulation\nof a zero-drift random walk", 
        ylab="Level", xlab = 'Observation', col="blue")
hist(rw, breaks = 20, col="gray", freq = FALSE, xlab = "Level", 
     main = "Histogram of a 500 simulation\nof a zero-drift random walk")
acf(rw, main="ACF of a 500 simulation\nof a zero-drift random walk")
pacf(rw, main="PACF of a 500 simulation\nof a zero-drift random walk")
par(mfrow=c(1, 1))



## @knitr Question3-a
# QUESTION 2 --------------------------------------------------------------
# Generate a random walk with drift model using 500 simulation, 
# with the drift = 0.5
drift <- 0.5 # drift
# Use the same GWN that genereate the prev. zero-drift RW
# set.seed(123); wn <- rnorm(n = N, mean = 0, sd = 1)
rw_drift <- cumsum(wn + drift)



## @knitr Question3-b
# Provide the descriptive statistics of the simulated realizations
# mean, standard deviation, 25th, 50th, and 75th quantiles, minimum, and maximum
# desc_stat(cbind(wn, rw), c(' White noise', 'Random walk'),
#           paste('Descriptive statistics of the simulated random walk',
#                 'and the white noise that generates it'))
# See the definition of the function in ## @knitr Libraries-Functions-Constants
desc_stat(cbind(rw, rw_drift), c('Random walk', 'Random walk with 0.5 drift'), 
          'Descriptive statistics of the two simulated random walks')


## @knitr Question3-e
# c. Plot the time-series plot of the simulated realizations
# d. Plot the autocorrelation graph
# e. Plot the partial autocorrelation graph
par(mfrow=c(2, 2))
plot.ts(rw_drift, 
        main = "Time plot of a 500 simulation\nof a 0.5-drift random walk", 
        ylab="Level", xlab = 'Observation', col="blue")
abline(a = 0, b = drift, lty = 2)
leg.txt <- c("Random walk with drift", "Drift")
legend("bottomright", legend = leg.txt, lty=c(1,2), col = c("blue", "black"), 
       bty = 'n', cex = .75)
hist(rw_drift, breaks = 20, col="gray", freq = FALSE, xlab = "Level", 
     main = "Histogram of a 500 simulation\nof a 0.5-drift random walk")
acf(rw_drift, main="ACF of a 500 simulation\nof a 0.5-drift random walk")
pacf(rw_drift, main="PACF of a 500 simulation\nof a 0.5-drift random walk")
par(mfrow=c(1, 1))



## @knitr Question4-a
# QUESTION 4 --------------------------------------------------------------
# Load the data and examine the basic structure of the data using `str()`, 
# `dim()`, `head()`, and `tail()` functions
# setwd('./HW6/data')
INJCJC_df <- read.csv('INJCJC.csv', header = TRUE)
str(INJCJC_df)
dim(INJCJC_df); obs <- dim(INJCJC_df)[1]
head(INJCJC_df)
tail(INJCJC_df)

## @knitr Question4-a-2
desc_stat(INJCJC_df[, -1], names(INJCJC_df)[-1], 
          'Descriptive statistics of the INJCJC variables')

## @knitr Question4-a-3
levels(as.factor(weekdays(as.Date(as.character(INJCJC_df$Date), '%d-%b-%y'))))

## @knitr Question4-a-4
INJCJC_df %>% 
  mutate(Date = as.Date(as.character(Date), '%d-%b-%y')) %>% 
  mutate(Year = year(Date)) %>% 
  group_by(Year) %>% 
  summarise(obs = n(), start_date = min(Date), end_date = max(Date)) %>% 
  print(n=Inf)
# Count weeks 
sum(sapply(1990:2014, function(y) 
  ifelse(((y %% 4 == 0) & (y %% 100 != 0)) | (y %% 400 == 0), 366, 365))) / 7
# Count Fridays in that period
ceiling(as.numeric(as.Date('2014-12-31') + 1 - 5 + 4) / 7) - 
  ceiling(as.numeric(as.Date('1990-01-01') - 5 + 4) / 7)

## @knitr Question4-a-5
INJCJC_df %>% 
  mutate(week_num = 1:obs, 
         week = ifelse(week_num %% 52== 0, 52, week_num %% 52)) %>% 
  filter(week %% 52 < 2)


## @knitr Question4-b
# Convert the variables `INJCJC` into a time series object `frequency=52, 
# start=c(1990,1,1), end=c(2014,11,28)`
# Examine the converted data series
INJCJC_wrong <- ts(INJCJC_df$INJCJC, frequency = 52, start = c(1990, 1, 1), 
                   end = c(2014, 11, 28))
length(INJCJC_wrong) # 1300 - (52 -11)
tail(INJCJC_wrong, 10)
tail(INJCJC_df$INJCJC[1:length(INJCJC_wrong)], 10)
# INJCJC_df$INJCJC[(length(INJCJC_wrong)+1):obs]

## @knitr Question4-b-2
par(mar = c(5, 6, 4, 3))
plot(as.Date(as.character(INJCJC_df$Date), '%d-%b-%y'), INJCJC_df$INJCJC, 
     col = 'blue', type = 'l', xlab = "Year (time period = weeks)", 
     ylab = "U.S. Initial Jobless Claims\n(in thousands)", 
     main = "U.S. Initial Jobless Claims\nfrom 5 Jan 1990 to 28 Nov 2014")
lines(as.Date(as.character(INJCJC_df$Date), '%d-%b-%y')[
  1:length(INJCJC_wrong)], as.numeric(INJCJC_wrong), col = 'red', lty=2)
leg.txt <- c("INJCJC", "INJCJC truncated")
legend("topleft", legend=leg.txt, lty=c(1,2), col=c("blue", "red"), 
       bty = 'n', cex = .6)

## @knitr Question4-b-3
INJCJC <- ts(INJCJC_df$INJCJC, frequency = 52, start = c(1990, 1), 
             end = c(2014, 52))
length(INJCJC)

## @knitr Question4-b-4
comp <- rbind(as.character(INJCJC_df$Date[c(1, 52, length(INJCJC_wrong))]), 
              formatC(time(INJCJC)[c(1, 52, length(INJCJC_wrong))], 3, 
                      format = 'f'), 
              format(date_decimal(time(INJCJC)[c(1, 52, length(INJCJC_wrong))]), 
                     "%d-%b-%Y %H:%M:%S"))
rownames(comp) <- c("Original date", "time(ts)", "Corresponding date")
colnames(comp) <- c(1, 52, length(INJCJC_wrong))
comp

## @knitr Question4-b-5
INJCJC_2 <- xts(INJCJC_df$INJCJC, 
                order.by = as.Date(as.character(INJCJC_df$Date), '%d-%b-%y'))
head(INJCJC_2)
par(mar = c(5, 6, 4, 3), col = "blue")
plot(INJCJC_2, xlab = "Year (time period = weeks)", 
     ylab = "U.S. Initial Jobless Claims\n(in thousands)", 
     main = "U.S. Initial Jobless Claims\nfrom 5 Jan 1990 to 28 Nov 2014")
box(col = "black"); par(col = "black")


## @knitr Question4-c
# Define a variable using the command `INJCJC.time<-time(INJCJC)`
INJCJC.time <- time(INJCJC)
head(INJCJC.time)
tail(INJCJC.time)
tail(time(INJCJC_wrong))

## @knitr Question4-d
# Using the following command to examine the first 10 rows of the data
# Change the parameter to examine different number of rows of data
head(cbind(INJCJC.time, INJCJC), 10)
head(cbind(INJCJC.time, INJCJC)) # default: 6
head(cbind(INJCJC.time, INJCJC), -(length(INJCJC)-6)) # -1294: equivalent
head(cbind(INJCJC.time, INJCJC), 13) # approximately 3 months (1 quarter)


## @knitr Question4-e-1
# Plot the time series plot of `INJCJC`
par(mar=c(5, 6, 4, 3))
plot.ts(INJCJC, col = 'blue', xlab = "Year (time period = weeks)", 
        ylab = "U.S. Initial Jobless Claims\n(in thousands)", 
        main = "U.S. Initial Jobless Claims\nfrom 5 Jan 1990 to 28 Nov 2014")

## @knitr Question4-e-2
# Plot the histogram of `INJCJC`
par(mfrow=c(3, 1))
hist(INJCJC, col="gray", freq = FALSE, 
     xlab = "U.S. Initial Jobless Claims (in thousands, 9 bins)", 
     main = paste0("Histogram of the U.S. Initial Jobless Claims", 
                   "from 5 Jan 1990 to 28 Nov 2014"))
hist(INJCJC, breaks = 50, col="gray", freq = FALSE, 
     xlab = "U.S. Initial Jobless Claims (in thousands, 50 bins)", 
     main = paste0("Histogram of the U.S. Initial Jobless Claims", 
                   "from 5 Jan 1990 to 28 Nov 2014"))
hist(INJCJC, breaks = 500, col="gray", freq = FALSE, 
     xlab = "U.S. Initial Jobless Claims (in thousands, 500 bins)", 
     main = paste0("Histogram of the U.S. Initial Jobless Claims", 
                   "from 5 Jan 1990 to 28 Nov 2014"))
par(mfrow=c(1, 1))


## @knitr Question4-e-3
# Plot the autocorrelation graph of `INJCJC` series
acf(INJCJC, lag.max = 52, 
    main="ACF of U.S. Initial Jobless Claims\nfrom 5 Jan 1990 to 28 Nov 2014")


## @knitr Question4-e-4
# Plot the partial autocorrelation graph of `INJCJC` series
pacf(INJCJC, lag.max = 52, 
     main="PACF of U.S. Initial Jobless Claims\nfrom 5 Jan 1990 to 28 Nov 2014")


## @knitr Question4-e-5
# Plot a 3x3 Scatterplot Matrix of correlation against lag values
lag.plot(INJCJC, lags = 9, layout = c(3, 3), diag=TRUE, diag.col="red", 
         main = paste("Autocorr. between the INJCJC time series and its", 
                      "Own Lags"))
# par(mfrow=c(3, 3))
# for (i in 1:9) {
#   plot(INJCJC[1:(length(INJCJC) - i)], INJCJC[(i+1):length(INJCJC)], asp = 1, 
#        xlab = paste("Lag", i), ylab = "INJCJC")
#   abline(h=mean(INJCJC[1:(length(INJCJC) - i)]))
#   abline(v=mean(INJCJC[(i+1):length(INJCJC)]))
#   abline(lm(INJCJC[(i+1):length(INJCJC)] ~ INJCJC[1:(length(INJCJC)-i)]), col = 'red')
# }
# par(mfrow=c(1, 1))


## @knitr Question4-f-1
# Generate two symmetric Moving Average Smoothers
# Choose the number of moving average terms such that one of the smoothers is 
# very smoother and the other one can trace through the dynamics of the series
# Plot the smoothers and the original series in one graph
INJCJC.1 = stats::filter(INJCJC, sides=2, rep(1, 5)/5)
INJCJC.2 = stats::filter(INJCJC, sides=2, rep(1, 53)/53)
par(mar = c(5, 6, 4, 3))
plot(INJCJC, pch = 4, lty = 5, lwd=1, xlab = "Year (time period = weeks)", 
     ylab = "U.S. Initial Jobless Claims\n(in thousands)", 
     main = paste0("U.S. Initial Jobless Claims\nfrom 5 Jan 1990 to 28 Nov", 
                   "2014\nplus two Moving Average Smoothers"))
lines(INJCJC.1, lty = 1, lwd = 1.5, col = "green")
lines(INJCJC.2, lty = 1, lwd = 1.5, col = "blue")
leg.txt <- c("Original Series", "5-Point Symmetric Moving Average", 
             "53-Point Symmetric Moving Average")
legend("topleft", legend=leg.txt, lty = c(1, 1, 1), 
       col=c("black", "green", "blue"), bty = 'n', cex=1, merge = TRUE, 
       bg = 336)

## @knitr Question4-f-2
# Generate two regression smoothers, one being a cubic trend regression and the 
# other being a periodic regression
# Plot the smoothers and the original series in one graph
cbind(INJCJC.time[c(1:2, (length(INJCJC)-1):length(INJCJC))], 
      mean(INJCJC.time))
wk = INJCJC.time - mean(INJCJC.time)
wk2 = wk^2
wk3 = wk^3
cs = cos(2 * pi * wk)  
sn = sin(2 * pi * wk)
reg1 = lm(INJCJC ~ wk + wk2 + wk3, na.action = NULL)
reg2 = lm(INJCJC ~ wk + wk2 + wk3 + cs + sn, na.action = NULL)
par(mar = c(5, 6, 4, 3))
plot(INJCJC, pch=4, lty=5, lwd=1, xlab = "Year (time period = weeks)", 
     ylab = "U.S. Initial Jobless Claims\n(in thousands)", 
     main = paste0("U.S. Initial Jobless Claims\nfrom 5 Jan 1990 to 28 Nov", 
                   "2014\nplus two Regression Smoothers"))
lines(fitted(reg1), lty=1, lwd=1.5, col="green")
lines(fitted(reg2), lty=1, lwd=1.5, col="blue")
# Add Legend
leg.txt <- c("Original Series", "Cubic Trend Regression Smoothing", 
             "Periodic Regression Smoothing")
legend("topleft", legend = leg.txt, lty = c(1, 1, 1), 
       col = c("black", "green", "blue"), bty = 'n', cex = 1, merge = TRUE, 
       bg = 336)

## @knitr Question4-f-3
# Generate kernel smoothers
# Choose the smoothing parametrs such that one of the smoothers is very 
# smoother and the other one can trace through the dynamics of the series
# Plot the smoothers and the original series in one graph.
INJCJC.1 <- ksmooth(INJCJC.time, INJCJC, "normal", bandwidth = 5/52)
INJCJC.2 <- ksmooth(INJCJC.time, INJCJC, "normal", bandwidth = 2)
par(mar = c(5, 6, 4, 3))
plot(INJCJC, pch=4, lty=5, lwd=1, xlab = "Year (time period = weeks)", 
     ylab = "U.S. Initial Jobless Claims\n(in thousands)", 
     main = paste0("U.S. Initial Jobless Claims\nfrom 5 Jan 1990 to 28 Nov", 
                   "2014\nplus two Kernel Smoothers"))
lines(INJCJC.1, lty = 1, lwd = 1.5, col = "green")
lines(INJCJC.2, lty = 1, lwd = 1.5, col = "blue")
leg.txt <- c("Original Series", "Kernel Smoothing: bandwidth=5/52", "Kernel Smoothing: bandwidth=2")
legend("topleft", legend = leg.txt, lty = c(1, 1, 1), 
       col = c("black", "green", "blue"), bty = 'n', cex = 1, merge = TRUE, 
       bg = 336)

## @knitr Question4-f-4
# Generate two nearest neighborhood smoothers
# Choose the smoothing parameters such that one of the smoothers is very 
# smoother and the other one can trace through the dynamics of the series
# Plot the smoothers and the original series in one graph
INJCJC.1 <- supsmu(INJCJC.time, INJCJC, span = .01)
INJCJC.2 <- supsmu(INJCJC.time, INJCJC, span = .1)
par(mar = c(5, 6, 4, 3))
plot(INJCJC, pch=4, lty=5, lwd=1, xlab = "Year (time period = weeks)", 
     ylab = "U.S. Initial Jobless Claims\n(in thousands)", 
     main = paste0("U.S. Initial Jobless Claims\nfrom 5 Jan 1990 to 28 Nov", 
                   "2014\nplus two NN Smoothers"))
lines(INJCJC.1, lty = 1, lwd = 1.5, col = "green")
lines(INJCJC.2, lty = 1, lwd = 1.5, col = "blue")
leg.txt <- c("Original Series", "NN Smoothing: bandwidth=.01", 
             "NN Smoothing: bandwidth=.1")
legend("topleft", legend = leg.txt, lty = c(1, 1, 1), 
       col = c("black", "green", "blue"), bty = 'n', cex = 1, merge = TRUE, 
       bg = 336)

## @knitr Question4-f-5
# Generate two LOWESS smoothers
# Choose the smoothing parameters such that one of the smoothers is very 
# smoother and the other one can trace through the dynamics of the series
# Plot the smoothers and the original series in one graph.**
INJCJC.1 <- lowess(INJCJC, f = .02)
INJCJC.2 <- lowess(INJCJC, f = .2)
par(mar = c(5, 6, 4, 3))
plot(INJCJC, pch=4, lty=5, lwd=1, xlab = "Year (time period = weeks)", 
     ylab = "U.S. Initial Jobless Claims\n(in thousands)", 
     main = paste0("U.S. Initial Jobless Claims\nfrom 5 Jan 1990 to 28 Nov", 
                   "2014\nplus two LOWESS Smoothers"))
lines(INJCJC.1, lty = 1, lwd = 1.5, col = "green")
lines(INJCJC.2, lty = 1, lwd = 1.5, col = "blue")
leg.txt <- c("Original Series", "LOWESS Smoothing: bandwidth=.02", 
             "LOWESS Smoothing: bandwidth=.2")
legend("topleft", legend = leg.txt, lty = c(1, 1, 1), 
       col = c("black", "green", "blue"), bty = 'n', cex = 1, merge = TRUE, 
       bg = 336)

## @knitr Question4-f-6
# Generate two spline smoothers
# Choose the smoothing parameters such that one of the smoothers is very 
# smoother and the other one can trace through the dynamics of the series
# Plot the smoothers and the original series in one graph
INJCJC.1 <- smooth.spline(INJCJC.time, INJCJC, spar = 0.05)
INJCJC.2 <- smooth.spline(INJCJC.time, INJCJC, spar = 0.8)
par(mar = c(5, 6, 4, 3))
plot(INJCJC, pch=4, lty=5, lwd=1, xlab = "Year (time period = weeks)", 
     ylab = "U.S. Initial Jobless Claims\n(in thousands)", 
     main = paste0("U.S. Initial Jobless Claims\nfrom 5 Jan 1990 to 28 Nov", 
                   "2014\nplus two Spline Smoothers"))
lines(INJCJC.1, lty = 1, lwd = 1.5, col = "green")
lines(INJCJC.2, lty = 1, lwd = 1.5, col = "blue")
leg.txt <- c("Original Series", "Spline: Smoothing Parameter=.05", 
             "Spline: Smoothing Parameter=0.8")
legend("topleft", legend = leg.txt, lty = c(1, 1, 1), 
       col = c("black", "green", "blue"), bty = 'n', cex = 1, merge = TRUE, 
       bg = 336)

