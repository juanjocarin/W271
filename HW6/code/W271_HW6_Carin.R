## MIDS W271-4 HW6            ##
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
library(pander)
# library(texreg)
library(weatherData)
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
INJCJC <- read.csv('INJCJC.csv', header = TRUE)
str(INJCJC)
dim(INJCJC); obs <- dim(INJCJC)[1]
head(INJCJC)
tail(INJCJC)
desc_stat(INJCJC[, -1], names(INJCJC)[-1], 
          'Descriptive statistics of the INJCJC variables')
INJCJC %>% 
  mutate(Date = as.Date(as.character(Date), '%d-%b-%y')) %>% 
  mutate(Year = year(Date)) %>% 
  group_by(Year) %>% 
  summarise(obs = n(), start_date = min(Date), end_date = max(Date)) %>% 
  print(n=Inf)
INJCJC %>% 
  mutate(week = 1:obs) %>% 
  filter(week %% 52 < 2)


## @knitr Question4-b
# Convert the variables `INJCJC` into a time series object `frequency=52, 
# start=c(1990,1,1), end=c(2014,11,28)`
# Examine the converted data series
INJCJC4 <- ts(INJCJC$INJCJC4, frequency = 52, start = c(1990, 1, 1), 
              end = c(2014, 11, 28))
INJCJC2 <- INJCJC %>% 
  mutate(Date = as.Date(as.character(Date), '%d-%b-%y'))
INJCJC2 <- ts(INJCJC2$INJCJC, frequency = 52, start = c(1990, 1, 1))
plot.ts(INJCJC, col = 'blue')
lines(INJCJC4, col = 'red', lty = 2)
leg.txt <- c("INJCJC", "INJCJC4")
legend("topright", legend=leg.txt, lty=c(1,2), col=c("blue", "red"), 
       bty = 'n', cex = .75)
# plot.ts(cbind(INJCJC, INJCJC4), main = 'Time', col = 'blue')

## @knitr Question4-b-2
plot.ts(window(INJCJC, start = c(2014, 1), end = c(2015,52)), col = 'blue', 
        ylab = 'INJCJC')
lines(INJCJC4, col='red', lty = 2)
leg.txt <- c("INJCJC", "INJCJC4")
legend("topright", legend=leg.txt, lty=c(1,2), col=c("blue", "red"), 
       bty = 'n', cex = .75)

time(INJCJC)
# r = 'blue') + 
#   labs(y = 'Level', x = 'Observation', 
#        title = 'Plot of a simulation of an\nAR(1) model with coef. 0.9')

## @knitr Question4-2
ggplot(y1, aes(Level)) + 
  geom_histogram(bins = 60, color = 'black', fill = 'white') + 
  labs(x = 'AR(1, 0.9) level', y = 'Count of observations', 
       title = 'Histogram ofa simulation of\nan AR(1) model with coef. 0.9')

## @knitr Question4-3
y2 <- generate_AR(wn, 0.2)
ggplot(y2, aes(Time, Level)) + 
  geom_line(colour = 'blue') + 
  labs(y = 'Level', x = 'Observation', 
       title = 'Plot of a simulation of an\nAR(1) model with coef. 0.2')

## @knitr Question4-4
ggplot(y2, aes(Level)) + 
  geom_histogram(bins = 60, color = 'black', fill = 'white') + 
  labs(x = 'AR(1, 0.9) level', y = 'Count of observations', 
       title = 'Histogram of a simulation of\nan AR(1) model with coef. 0.2')



## @knitr Question5-1
# QUESTION 5 --------------------------------------------------------------
# Simulate (with 1000 random draws) the following 3 models:
# 1. A deterministic linear (time) trend of the form: yt = 10 + 0.5t
# 2. Random walk without drift
# 3. Random walk with drift = 0.5
# Plot a time plot for each of the simulated series. 
# Graph a histogram for each of the simulated series.
trend_drift <- 0.5
y1 <- y2 <- y3 <- data.frame(Time = 1:N)
y1$'Trend of slope 0.5' <- 10 + trend_drift * 1:N
y2$'Random walk' <- cumsum(wn$Level)
y3$'Random walk with drift of 0.5' <- cumsum(wn$Level + trend_drift)
whole_dataset <- cbind(y1, y2, y3)
whole_dataset <- melt(whole_dataset, variable.name = 'Series', 'Time')
ggplot(whole_dataset, aes(x = Time, y = value, colour = Series)) + 
  geom_line() + 
  ggtitle("Time plot of the\nthree simulated series") +
  labs(y = "Level", x = 'Observation') + 
  scale_colour_discrete("Simulated series")

## @knitr Question5-2
ggplot(whole_dataset, aes(value)) +
  facet_wrap(~ Series, scales = "free", nrow = 3) +
  geom_histogram(bins = 100, color = 'black', fill = 'white') + 
  labs(x = 'Level', y = 'Count of observations', 
       title = "Histogram of the three simulated series")

## @knitr Question5-3
ggplot(whole_dataset, aes(value)) +
  facet_wrap(~ Series, nrow = 3) +
  geom_histogram(bins = 100, color = 'black', fill = 'white') + 
  labs(x = 'Level', y = 'Count of observations', 
       title = "Histogram of the three simulated series\n(same scale)")

