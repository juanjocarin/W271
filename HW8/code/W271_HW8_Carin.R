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
library(car)
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
hw08_df_mv <- hw08_df %>% 
  group_by(Year) %>% summarise(Mean = mean(Level), Variance = var(Level))
kable(cbind(hw08_df_mv %>% filter(Year %in% factor(1980:1989)), 
            hw08_df_mv %>% filter(Year %in% factor(1990:1999)), 
            hw08_df_mv %>% filter(Year %in% factor(2000:2009))), digits = 2, 
      caption = paste("Variance of the time-series amplitude per year ", 
                      "(for the first 30 out of 31)."))

## @knitr ex1-7
plot(decompose(hw08.ts, type = 'additive'), col = 'blue', 
     xlab = "Year (time period: month)")
# pacf(na.omit(decompose(hw08.ts, type = 'additive')$random), lag.max = 24)
# plot(stl(hw08.ts, s.window="periodic"), col = 'blue')


M <- factor(cycle(hw08.ts))
reg <- lm(hw08.ts ~ time(hw08.ts) + I(time(hw08.ts)^2)+ M)
plot(reg$residuals)

library(forecast)
m2 <- tslm(hw08.ts~trend)
f <- forecast(m2, h=5,level=c(80,95))
plot(f)
lines(fitted(m2))
summary(m2)
d <- data.frame(x = hw08, time = time(hw08.ts))
m <- lm(x~time, d)
plot(hw08.ts)
abline(m)

library(car)
linearHypothesis(reg, sapply(c(2:12), function(i) paste0("M", i)))


