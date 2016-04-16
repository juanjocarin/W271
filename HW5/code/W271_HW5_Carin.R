## MIDS W271-4 HW5            ##
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
# library(dplyr)
# library(stargazer)
# library(texreg)
library(weatherData)
library(scales)
library(xts)
library(reshape2)

# Define functions

# THE FOLLOWING FUNCTIONS ARE JUST FOR FORMATTING PURPOSES

# A function to apply format
frmt <- function(qty, digits = 3) {
  formatC(qty, digits = digits, format = "f", drop0trailing = FALSE, 
          big.mark = ",")
}

# Define constants



## @knitr Question1-1
# QUESTION 1 --------------------------------------------------------------
# Install the library "astsa" using the function: install.packages("astsa")
# Check if already installed; if not, install it
if (!"astsa" %in% installed.packages()[, "Package"]) install.packages("astsa")

## @knitr Question1-2
# Load the library: library(astsa)
library(astsa)
# Last two commands can be substituted by simply...
if (!require(astsa)) install.packages("astsa")

## @knitr Question1-3
# Use the function str() to see the information of a particular data series, 
# such as str(EQ5) for the Seismic Trace of Earthquake number 5 series
str(EQ5)
str(flu)
str(gas)

## @knitr Question1-4-1a
# Plot the time series plots and histograms of the following 3 series.
# EQ5, flu, gas
# plot(EQ5, main = "Time series plot of seismic trace", col = 'blue', 
#      ylab = "Amplitude of seismic trace")
EQ5_df <- data.frame(Time = as.numeric(time(EQ5)), Amplitude = as.numeric(EQ5))
ggplot(EQ5_df, aes(Time, Amplitude)) + 
  geom_line(colour = 'blue') + 
  labs(y = 'Amplitude of sesismic trace', 
       title = 'Time series plot of seismic trace')

## @knitr Question1-4-1b
# hist(EQ5, breaks = 60, main = 'Histogram of amplitude of seismic trace', 
#      xlab = 'Amplitude of seismic trace', ylab = 'Count of observations')
ggplot(EQ5_df, aes(Amplitude)) + 
  geom_histogram(bins = 60, color = 'black', fill = 'white') + 
  labs(x = 'Amplitude of seismic trace', y = 'Count of observations', 
       title = 'Histogram of amplitude of seismic trace')

## @knitr Question1-4-2a
# plot(flu, 
#      main = "Time series plot of pneumonia and influenza deaths in the U.S.", 
#      ylab = "Number of deaths per 10,000 people", 
#      xlab = 'Year', col = 'blue')
flu_df <- data.frame(Year = as.Date(time(flu)), Deaths = as.numeric(flu))
ggplot(flu_df, aes(Year, Deaths)) + 
  geom_line(colour = 'blue') + 
  labs(y = 'Number of deaths\nper 10,000 people', 
       title = paste('Time series plot of pneumonia\nand influenza deaths', 
                     'in the U.S.'))

## @knitr Question1-4-2b
# hist(flu, breaks = 60, 
#      main = 'Histogram of pneumonia and influenza deaths in the U.S.',
#      xlab = 'Number of deaths per 10,000 people', 
#      ylab = 'Count of observations')
ggplot(flu_df, aes(Deaths)) + 
  geom_histogram(bins = 60, color = 'black', fill = 'white') + 
  labs(x = 'Number of deaths\nper 10,000 people', y = 'Count of observations', 
       title = 'Histogram of pneumonia and\ninfluenza deaths in the U.S.')

## @knitr Question1-4-3a
# plot(gas, main = "Time series plot of weekly gas prices", 
#      ylab = "Price (in cents per gallon)", xlab = 'Year', col = 'blue')
gas_df <- data.frame(Year = as.numeric(time(gas)), 
                     Price = as.numeric(gas))
ggplot(gas_df, aes(Year, Price)) + 
  geom_line(colour = 'blue') + 
  labs(y = 'Price (in cents per gallon)', 
       title = 'Time series plot of weekly gas prices') + 
  scale_x_continuous(breaks = pretty_breaks())


## @knitr Question1-4-3b
# hist(gas, breaks = 60, 
#      main = 'Histogram of weekly gas prices',
#      xlab = 'Price (in cents per gallon)', 
#      ylab = 'Count of observations')
ggplot(gas_df, aes(Price)) + 
  geom_histogram(bins = 60, color = 'black', fill = 'white') + 
  labs(x = 'Price (in cents per gallon)', y = 'Count of observations', 
       title = 'Histogram of weekly gas prices')



## @knitr Question2-1
# QUESTION 2 --------------------------------------------------------------
# Describe 3 examples you have used in your work or encounter in real life. 
# Ideally, you can even load at least one of these time series, plot it, 
# and the write a few statements to describe its characteristics.
# Plot some biotech stocks to watch in 2016.
# Define the variable to get access to the yahoo finacial stock data 
biogen_stock_url <- "http://real-chart.finance.yahoo.com/table.csv?s=BIIB
&a=07&b=24&c=2010&d=07&e=24&f=2015&g=d&ignore=.csv"
mdvn_stock_url <- "http://real-chart.finance.yahoo.com/table.csv?s=MDVN
&a=07&b=24&c=2010&d=07&e=24&f=2015&g=d&ignore=.csv"
lexicon_stock_url <- "http://real-chart.finance.yahoo.com/table.csv?s=LXRX
&a=07&b=24&c=2010&d=07&e=24&f=2015&g=d&ignore=.csv"
gilead_stock_url <- "http://real-chart.finance.yahoo.com/table.csv?s=GILD
&a=07&b=24&c=2010&d=07&e=24&f=2015&g=d&ignore=.csv"
enanta_stock_url <- "http://real-chart.finance.yahoo.com/table.csv?s=ENTA
&a=07&b=24&c=2010&d=07&e=24&f=2015&g=d&ignore=.csv"
celgen_stock_url <- "http://real-chart.finance.yahoo.com/table.csv?s=CELG
&a=07&b=24&c=2010&d=07&e=24&f=2015&g=d&ignore=.csv"
# Define function to read financial data through url
yahoo.read <- function(url, variable) {
  dat <- read.table(url, header = TRUE, sep = ",")
  df <- dat[ ,c(1, 5)]
  names(df) <- c('Date', variable)
  df$Date <- as.Date(as.character(df$Date))
  return(df)
}
# Collect the stock data from 2010 to 2016 for those companies 
biogen <- yahoo.read(biogen_stock_url, 'Biogen')
medivation <- yahoo.read(mdvn_stock_url, 'Medivation')
lexicon <- yahoo.read(lexicon_stock_url, 'Lexicon')
gilead <- yahoo.read(gilead_stock_url, 'Gilead')
enanta <- yahoo.read(enanta_stock_url, 'Enanta')
enanta2 <- data.frame(Date = biogen$Date)
enanta2$Enanta = ifelse(biogen$Date %in% enanta$Date, 
                        enanta$Enanta, NA)
celgen <- yahoo.read(celgen_stock_url, 'Celgen')
# Time series plot for those stocks 
whole_dataset <- cbind(biogen, medivation, lexicon, gilead, enanta2, celgen)
whole_dataset <- melt(whole_dataset, variable.name = 'Company', 'Date')
ggplot(whole_dataset, aes(x = Date, y = value, colour = Company)) + 
  geom_line() + 
  ggtitle("Biotech. Stocks to watch in 2016") +
  labs(y = "Closing Stock Price ($)")

## @knitr Question2-2
# The daily tempreture of 2015 at JFK airport.
# Get access to the weather data through weatherdata package 
# (need the scales package as well) 
W_KJFK_2015 <- getWeatherForYear("KJFK", 2015)
W_KJFK_2015$Date <- as.Date(W_KJFK_2015$Date, format = "%y-%m-%d")
ggplot(W_KJFK_2015, aes(Date, Mean_TemperatureF)) + 
  geom_line(colour = 'blue') + 
  scale_x_date(labels = date_format("%y/%m/%d")) + 
  labs(x = 'Date', y = "Mean Temp in ºF", 
       title = "Daily average temperature\nat JFK airport in 2015")

## @knitr Question2-3
# The daily electricity usage for every month during 2014 Jan. and 2015 Dec.
# Load monthly averaged electricity usage for months in 2014 and 2015
elec_usage = c(35.94, 29.68, 31.83, 31.36, 24.61, 17.91, 18.29, 17.74, 15.70, 
               19.33, 23.90, 23.39, 23.30, 28.63, 29.07, 24.27, 20.68, 18.33, 
               18.42, 18.41, 24.66, 18.93, 23.03, 23.18)
elec_usage_ts <- ts(elec_usage, start=c(2014, 1), end=c(2015, 12), frequency=12) 
elec_usage_df <- data.frame(Date = as.Date(time(elec_usage_ts)), 
                             Price = as.numeric(elec_usage_ts))
ggplot(elec_usage_df, aes(Date, Price)) + 
  geom_line(colour = 'blue') + 
  labs(x = 'Date',  y = 'Daily Electricity Usage (KWh)', 
       title = 'Monthyl Electricity Usage in 2014 & 2015') + 
  scale_x_date(date_labels = "%b%Y")



## @knitr Question3-1
# QUESTION 3 --------------------------------------------------------------
# Simulate a white noise series with 1000 random draws and plot 
# (1) a time series plot and
# (2) a histogram.
N <- 1000
wn <- data.frame(Time = 1:N, Level = rnorm(N))
ggplot(wn, aes(Time, Level)) + 
  geom_line(colour = 'blue') + 
  labs(y = 'Level', x = 'Observation', 
       title = 'White noise time series plot')

## @knitr Question3-2
ggplot(wn, aes(Level)) + 
  geom_histogram(bins = 60, color = 'black', fill = 'white') + 
  labs(x = 'White noise level', y = 'Count of observations', 
       title = 'Histogram of white noise')



## @knitr Question4-1
# QUESTION 4 --------------------------------------------------------------
# Simulate (with 1000 random draws) the following two zero-mean 
# autoregressive model with order 1 (i.e. AR(1)) models:
# yt = 0.9yt1 + w
# yt = 0.2yt1 + w
# Plot a time plot for each of the simulated series. 
# Graph a histogram for each of the simulated series.
generate_AR <- function(wn, coef) {
  y <- wn
  for (i in 2:length(wn)) {
  y$Level[i] <- y$Level[i] + coef * y$Level[i - 1]
  }
  return(y)
}
y1 <- generate_AR(wn, 0.9)
ggplot(y1, aes(Time, Level)) + 
  geom_line(colour = 'blue') + 
  labs(y = 'Level', x = 'Observation', 
       title = 'Plot of a simulation of an\nAR(1) model with coef. 0.9')

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

