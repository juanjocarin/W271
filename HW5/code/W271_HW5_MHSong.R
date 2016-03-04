## MIDS W271-4 HW5            ##
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
library(xts)
library(weatherData)
library(scales)


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

## @knitr Question1-4-1
# Time series and histogram Plots for the EQ5 seismic trace
autoplot(EQ5, main='Seismic Trace of EQ5', ts.colour= 'dodgerblue2', 
         xlab='Time', ylab='Amplititue')
qplot(EQ5, geom="histogram",  main='Histogram of EQ5', xlab='Amplitude',
      ylab='Frequency', colour = I('dodgerblue3'), fill = I("white") )

## @knitr Question1-4-2
# Time series and histogram Plots for the flu data series
autoplot(flu, main='Flu Time Series Plot', ts.colour = 'dodgerblue2',
         xlab='Time', ylab='Deaths Per 10,000 People' )
qplot(flu, geom="histogram",  main='Histogram of Flu Series', xlab='Monthly Deaths per 10000 People',
      ylab='Frequency', colour = I('dodgerblue3'), fill = I("white") )

## @knitr Question1-4-3
# Time series and histogram plots for the gas price series 
autoplot(gas, main='Gas Prices Time Series Plot',  ts.colour = 'dodgerblue2', xlab='Weeks in 2000-2010',
         ylab='Gasoline Price (cents per gallon)')
qplot(gas, geom="histogram",  main='Histogram of Gas Series', xlab='Weeks in 2000-2010',
      ylab='Frequency', colour = I('dodgerblue3'), fill = I("white") )


## @knitr Question1-5
# Write a few sentences to describe each of the series: EQ5, flu, and gas


## @knitr Question2
# QUESTION 2 --------------------------------------------------------------
# Describe 3 examples you have used in your work or encounter in real life. 
# Ideally, you can even load at least one of these time series, plot it, 
# and the write a few statements to describe its characteristics.

## @knitr Question2-1
# Plot some biotech stocks to watch in 2016.
# define the variable to get access to the yahoo finacial stock data 
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

# define function to read financial data through url
yahoo.read <- function(url){
  dat <- read.table(url,header=TRUE,sep=",")
  df <- dat[,c(1,5)]
  df$Date <- as.Date(as.character(df$Date))
  return(df)}

# grap the stock data from 2010 to 2016 for those companies 
biogen  <- yahoo.read(biogen_stock_url)
medivation <- yahoo.read(mdvn_stock_url)
lexicon <- yahoo.read(lexicon_stock_url)
gilead <- yahoo.read(gilead_stock_url)
enanta <- yahoo.read(enanta_stock_url)
celgen <- yahoo.read(celgen_stock_url)

# time series plot for those stocks 
ggplot(biogen,aes(Date,Close)) +  
  geom_line(aes(color="biogen")) +
  geom_line(data=medivation,aes(color="medivation")) +
  geom_line(data=lexicon,aes(color="lexicon")) +
  geom_line(data=gilead,aes(color="gilead")) +
  geom_line(data=enanta,aes(color="enanta")) +
  geom_line(data=celgen,aes(color="celgen")) +
  labs(color="Legend") +
  scale_colour_manual("", breaks = c("biogen", "medivation","lexicon","gilead","enanta","celgen"),
                      values = c("red", "brown", "green", "blue","purple","black")) +
  ggtitle("Biotech. Stocks to Watch in 2016") +
  theme(plot.title = element_text(lineheight=.7, face="bold")) +
  labs(y = "Stock Closing Price ($)")

## @knitr Question2-2
# the daily tempreture of 2015 at JFK airport.
# get access to the weather data through weatherdata package (need the scales package as well) 
W_KJFK_2015 <- getWeatherForYear("KJFK",2015)
W_KJFK_2015$Date <- as.Date(W_KJFK_2015$Date,format="%y-%m-%d")

ggplot(W_KJFK_2015, aes(Date, Mean_TemperatureF)) + geom_line(colour = 'dodgerblue2') +
  scale_x_date(labels=date_format("%y/%m/%d")) + xlab("") + ylab("Mean Temp deg F") +
  ggtitle("2015 Averaged Daily Temp. at JFK")

## @knitr Question2-3
# the daily electricity usage for every month during 2014 Jan. and 2015 Dec.
# load monthyl averaged electricity usage for months in 2014 and 2015
elec_usage = c(35.94, 29.68, 31.83, 31.36, 24.61, 17.91, 18.29, 17.74, 15.70, 
              +19.33, 23.90, 23.39, 23.30, 28.63, 29.07, 24.27, 20.68, 18.33,
              +18.42, 18.41, 24.66, 18.93, 23.03, 23.18)
elec_usage_ts <- ts(elec_usage, start=c(2014, 1), end=c(2015, 12), frequency=12) 
autoplot(elec_usage_ts, main='Monthyl Electricity Usage in 2014 & 2015',geom = "bar",xlab='Time',
        ylab='Daily Electricity Usage (KWh)',colour = I('dodgerblue3'),fill = I("white"))



## @knitr Question3
# QUESTION 3 --------------------------------------------------------------
# Simulate a white noise series with 1000 random draws and plot 
# (1) a time series plot and
# (2) a histogram.
rand_draw <- rnorm(1000) # 1000 random draw
rand_draw_ts <- ts(rand_draw)
autoplot(rand_draw_ts , xlab = "Simulated Time Period", ylab = "Simulated Values", 
         main="Simulated White Noise", ts.colour = 'dodgerblue2', ylim=c(-6,6) )
qplot(rand_draw_ts, geom="histogram",  main='Histogram of Simulated White Noise',ylab='Frequency',
      xlab='Simulated Values', colour = I('dodgerblue3'), fill = I("white"), xlim=c(-6,6) )


## @knitr Question4-1
# QUESTION 4 --------------------------------------------------------------
# Simulate (with 1000 random draws) the following two zero-mean 
# autoregressive model with order 1 (i.e. AR(1)) models:
# yt = 0.9yt1 + w
# yt = 0.2yt1 + w
# Plot a time plot for each of the simulated series. 
# Graph a histogram for each of the simulated series.

# Generate two simulated time series
z1 <- rand_draw 
for (t in 2:length(rand_draw)){ 
  z1[t] <- 0.9 * z1[t-1] + z1[t] # use the same random normal sequence generated above
}
z1_ts <- ts(z1)
z2 <- rand_draw 
for (t in 2:length(rand_draw)){ 
  z2[t] <- 0.2 * z2[t-1] + z2[t] # use the same random normal sequence generated above
}
z2_ts <- ts(z2)

## @knitr Question4-2
# the time series plot and histogram of 1st series 
#par(mfrow=c(1,2))
autoplot(z1_ts , xlab = "Simulated Time Period", ylab = "Simulated Values", 
         main="AR(1) with alpha=0.9 ", ts.colour = 'dodgerblue2', ylim=c(-6,6) )
qplot(z1, geom="histogram",  main='Histogram of AR(1) with alpha=0.9', ylab='Frequency',
      xlab='Simulated Values', colour = I('dodgerblue3'), fill = I("white"), xlim=c(-6,6)) 

## @knitr Question4-3
# the time series plot and histogram of 2nd series
#par(mfrow=c(1,2))
autoplot(z2_ts , xlab = "Simulated Time Period", ylab = "Simulated Values", 
         main="AR(1) with alpha=0.2", ts.colour = 'dodgerblue2', ylim=c(-6,6))
qplot(z2_ts, geom="histogram",  main='Histogram of AR(1) with alpha=0.2', ylab='Frequency',
      xlab='Simulated Values', colour = I('dodgerblue3'), fill = I("white"),xlim=c(-6,6) ) 


## @knitr Question5
# QUESTION 5 --------------------------------------------------------------
# Simulate (with 1000 random draws) the following 3 models:
# 1. A deterministic linear (time) trend of the form: yt = 10 + 0.5t
# 2. Random walk without drift
# 3. Random walk with drift = 0.5
# Plot a time plot for each of the simulated series. 
# Graph a histogram for each of the simulated series.
