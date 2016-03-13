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
library(scales)
library(weatherData)
library(xts)
library(reshape2)
library(astsa)

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




## @knitr Question2-1
# QUESTION 2 --------------------------------------------------------------
x<-w<-rnorm(500)
for (t in 2:500) x[t] <- x[t-1] + w[t]


## @knitr Question2-2
mean(x)
sd(x)
min(x)
max(x)
quantile(x, probs=c(.25, .5, .75))


## @knitr Question2-3
plot(x, type= "l")

## @knitr Question2-4
acf(x, main="ACF Zero-drift random walk")

## @knitr Question2-5
pacf(x, main ="PACF Zero-drift random walk")



## @knitr Question3-1
x1<-w1<-rnorm(500)
d<-.5
for (t in 2:500) x1[t] <- x1[t-1] + w1[t] + d


## @knitr Question3-2
mean(x1)
sd(x1)
min(x1)
max(x1)
quantile(x1, probs=c(.25, .5, .75))


## @knitr Question3-3
plot(x1, type= "l")

## @knitr Question3-4
acf(x1, main="ACF drift random walk")

## @knitr Question3-5
pacf(x1, main ="PACF drift random walk")

## @knitr Question4-1
INJCJC <- read.table("INJCJC.csv", header = TRUE, sep=",")
str(INJCJC)
dim(INJCJC)
head(INJCJC)
tail(INJCJC)

## @knitr Question4-2
INJCJC_ts<- ts(INJCJC$INJCJC,  start=c(1990,1,1), end=c(2014,11,28), frequency=52)
str(INJCJC_ts)
dim(INJCJC_ts)
head(INJCJC_ts)
tail(INJCJC_ts)

## @knitr Question4-3
INJCJC_ts.time<-time(INJCJC_ts)

## @knitr Question4-4
head(cbind(INJCJC_ts.time, INJCJC_ts),10)

## @knitr Question4-5-1
autoplot(INJCJC_ts , xlab = "Time Period", ylab = "Number of Filed Jobless Claims",
         main="Initial Unemployment Claims", ts.colour = 'dodgerblue2' )

## @knitr Question4-5-2
qplot(INJCJC_ts , geom="histogram",  main='Histogram of Filed Jobless Claims',
      ylab='Frequency', xlab='Time Period', colour = I('dodgerblue3'),
      fill = I("white") ) 

## @knitr Question4-5-3
autoplot(acf(INJCJC_ts, plot = FALSE), xlab = "Lag", main="Autocorrelation of INHCJC_ts",
         ts.colour = 'dodgerblue2')

## @knitr Question4-5-4
autoplot(pacf(INJCJC_ts, plot = FALSE), xlab = "Lag",main="Partial Autocorrelation of INHCJC_ts",
           colour = "dodgerblue3", conf.int.colour = "red")

## @knitr Question4-5-5
gglagplot(INJCJC_ts, lags = 9, nrow = 3, ncol = 3)

## @knitr Question4-6-1
INJCJC_ts_rep5 = filter(INJCJC_ts, sides=2, rep(1,5)/5)
INJCJC_ts_rep52 = filter(INJCJC_ts, sides=2, rep(1,52)/52)
plot(INJCJC_ts, main="Moving Average Smoothing for Initial Unemployment Claims", 
     pch=4, lty=5, lwd=1, xlab="Year", 
     ylab="Number of Filed Jobless Claims")
lines(INJCJC_ts_rep5, lty=1, lwd=1.5, col="green")
lines(INJCJC_ts_rep52, lty=1, lwd=1.5, col="blue")
# Add Legend
leg.txt <- c("Original Series", "5-Point Symmetric Moving Average", "52-Point Symmetric Moving Average")
legend("topleft", legend=leg.txt, lty=c(1,1,1), col=c("black","green","blue"),
       bty='n', cex=1, merge = TRUE, bg=336)

