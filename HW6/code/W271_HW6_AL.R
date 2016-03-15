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


## @knitr Question4
setwd("C:\\Users\\Batman\\Documents\\Term2\\271\\github_group\\W271\\HW6\\data")

## @knitr Question4-1
data<-read.csv("INJCJC.csv")
str(data)
dim(data)
head(data)
tail(data)

## @knitr Question4-2
data_ts<-ts(data$INJCJC, start=c(1990, 1, 1), end=c(2014, 11, 28), frequency=52)
length(data_ts)
head(data_ts)
tail(data_ts)


## @knitr Question4-3
data_ts.time<-time(data_ts)


## @knitr Question4-4
head(cbind(data_ts.time, data_ts), 10)
head(cbind(data_ts.time, data_ts), 13)
head(cbind(data_ts.time, data_ts), 6)

## @knitr Question4-5-1
autoplot(data_ts , xlab = "Time Period", ylab = "Number of Filed Claims",
         main="Initial Unemployment Claims", ts.colour = 'magenta4' )

## @knitr Question4-5-2
qplot(data_ts , geom="histogram", main='Histogram of Filed Jobless Claims',
      ylab='Frequency', xlab='Time', colour = I('magenta4'))

## @knitr Question4-5-3
autoplot(acf(data_ts, plot = FALSE), xlab = "Lag", main="Autocorrelation",
         ts.colour = 'magenta4')

## @knitr Question4-5-4
autoplot(pacf(data_ts, plot = FALSE), xlab = "Lag",main="Partial Autocorrelation",
         colour = "magenta4", conf.int.colour = "darkorange")


## @knitr Question4-5-5
gglagplot(data_ts, lags = 9, nrow = 3, ncol = 3)


## @knitr Question4-6-1
data_ts_rep5 = filter(data_ts, sides=2, rep(1,5)/5)
data_ts_rep52 = filter(data_ts, sides=2, rep(1,52)/52)
plot(data_ts, main="Moving Average Smoothing", 
     pch=4, lty=5, lwd=1, xlab="Year", 
     ylab="Number of Claims")
lines(data_ts_rep5, lty=1, lwd=1.5, col="chartreuse4")
lines(data_ts_rep52, lty=1, lwd=1.5, col="mediumblue")
# Add Legend
leg.txt <- c("Original Series", "5-Point Sym MA", "52-Point Sym MA")
legend("topleft", legend=leg.txt, lty=c(1,1,1), col=c("black","chartreuse4","mediumblue"),
       bty='n', cex=0.8, merge = TRUE, bg=336)

## @knitr Question4-6-2
wk = time(data_ts) - mean(time(data_ts))  
wk2 = wk^2 
wk3 = wk^3
cs = cos(2*pi*wk)  
sn = sin(2*pi*wk)
reg1 = lm(data_ts~wk + wk2 + wk3, na.action=NULL)
reg2 = lm(data_ts~wk + wk2 + wk3 + cs + sn, na.action=NULL)
plot(data_ts, main="Regression & Periodic Smoothing", 
     pch=4, lty=5, lwd=1, xlab="Year", 
     ylab="Number of Claims")
lines(fitted(reg1), lty=1, lwd=1.5, col="chartreuse4")
lines(fitted(reg2), lty=1, lwd=1.5, col="mediumblue")
# Add Legend
leg.txt <- c("Original Series", "Cubic Trend", "Periodic")
legend("topleft", legend=leg.txt, lty=c(1,1,1), col=c("black","chartreuse4","mediumblue"),
       bty='n', cex=0.8, merge = TRUE, bg=336)


## @knitr Question4-6-3
plot(data_ts, main="Kernel Smoothing", 
     pch=4, lty=5, lwd=1, xlab="Year", 
     ylab="Number of Claims")
lines(ksmooth(time(data_ts), data_ts, "normal", bandwidth=5/52),lty=1, lwd=1.5, col="chartreuse4")
lines(ksmooth(time(data_ts), data_ts, "normal", bandwidth=0.5),lty=1, lwd=1.5, col="mediumblue")
# Add Legend
leg.txt <- c("Original Series", "bandwidth=5/52", "bandwidth=0.5")
legend("topleft", legend=leg.txt, lty=c(1,1,1), col=c("black","chartreuse4","mediumblue"),
       bty='n', cex=0.8, merge = TRUE, bg=336)

## @knitr Question4-6-4
plot(data_ts, main="Nearest Neighborhood Smoothing", 
     pch=4, lty=5, lwd=1, xlab="Year", 
     ylab="Number of Claims")
lines(supsmu(time(data_ts), data_ts, span=.01),lty=1, lwd=1.5, col="chartreuse4")
lines(supsmu(time(data_ts), data_ts, span=.1),lty=1, lwd=1.5, col="mediumblue")
# Add Legend
leg.txt <- c("Original Series", "bandwidth=.01", "bandwidth=.1")
legend("topleft", legend=leg.txt, lty=c(1,1,1), col=c("black","chartreuse4","mediumblue"),
       bty='n', cex=0.8, merge = TRUE, bg=336)

## @knitr Question4-6-5
plot(data_ts, main="LOWESS Smoothing", 
     pch=4, lty=5, lwd=1, xlab="Year", 
     ylab="Number of Claims")
lines(lowess(data_ts, f=.001),lty=1, lwd=1.5, col="chartreuse4")
lines(lowess(data_ts, f=0.05),lty=1, lwd=1.5, col="mediumblue")
# Add Legend
leg.txt <- c("Original Series", "bandwidth=.001", "bandwidth=0.025")
legend("topleft", legend=leg.txt, lty=c(1,1,1), col=c("black","chartreuse4","mediumblue"),
       bty='n', cex=0.8, merge = TRUE, bg=336)

## @knitr Question4-6-6
plot(data_ts, main="Spline Smoothing", 
     pch=4, lty=5, lwd=1, xlab="Year", 
     ylab="Number of Claims")
lines(smooth.spline(time(data_ts), data_ts, spar=0.05),lty=1, lwd=1.5, col="chartreuse4")          
lines(smooth.spline(time(data_ts), data_ts, spar=0.75),lty=1, lwd=1.5, col="mediumblue")  
# Add Legend
leg.txt <- c("Original Series", "Smoothing Parameter=.05", "Smoothing Parameter=0.7")
legend("topleft", legend=leg.txt, lty=c(1,1,1), col=c("black","chartreuse4","mediumblue"),
       bty='n', cex=0.8, merge = TRUE, bg=336)

