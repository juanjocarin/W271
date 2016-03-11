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
