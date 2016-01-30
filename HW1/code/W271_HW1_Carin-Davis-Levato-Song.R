## MIDS W271-4 HW1           ##
## Carin, Davis, Levato, Song ##


# LIBRARIES AND CONSTANTS -------------------------------------------------
## @knitr Libraries
#### LOAD LIBRARIES AND DEFINE CONSTANTS USED IN MULTIPLE CHUNKS
library(knitr)

# QUESTION 1 --------------------------------------------------------------
# load the data
## @knitr Question1
#### Load the data 
setwd("C:/Users/songminghu/UCB_DataScience/W271_ApplyRegressionTimeSeriesAnalysis/data")
load("birthweight_w271.Rdata")


# QUESTION 2 --------------------------------------------------------------
# check how many variables and observations in the data -------------------
# there are  14 variables and 1388   observations in the data -------------
## @knitr Question2
dim(desc)[1] # check number of variables, or use str(data) command
dim(data)[1] # check number of observations, or use str(data) command


# QUESTION 3 --------------------------------------------------------------

## @knitr Question3-part1
summary(data$bwght)


## @knitr Question3-part2
quantile( data$bwght, probs = c(0.01, 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99) )

## @knitr Question3-part3
#### PLOT HISTOGRAM AND PDF OF Z
par(mfrow = c(2, 2))
## Histogram with 10 bins
hist(data$bwght, breaks=10, main = 'Histogram with 10 bins', xlab ='Birth Weight')
## Histogram with 20 bins
hist(data$bwght, breaks=20, main = 'Histogram with 20 bins',  xlab ='Birth Weight')
## Histogram with 30 bins
hist(data$bwght, breaks=30, main = 'Histogram with 30 bins',  xlab ='Birth Weight')
## Histogram with 40 bins
hist(data$bwght, breaks=40, main = 'Histogram with 40 bins',  xlab ='Birth Weight')


# QUESTION 4 --------------------------------------------------------------

## @knitr Question4-part1
summary(data$cigs)


## @knitr Question4-part2
quantile( data$cigs, probs = c(0.01, 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99) )

## @knitr Question4-part3
#### PLOT HISTOGRAM AND PDF OF Z
par(mfrow = c(1, 2))
## Histogram with 10 bins
hist(data$cigs, breaks=10, main = 'Histogram with 10 bins', xlab ='The Number of Smoked Cigarettes')
## Histogram with 20 bins
hist(data$cigs, breaks=20, main = 'Histogram with 20 bins',  xlab ='The Number of Smoked Cigarettes')

