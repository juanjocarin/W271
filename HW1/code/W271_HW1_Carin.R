## MIDS W271-4 HW1            ##
## Carin, Davis, Levato, Song ##


## @knitr Libraries
# PATHS, LIBRARIES AND CONSTANTS -------------------------------------------------
# Set relative path (referred to project)
# Load libraries
# Define constants used in multiple chunks
library(ggplot2)
# library(knitr)
# library(plot3D)
# num_sim <- 100e3 # number of simulations



## @knitr Question1
# QUESTION 1 --------------------------------------------------------------
# Load the birthweight dataset
load("data/birthweight_w271.rdata")



## @knitr Question2
# QUESTION 2 --------------------------------------------------------------
# Examine the basic structure of the data set using desc, str, and summary
desc
str(data)
summary(data)



## @knitr Question3-1
# QUESTION 3 --------------------------------------------------------------
# Summarize the variable bwght: summary(data$bwght)
summary(data$bwght)

## @knitr Question3-2
# List the following quantiles: 1%, 5%, 10%, 25%, 50%, 75%, 90%, 95%, 99%**
quantile(data$bwght, probs = c(1, 5, 10, 25, 50, 75, 90, 95, 99)/100)

## @knitr Question3-3-1
# Plot the histogram of bwght and comment on the shape of its distribution
# Use hist and bin width = 5
bin_width = 5
hist(data$bwght, breaks = seq(floor(min(data$bwght)/bin_width)*bin_width, 
                              ceiling(max(data$bwght)/bin_width)*bin_width, by = bin_width), 
     xlab = "Birth weight (ounces)", ylab = "Count", 
     main = "Histogram of birth weight")

## @knitr Question3-3-2
# Use ggplot and bin width = 5
ggplot(data = data, aes(bwght)) + 
  geom_histogram(colour = 'black', fill = 'white', 
                 binwidth = bin_width) +  
  labs(x = "Birth weight (ounces)", y = "Count", 
       title = "Histogram of birth weight")

# With density plot
# ggplot(data = data, aes(bwght)) + 
#   geom_histogram(aes(y = ..density..), colour = 'black', fill = 'white', 
#                  binwidth = bin_width) +  
#   labs(x = "Birth weight (ounces)", y = "Density", 
#        title = "Histogram of birth weight") + 
#   stat_function(fun = dnorm, args = list(mean = mean(data$bwght, na.rm = TRUE), 
#                                          sd = sd(data$bwght, na.rm = TRUE)), 
#                 colour = 'black', size = 1)

## @knitr Question3-3-3
# Use ggplot and bin width = 10
bin_width = 10
ggplot(data = data, aes(bwght)) + 
  geom_histogram(binwidth = bin_width) +  
  labs(x = "Birth weight (ounces)", y = "Count", 
       title = "Histogram of birth weight")

## @knitr Question3-3-4
# Use ggplot and bin width = 20
bin_width = 20
ggplot(data = data, aes(bwght)) + 
  geom_histogram(binwidth = bin_width) +  
  labs(x = "Birth weight (ounces)", y = "Count", 
       title = "Histogram of birth weight")