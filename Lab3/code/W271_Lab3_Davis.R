## MIDS W271-4 Lab3           ##
## Carin, Davis, Levato, Song ##


## @knitr Libraries-Functions-Constants
# LIBRARIES, FUNCTIONS AND CONSTANTS -------------------------------------------------
# Load libraries
# library(e1071)
library(gtools)
library(ggplot2)
library(cowplot)
library(ggfortify)
library(scatterplot3d)
library(scales)
library(knitr)
library(pastecs)
library(car)
library(sandwich)
# library(lmtest)
library(plyr)
library(dplyr)
library(tidyr)
library(stargazer)
library(pander)
# library(texreg)
# library(weatherData)
library(scales)
library(xts)
library(reshape2)
library(lubridate)
library(forecast)
library(zoo)

library(fGarch)
library(quantmod)
library(tseries)

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
par(cex.main = 1, cex.lab = 0.9, cex.axis = 0.9)
set.seed(1234)



## @knitr ex1-load
# Loading the Data --------------------------------------------------------
# setwd('./HW8/data')

ex1df <- read.csv("houseValueData.csv")


## @knitr ex1-summary_table
# Exploratory analysis
stargazer(ex1df, header = F, title ="Summary Statistics of Wage Data",
          summary.stat = c("n", "mean", "sd", "min", "p25", "median", "p75", 
                           "max"))

## @knitr ex1-CrimeRate
# Not sure if this is percentile data, but that's what I'm assuming
crime_plot <- ggplot(data=ex1df, aes(crimeRate_pc)) + 
  geom_histogram(aes(y = (..count..)/sum(..count..)),
                 boundary = 0, binwidth = 2.5,
                 colour='black', fill='white') +
  labs(title="Histogram of Crime Rate",
       x="Crime Rate (Percentile)",
       y = "Relative Frequency") + theme_gray()

logCrime_plot <- ggplot(data=ex1df, aes(log(crimeRate_pc))) + 
  geom_histogram(aes(y = (..count..)/sum(..count..)),
                 binwidth = .25,
                 colour='black', fill='white') +
  labs(title="Histogram of Log(Crime Rate)",
       x="Log(Crime Rate (Percentile))",
       y = "Relative Frequency") +  theme_gray()

plot_grid(crime_plot, logCrime_plot)

## @knitr ex1-Business
# Kind of confused what this variable is. My guess is thhe percentage of businesses that are non-retail. 
# Maybe this is a good candidate variabel for instrumental variable??
business_plot <- ggplot(data=ex1df, aes(nonRetailBusiness)) + 
  geom_histogram(aes(y=(..count..)/sum(..count..)),
                 boundary=0, binwidth = .01,
                 colour='black', fill='white')+
  labs(title="Histogram of Non-Retail Business Percentage",
       x="Non-Retail Business Percentage",
       y="Relative Frequency") + theme_gray()
logBusiness_plot <- ggplot(data=ex1df, aes(log(nonRetailBusiness))) + 
  geom_histogram(aes(y=(..count..)/sum(..count..)),
                 binwidth = .15,
                 colour='black', fill='white')+
  labs(title="Histogram of log(Non-Retail Business Percentage)",
       x="log(Non-Retail Business Percentage)",
       y="Relative Frequency") + theme_gray()
plot_grid(business_plot, logBusiness_plot)

# Perhaps we can group these into  larger grids for the final paper


