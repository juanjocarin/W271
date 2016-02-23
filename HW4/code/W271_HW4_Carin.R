## MIDS W271-4 HW4            ##
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
library(tidyr)
library(stargazer)
library(texreg)
library(pander)

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
# Examine and summarize the dataset.
# How many observations and variables are there?
# Examine the variables of key interest: apps, bowl, btitle, and finfour.
# Path relative to W271.Rproj, never to be run by the .Rmd (conflict with knitr)
# setwd('HW4/data')
load("athletics.RData")
desc
str(data)

## @knitr Question1-1bis
head(data)
# summary(data)
# Omit character vectors (only column 'school') in Descriptive Statistics
round(stat.desc(data[, lapply(data, class) != "character"], desc = TRUE, 
                basic = TRUE), 2)

## @knitr Question1-2
# count_NAs <- function(x) unlist(lapply(lapply(x, is.na), sum))
# count_NAs(data)[count_NAs(data) > 0]
colSums(is.na(data[colSums(is.na(data)) > 0]))

## @knitr Question1-3
# Keep only variables of interest
# Also convert binary variables to logical
# (not necessary but better for plotting)
# No need to keep 'lapps', it's just log('apps')
categories <- c('bowl', 'btitle', 'finfour')
vars_of_interest <- c('year', 'school', 'apps', categories)
data2 <- data %>% 
  select(match(vars_of_interest, names(data))) %>% 
  mutate_each_(funs(as.logical), categories)
subset(desc, variable %in% vars_of_interest)
head(data2)

## @knitr Question1-4
kable(data.frame(ftable(data2[, names(data2) %in% categories])), 
      caption = "Number of observations per group")
tableCount <- incCount(tableCount, "table-Q1")

## @knitr Question1-5
# ggplot(data2, aes(apps)) +
#   geom_histogram(aes(y = (..count..)/sum(..count..)), colour = 'black',
#                  fill = 'white', binwidth = 1000) +
#   scale_y_continuous(labels = percent_format()) + 
#   facet_wrap(nrow = 2, ~ year) + 
#   labs(x = "Number of applications for admission",
#        y = "Relative frequency",
#        title = "Histogram of applications for admission")
data_aux <- data2 %>% 
  select(school, year, apps) %>%
  group_by(school) %>%
  mutate(apps = mean(apps)) %>% 
  select(school, apps) %>% 
  filter(row_number() == 1) %>% 
  mutate(year = 'Average 1992-1993')
data_aux <- rbind(data_aux, data2[, c('school', 'apps', 'year')])
ggplot(data_aux, aes(apps)) + 
  geom_histogram(aes(y = (..count..)/sum(..count..)), colour = 'black',
                 fill = 'white', binwidth = 1000) +
  scale_y_continuous(labels = percent_format()) + 
  facet_wrap(nrow = 3, ~ year) + 
  labs(x = "Number of applications for admission",
       y = "Relative frequency",
       title = "Histogram of applications for admission per year")
figCount <- incCount(figCount, "hist-Q1")

## @knitr Question1-6
# data_bowl <- data2 %>%
#   select(school, year, bowl) %>%
#   group_by(school) %>%
#   summarise(bowl = as.logical(sum(bowl)))
# ggplot(data_bowl, aes(bowl)) +
#   geom_bar(colour='black', fill = 'white')
ggplot(data2, aes(bowl)) +
  geom_bar(colour='black', fill = 'white') + 
  facet_grid( ~ year) + 
  scale_y_continuous(limits = c(0, 55), breaks = seq(0, 55, 10)) + 
  labs(x = "Bowl game in the previous year", y = "Number of schools", 
       title = "Number of schools depending\non bowl game in the previous year")
figCount <- incCount(figCount, "barchart-Q1-bowl-1")

## @knitr Question1-7
ggplot(data2, aes(x = bowl, y = apps)) + 
  stat_summary(fun.y = mean, geom = 'bar', position = 'dodge', 
               colour = 'black', fill = 'white') +
  stat_summary(fun.data = mean_cl_normal, geom = 'errorbar', width = .1) + 
  facet_grid( ~ year) + 
  scale_y_continuous(limits = c(0, 24000), breaks = seq(0, 24000, 5000)) + 
  labs(x = "Bowl game in the previous year", 
       y = "Number of applications\nfor admission", 
       title = paste("Bar chart of the mean number of\napplications depending", 
                     "on bowl game\nin the previous year"))
figCount <- incCount(figCount, "barchart-Q1-bowl-2")

## @knitr Question1-8
ggplot(data2, aes(btitle)) +
  geom_bar(colour='black', fill = 'white') + 
  facet_grid( ~ year) + 
  scale_y_continuous(limits = c(0, 55), breaks = seq(0, 55, 10)) + 
  labs(x = "Men's conference championship in the previous year", 
       y = "Number of schools", 
       title = paste0("Number of schools depending on\nmen's conference ", 
                     "championship\nin the previous year"))
figCount <- incCount(figCount, "barchart-Q1-btitle-1")

## @knitr Question1-9
ggplot(data2, aes(x = btitle, y = apps)) +
  stat_summary(fun.y = mean, geom = 'bar', position = 'dodge', 
               colour = 'black', fill = 'white') +
  stat_summary(fun.data = mean_cl_normal, geom = 'errorbar', width = .1) + 
  facet_grid( ~ year) + 
  scale_y_continuous(limits = c(0, 24000), breaks = seq(0, 24000, 5000)) + 
  labs(x = "Men's conference championship\nin the previous year", 
       y = "Number of applications\nfor admission", 
       title = paste0("Bar chart of the mean number of\napplications ", 
                     "depending on men's\nconference championship\nin the ", 
                     "previous year"))
figCount <- incCount(figCount, "barchart-Q1-btitle-2")

## @knitr Question1-10
ggplot(data2, aes(finfour)) +
  geom_bar(colour='black', fill = 'white') + 
  facet_grid( ~ year) + 
  scale_y_continuous(limits = c(0, 55), breaks = seq(0, 55, 10)) + 
  labs(x = "Men's final four in the previous year", 
       y = "Number of schools", 
       title = paste0("Number of schools depending on men's\n", 
                      "final four in the previous year"))
figCount <- incCount(figCount, "barchart-Q1-finfour-1")

## @knitr Question1-11
ggplot(data2, aes(x = finfour, y = apps)) + 
  stat_summary(fun.y = mean, geom = 'bar', position = 'dodge', 
               colour = 'black', fill = 'white') +
  stat_summary(fun.data = mean_cl_normal, geom = 'errorbar', width = .1) + 
  facet_grid( ~ year) + 
  scale_y_continuous(limits = c(0, 24000), breaks = seq(0, 24000, 5000)) + 
  labs(x = "Men's final four in the previous year", 
       y = "Number of applications\nfor admission", 
       title = paste0("Bar chart of the mean number of\napplications ", 
                      "depending on\nmen's final four in the previous year"))
figCount <- incCount(figCount, "barchart-Q1-finfour-2")



## @knitr Question2-1
# QUESTION 2 --------------------------------------------------------------
# To prepare for a difference-in-difference analysis, transfer the dataset to 
# wide-format. Each school should have a single row of data, with separate 
# variables for 1992 and 1993.
# Create a new variable, clapps to represent the change in the log of the 
# number of applications from 1992 to 1993.
# Examine this variable and its distribution.
# Which schools had the greatest increase and the greatest decrease in number 
# of log applications?
data3 <- reshape(data2, idvar = "school", timevar = "year", 
                 v.names = c("apps","bowl", "btitle", "finfour"), 
                 varying = list(c("apps.1992", "apps.1993"), 
                                c("bowl.1992", "bowl.1993"), 
                                c("btitle.1992", "btitle.1993"), 
                                c("finfour.1992", "finfour.1993")), 
                 direction = "wide")
head(data3, 4)
# Same using tidyr: melt/gather columns + unite variable w/ year + spread/dcast
data3 <- data2 %>% 
  gather(variable, value, -(year:school)) %>% 
  unite(temp, variable, year, sep = ".") %>%
  spread(temp, value)
# Convert to logical values again
vars_to_convert <- unlist(lapply(categories, function(x) grep(x, names(data3))))
data3 <- data3 %>% 
  mutate_each_(funs(as.logical), names(data3)[vars_to_convert])
head(data3, 4)

## @knitr Question2-2
# data3$clapps <- log(data3$apps.1993) - log(data3$apps.1992)
# data3$clapps <- log(data3$apps.1993 / data3$apps.1992)
data3 <- data3 %>% 
  mutate(clapps = log(apps.1993) - log(apps.1992))
# Results may differ from those we'd obtain using lapps due to decimals!!!
head(data3, 4)

## @knitr Question2-3
# (schools_greatest_increase <- head(data3[order(data3$clapps, 
#                                                decreasing = TRUE), ], 5))
schools_greatest_increase <- data3 %>% 
  arrange(desc(clapps)) %>% 
  select(school, apps.1992, apps.1993,clapps) %>% 
  head(5)
kable(schools_greatest_increase, row.names = FALSE, 
      caption = paste("Schools with the greatest increase in number of log", 
                      "applications"))
tableCount <- incCount(tableCount, "table-Q2-1")

## @knitr Question2-4
# (schools_greatest_decrease <- tail(data3[order(data3$clapps, 
#                                                decreasing = TRUE), ], 5))
schools_greatest_decrease <- data3 %>% 
  arrange(clapps) %>% 
  select(school, apps.1992, apps.1993,clapps) %>% 
  head(5)
kable(schools_greatest_decrease, row.names = FALSE, 
      caption = paste("Schools with the greatest decrease in number of log", 
                      "applications"))
tableCount <- incCount(tableCount, "table-Q2-2")
# Schools more stable
# data3 %>% arrange(abs(clapps)) %>% head(5)



## @knitr Question3-1
# QUESTION 3 --------------------------------------------------------------
# Create 3 variables, cperf, cbball, and cbowl to represent the changes in the 
# 3 athletic success variables.
# Which of these variables has the highest variance?
# data3$cbowl <- data3$bowl.1993 - data3$bowl.1992
data3 <- data3 %>% 
  mutate(cbowl = bowl.1993 - bowl.1992, cperf = btitle.1993 - btitle.1992, 
         cbball = finfour.1993 - finfour.1992)
head(data3, 4)

## @knitr Question3-2
data3 %>% select(matches('bowl.1|finfour|btitle')) %>% summarise_each(funs(sum))

## @knitr Question3-3
# data3 %>% select(cbowl, cperf, cbball) %>% var %>% diag
(v <- data3 %>% select(cbowl, cperf, cbball) %>% summarise_each(funs(var)))



## @knitr Question4
# QUESTION 4 --------------------------------------------------------------
# ...
data3 <- data3 %>% 
  rename(cbtitle = cperf, cfinfour = cbball)



## @knitr Question5-1
# QUESTION 5 --------------------------------------------------------------
# Estimate the first-difference model given above.
# interpret the slope coefficients and comment on their statistical 
# significance and practical significance.
model1 <- lm(clapps ~ cbowl + cbtitle + cfinfour, data3)
# coeftest(model1, vcov = vcovHC)
stargazer2(model1, title = "Regression summary", digits = 4, digits.extra = 6, 
           dep.var.labels = 'Change in log(applications)', 
           covariate.labels = c("Change in bowl game in previous year",
                                paste("Change in men's conference", 
                                      "championship in previous year"),
                                "Change in men's final four in previous year", 
                                "Intercept (Constant): year 1993"))
tableCount <- incCount(tableCount, "table-Q5")


## @knitr Question5-2
model0 <- lm(log(apps) ~ as.factor(year) + bowl + btitle + finfour + 
               as.factor(school), data)
coeftest(model0, vcov = vcovHC)[1:5, ]

## @knitr Question5-3
autoplot(model1)



## @knitr Question6
# QUESTION 6 --------------------------------------------------------------
# Test the joint signifance of the three indicator variables. 
# What impact does the result have on your conclusions?
  
