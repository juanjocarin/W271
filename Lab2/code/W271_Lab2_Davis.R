## MIDS W271-4 Lab2           ##
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
library(reshape2)

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



## @knitr Question1-1-1
# QUESTION 1 --------------------------------------------------------------
# Find the conditional expectation of Y given X, E(Y|X)

## @knitr Question1-1-2

## @knitr Question1-3

## @knitr Question1-4

## @knitr Question1-5

## @knitr Question2
# QUESTION 2 --------------------------------------------------------------
# Find, the values of a, b, and c that minimize the variance of total payoff



## @knitr Question3
# QUESTION 3 --------------------------------------------------------------


## @knitr Question4
# QUESTION 4 --------------------------------------------------------------

## @knitr Question4-1-1
# Load data and summarize
data <- read.csv("WageData2.csv", header = T)
data$experienceSquare = data$experience^2
#round(stat.desc(data, desc = TRUE, basic = TRUE), 2)

## @knitr Question4-1-1a
stargazer(data, header = F,
          summary.stat = c("n", "mean", "sd", "min", "p25", "median", "p75", "max"))

## @knitr Question4-1-2
data2 <- melt(data[, c(2,13,14)])
ggplot(data2, aes(x= value)) +
  facet_wrap(~variable, scales = "free", ncol = 3) +
  geom_histogram(bins = 25, color = 'black', fill = 'white') +
  labs(title = "Histogram of Wage And IQ Variables") 

## @knitr Question4-1-3
data4 <- melt(data[, c(6, 9:12)])
ggplot(data4, aes(factor(value))) + 
  geom_bar(colour='black', fill = 'white') + 
  facet_grid(~variable) +
  labs(title = "Histogram of Race, Location, and Instrumental Variables") 

## @knitr Question4-1-4
data3 <- melt(data[, c(3,4,5,7,8)])
ggplot(data3, aes(x= value)) +
  facet_wrap(~variable, scales = "free", ncol = 2) +
  geom_histogram(binwidth = 1, color = 'black', fill = 'white') +
  labs(title = "Histogram of Education, Age, and Experience Variables") 

## @knitr Question4-2-1
cor.matrix1 <- as.data.frame(cor(data$wage, data[,c(3:15)], use="pairwise.complete.obs"),
                             row.names = "Wage")
stargazer(cor.matrix1, title="Correlations for Wage", flip=T, summary = F, header = F)

cor.matrix2 <- as.data.frame(cor(data$logWage, data[,c(2:13,15)], use="pairwise.complete.obs"),
                             row.names = "log(Wage)")
stargazer(cor.matrix2, title="Correlations for log(Wage)", flip=T, summary=F, header = F)


## @knitr Question4-2-2
data5 <- melt(data[, c(2:13, 15)], id.vars="wage")
ggplot(data5, aes(wage, value)) +
  geom_point() + geom_smooth(method = "lm") +
  facet_wrap(~variable,  scales = "free", ncol = 2) + 
  labs(title = "Scatterplot of Wage Against Variables of Interest") 

## @knitr Question4-2-3
data6 <- melt(data[, c(3:15)], id.vars="logWage")
ggplot(data6, aes(logWage, value)) +
  geom_point() + geom_smooth(method = "lm") +
  facet_wrap(~variable,  scales = "free", ncol = 2) + 
  labs(title = "Scatterplot of log(Wage) Against Variables of Interest") 

## @knitr Question4-3-1
model <- lm(logWage ~ education + experience+ age + raceColor, data = data)
stargazer(model, type="latex", title="Regression Summary",
          header = FALSE, table.placement = "h!")

## @knitr Question4-3-2
model2 <-  lm(logWage ~ education + experience + raceColor, data = data)
stargazer2(model2, title = "Regression summary", digits = 3, digits.extra = 6, 
           dep.var.labels = 'log(Wages)', 
           covariate.labels = c("Education", "Experience", "Race (White or Non-white)"))

## @knitr Question4-3-3
autoplot(model2)

## @knitr Question4-4-1
model3 <- lm(logWage ~ education + experience + experienceSquare + raceColor, data = data)
stargazer2(model3, title = "Regression summary", digits = 3, digits.extra = 6, 
           dep.var.labels = 'log(Wages)', 
           covariate.labels = c("Education", "Experience", "$\\text{Expereince}^{2}$",
                                "Race (White or Non-white)"))

## @knitr Question4-4-2
# model4 <- lm(wage ~ education + experience + experienceSquare + raceColor, data = data)
x <- data$experience
y <- coeftest(model3, vcovHC)[1] + x*coeftest(model3, vcovHC)[3] + x**2*coeftest(model3, vcovHC)[4]
plt_df <- data.frame(x,y)
ggplot(plt_df, aes(x,y)) + geom_smooth(na.rm=T) +
  labs(x = "Experience", y = "log(Wage) ($)", title = "Effect of Experience on log(Wages)")

## @knitr Question4-5-1
model5 <- lm(logWage ~ education + experience + experienceSquare + raceColor + 
               dad_education + mom_education + rural + city, data = data)
# coeftest(model5, vcovHC)
stargazer2(model5, title = "Regression summary", digits = 3, digits.extra = 6,
           dep.var.labels = 'log(Wages)', 
           covariate.labels = c("Education", "Experience", "$\\text{Expereince}^{2}$",
                                "Race (White or Non-white)", "Father's Education",
                                "Mother's Education", "Rural (Yes or No)", "City (Yes or No)"))

## @knitr Question4-5-2 
dad_edu_na <- data[is.na(data$dad_education),]
mom_edu_na <- data[is.na(data$mom_education),]
actual_obs <- data[!is.na(data$mom_education) &
                       !is.na(data$dad_education),]

stargazer(data, header = F,
          summary.stat = c("n", "mean", "sd", "min", "p25", "median", "p75", "max"),
          title="All Observations")
stargazer(dad_edu_na, header = F,
          summary.stat = c("n", "mean", "sd", "min", "p25", "median", "p75", "max"),
          title="Missing Father's Education")
stargazer(mom_edu_na, header = F,
          summary.stat = c("n", "mean", "sd", "min", "p25", "median", "p75", "max"),
          title="Missing Mother's Education")
stargazer(actual_obs, header = F,
          summary.stat = c("n", "mean", "sd", "min", "p25", "median", "p75", "max"),
          title="Actual Observations")

## @knitr Question4-5-3
data7 <- data
data7$dad_education <- na.fill(data7$dad_education, mean(data7$dad_education, na.rm=T))
data7$mom_education <- na.fill(data7$mom_education, mean(data7$mom_education, na.rm=T))
model6 <- lm(logWage ~ education + experience + experienceSquare + raceColor + 
               dad_education + mom_education + rural + city, data = data7)

stargazer2(model6, title = "Regression summary", digits = 3, digits.extra = 6,
dep.var.labels = 'log(Wages)', 
covariate.labels = c("Education", "Experience", "$\\text{Expereince}^{2}$",
                     "Race (White or Non-white)", "Father's Education",
                     "Mother's Education", "Rural (Yes or No)", "City (Yes or No)"))

## @knitr Question4-5-4
data8 <-data
model_dad<-lm(dad_education~education+experience+raceColor, data=data8)
model_mom<-lm(mom_education~education+experience+raceColor, data=data8)
coef_dad<-coef(model_dad)
coef_mom<-coef(model_mom)
# Impute values for missing observations
for (i in 1:nrow(data8)) {
  if (is.na(data8$dad_education[i])==TRUE) {
    data8$dad_education[i]= coef_dad[1]+coef_dad[2]*data8$education[i]+
      coef_dad[3]*data8$experience[i]+coef_dad[4]*data8$raceColor[i]
  }
  if (is.na(data8$mom_education[i])==TRUE) {
    data8$mom_education[i]= coef_mom[1]+coef_mom[2]*data8$education[i]+
      coef_mom[3]*data8$experience[i]+coef_mom[4]*data8$raceColor[i]
  }
}

model7 <- lm(logWage ~ education + experience + experienceSquare + raceColor + 
               dad_education + mom_education + rural + city, data = data8)

stargazer2(model7, title = "Regression summary", digits = 3, digits.extra = 6,
           dep.var.labels = 'log(Wages)', 
           covariate.labels = c("Education", "Experience", "$\\text{Expereince}^{2}$",
                                "Race (White or Non-white)", "Father's Education",
                                "Mother's Education", "Rural (Yes or No)", "City (Yes or No)"))


## @knitr Question4-5-5
stargazer2(list(model5, model6, model7), dep.var.labels = 'log(Wages)', 
           covariate.labels = c("Education", "Experience", "$\\text{Expereince}^{2}$",
                                "Race (White or Non-white)", "Father's Education",
                                "Mother's Education", "Rural (Yes or No)", "City (Yes or No)"))

## @knitr Question4-6-1
first_stage_a <- lm(education ~ z1, data = data )
first_stage_b <- lm(education ~ z2, data = data)
second_stage_a <- lm(data$logWage ~ first_stage_a$fitted + data$experience +
                     data$experienceSquare + data$raceColor + data$dad_education + 
                     data$mom_education + data$rural + data$city)
second_stage_b <- lm(data$logWage ~ first_stage_b$fitted + data$experience +
                       data$experienceSquare + data$raceColor + data$dad_education + 
                       data$mom_education + data$rural + data$city)
stargazer2(list(first_stage_a, first_stage_b), dep.var.labels = 'education', 
           covariate.labels = c('IV 1', 'IV 2'))

## @knitr Question4-6-2
stargazer2(second_stage_a, dep.var.labels = 'log(Wages)', 
           covariate.labels = c("Education ($z_1$)", "Experience", "$\\text{Expereince}^{2}$",
                                "Race (White or Non-white)", "Father's Education",
                                "Mother's Education", "Rural (Yes or No)", "City (Yes or No)"),
           title = "Two One Regression summary")

## @knitr Question4-6-3
stargazer2(second_stage_b, dep.var.labels = 'log(Wages)', 
           covariate.labels = c("Education ($z_2$)", "Experience", "$\\text{Expereince}^{2}$",
                                "Race (White or Non-white)", "Father's Education",
                                "Mother's Education", "Rural (Yes or No)", "City (Yes or No)"),
           title = "Step Two Regression summary")



## @knitr Question5-1-1
# QUESTION 5 --------------------------------------------------------------
# Load data
d <- read.csv('wealthy_candidates.csv', header=T)
# Remove missing observation
d <- d[complete.cases(d$absolute_wealth),]
d$logWealth <- log(d$absolute_wealth)
d$hasWealth <- ifelse(d$absolute_wealth == 2,0,1)
d$hasWealth <- factor(d$hasWealth, labels=c("No", "Yes"))
stargazer(d, header = F, flip=T,
          summary.stat = c("n", "mean", "sd", "min", "p25", "median", "p75", "max"))

# noWealth <- d[d$hasWealth=="No",]
# Wealth <- d[d$hasWealth=="Yes",]
# mean(noWealth$voteshare)
# mean(Wealth$voteshare)

## @knitr Question5-1-2
d2 <- melt(d[,c(3:7)])
ggplot(d2, aes(x=value)) + facet_wrap(~variable, scales = "free", ncol = 3) +
  geom_histogram(bins = 20, color = 'black', fill = 'white') +
  labs(title = "Histogram of Weath, Vote share, and Covariates")

## @knitr Question5-1-3
ggplot(d, aes(hasWealth)) +
  geom_bar(color = 'black', fill = 'white') +
  labs(x= 'Weath Greater than Zero', title = "Wealth (Yes/No)")

## @knitr Question5-1-4
ggplot(d, aes(region)) +
  geom_bar(color = 'black', fill = 'white') +
  labs( title = "Region")

## @knitr Question5-1-5
ggplot(d[d$hasWealth=="Yes",], aes(logWealth)) +
  geom_histogram(bins = 20, color = 'black', fill = 'white') +
  labs(title = "Histogram of Weath Among Those With Wealth")

## @knitr Question5-1-6
d3 <- melt(d[, c(3:7)], id.vars="voteshare")
ggplot(d3, aes(voteshare, value)) +
  geom_point() + geom_smooth(method = "lm") +
  facet_wrap(~variable,  scales = "free", ncol = 2) + 
  labs(title = "Scatterplot of Voteshare Against Variables of Interest")

## @knitr Question5-1-7
m1 <- lm(voteshare~  logWealth + hasWealth, data = d)
stargazer2(m1, dep.var.labels = 'Voteshare', 
           covariate.labels = c("log(Wealth)", "Has Wealth = Yes"),
           title = "Regression summary")

# ggplot(d, aes(logWealth)) + geom_histogram(aes(fill=region)) +
#   facet_wrap(~region,ncol=1) + scale_fill_brewer(palette="Set3") +
#   labs( title = "Wealth by Region")
# 
# ggplot(d, aes(voteshare)) + geom_histogram(aes(fill=region)) +
#   facet_wrap(~region,ncol=1) + scale_fill_brewer(palette="Set3") +
#   labs( title = "Voteshare by Region")
# 
# ggplot(d, aes(urb)) + geom_histogram(aes(fill=region)) +
#   facet_wrap(~region,ncol=1) + scale_fill_brewer(palette="Set3") +
#   labs( title = "Urb by Region")
# 
# ggplot(d, aes(lit)) + geom_histogram(aes(fill=region)) +
#   facet_wrap(~region,ncol=1) + scale_fill_brewer(palette="Set3") +
#   labs( title = "Lit by Region")
# 
# ggplot(d, aes(x=voteshare, y=logWealth)) + geom_point(aes(color=region)) +
#   geom_smooth(method = "lm") +
#   facet_wrap(~region, ncol=1) + 
#   labs(title = "Scatterplot of Voteshare Against Wealth by Region")
# 
# ggplot(d, aes(x=voteshare, y=as.numeric(hasWealth))) + geom_point(aes(color=region)) +
#   geom_smooth(method = "lm") +
#   facet_wrap(~region, ncol=1) + 
#   labs(title = "Scatterplot of Voteshare Against Wealth by Region")
# 
# 
# m <- lm(voteshare~  hasWealth, data = d)
# m1 <- lm(voteshare~  logWealth + hasWealth, data = d)
# m2 <- lm(voteshare~logWealth, data=d)
# m3 <- lm(voteshare~ logWealth + hasWealth + region, data = d)
# m4 <- lm(voteshare~ logWealth + region, data = d)
# 
# 
# d$wealthSqr <- d$logWealth**2
# m5 <- lm(voteshare~ logWealth + wealthSqr + hasWealth , data = d)
# autoplot(m)
# autoplot(m2)
# autoplot(m3)
# summary.lm(m)
# summary.lm(m1)
# summary.lm(m2)
# summary.lm(m3)
# summary.lm(m4)
# summary.lm(m5)
## @knitr Question
# QUESTION 6 --------------------------------------------------------------
load("retailSales.Rdata")
