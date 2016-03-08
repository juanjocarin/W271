## MIDS W271-4 Lab2           ##
## Carin, Davis, Levato, Song ##


## @knitr Libraries-Functions-Constants
# LIBRARIES, FUNCTIONS AND CONSTANTS -------------------------------------------------
# Load libraries
library(plot3D)
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
library(reshape2)
library(GGally)

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
simulations <- 1e4 # number of simulations
set.seed(123)
x <- runif(simulations, min=0, max=1) # X ~ U(0,1)
y <- runif(simulations, min=0, max=x) # Y|X ~ U(0,X)
par(mfrow = c(1, 2))
hist(x, main = "pdf(x)", freq = FALSE)
lines(density(x), col = 'red')
hist(y, main = "pdf(y)", freq = FALSE)
lines(density(y), col = 'red')

## @knitr Question1-1-2
# y1 <- runif(simulations, min = 0, max = 0.2) # Fix X to 0.2
y1 <- y[x > 0.2 - 1e-2 & x < 0.2 + 1e-2] # Using previous simulation
hist(y1, main = 'pdf(y|x=0.2)', freq = FALSE)
lines(density(y1), xlim = c(0, 1), main = 'pdf(y|x=0.2)', col = 'red')
abline(v = mean(y1), col = 'green', lty = 2, lwd = 4)
# legend("topright", "E(Y|X=0.2)", lty = 1, bty="n", col = 'red')

## @knitr Question1-3
pdf_x <- function(x) ifelse(x<1 & x>0, 1, 0) # f(x)
integrate(pdf_x, -Inf, Inf) # integral
pdf_y_given_x <- function(x,y) ifelse(y<x & y>0 & x<1 & x>0, 1/x, 0) # f(y|x)
pdf_xy <- function(x,y) pdf_x(x)*pdf_y_given_x(x,y) # f(x,y)
# integral
integrate(function(y) sapply(y, function(y) integrate(function(x) 
  pdf_xy(x,y), 0, 1)$value), -Inf, Inf)
# Plot f(x,y)
x0 <- y0 <- seq(0, 1, by = 0.01)
grid <- mesh(x0, y0)
z0 <- with(grid, pdf_x(x)*pdf_y_given_x(x,y))
# contour(x0, y0, z0, asp=1)
# par(mfrow = c(1, 2))
persp3D(z = z0, x = x0, y = y0)
# Confirm that f(x,y) = 1/x
# z2 <- with(grid, ifelse(x<=y | x==0 | y == 0, 0, 1/x))
# persp3D(z = z2, x = x0, y = y0)

## @knitr Question1-4
# f(y)
pdf_y <- function(y) 
  sapply(y, function(y) integrate(function(x) 
    pdf_y_given_x(x,y)*pdf_x(x), 0, 1)$value)
integrate(pdf_y, -Inf, Inf) # integral
plot(sort(y), pdf_y(sort(y)), type = 'l', main = 'pdf(y)', xlab = 'y')
# Confirm that f(y) = log(1/y)
lines(sort(y), log(1/sort(y)), type = 'l', main = 'pdf(y)')


## @knitr Question1-5
# Confirm E(X|Y=0.5) (use values of Y around 0.5 in the previous simulation)
mean(x[y > 0.5 - 1e-2 & y < 0.5 + 1e-2])
1/(2*log(2))



## @knitr Question2
# QUESTION 2 --------------------------------------------------------------
# Find, the values of a, b, and c that minimize the variance of total payoff
payoff <- function(x) {
  a <- x[1]
  b <- x[2]
  c <- x[3]
  a^2 + b^2/2 + c^2/3
}
gradient_payoff <- function(x) {
  a <- x[1]
  b <- x[2]
  c <- x[3]
  c(2*a, b, 2*c/3)
}
sol <- constrOptim(theta = c(.3, .3, .4), f = payoff, grad = gradient_payoff, 
                   ui = rbind(c(1, 0, 0), c(0, 1, 0), c(0, 0, 1), 
                              c(-1, 0, 0), c(0, -1, 0), c(0, 0, -1), 
                              c(1, 1, 1), c(-1, -1, -1)), 
                   ci = c(0, 0, 0, -1, -1, -1, 1-1e-6, -1-1e-6))
sol$par



## @knitr Question3
# QUESTION 3 --------------------------------------------------------------
simulations <- 1e3 # number of simulations
theta <- 100 # an arbitrary value of theta
y <- runif(n = simulations, min = 0, max = theta) # Y ~ U(0,theta)
# any(y == theta); all(y < theta) # FALSE and TRUE, respectively
# No matter how large is the sample, Yi is always lower than 1
set.seed(1)
num_simulations <- sort(c(1, sample(c(2:simulations), 49)))
theta_mle <- unlist(lapply(num_simulations, function(n) 
  mean(max(runif(n = n, min = 0, max = theta)))))
plot(num_simulations, theta_mle, ylim = c(floor(min(theta_mle)), theta),
     xlab = "Number of simulations", ylab = "MLE of theta", pch = '*')
lines(num_simulations, theta_mle, lwd = 0.5, col = 'blue')
lines(num_simulations, theta*num_simulations/(num_simulations+1), col = 'green')
abline(h = theta, col = 'red', lty = 2)



## @knitr Question4
# QUESTION 4 --------------------------------------------------------------

## @knitr Question4-1-1
# Load data and summarize
data <- read.csv("WageData2.csv", header = T)
data$experienceSquare = data$experience^2
#round(stat.desc(data, desc = TRUE, basic = TRUE), 2)

## @knitr Question4-1-1a
stargazer(data, header = F, title ="Summary Statistics of Wage Data",
          summary.stat = c("n", "mean", "sd", "min", "p25", "median", "p75", 
                           "max"))

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
# Calculate correlations for wage and log(wage)
cor.matrix1 <- as.data.frame(cor(data$wage, data[,c(2:15)], 
                                 use="pairwise.complete.obs"), 
                             row.names = "Wage")
cor.matrix2 <- as.data.frame(cor(data$logWage, data[,c(2:15)], 
                                 use="pairwise.complete.obs"), 
                             row.names = "log(Wage)")
cor.matrix <- bind_rows(cor.matrix1, cor.matrix2)
rownames(cor.matrix) <- c("Wage", "log(Wage)")
stargazer(cor.matrix, title="Correlations for Wage and log(Wage)", 
          flip=T, summary = F, header = F)


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
stargazer(model, type="latex", title="Regression Summary", report = "vc*st",
          header = FALSE, table.placement = "h!")

## @knitr Question4-3-2
model2 <-  lm(logWage ~ education + experience + raceColor, data = data)
stargazer2(model2, title = "Regression summary", digits = 3, digits.extra = 6, 
           dep.var.labels = 'log(Wages)', 
           covariate.labels = c("Education", "Experience", 
                                "Race (White or Non-white)"))

## @knitr Question4-3-3
autoplot(model2)

## @knitr Question4-4-1
model3 <- lm(logWage ~ education + experience + experienceSquare + raceColor, 
             data = data)
stargazer2(model3, title = "Regression summary", digits = 3, digits.extra = 6, 
           dep.var.labels = 'log(Wages)', 
           covariate.labels = c("Education", "Experience", 
                                "$\\text{Expereince}^{2}$", 
                                "Race (White or Non-white)"))

## @knitr Question4-4-2
# model4 <- lm(wage ~ education + experience + experienceSquare + 
#                raceColor, data = data)
x <- data$experience
y <- coeftest(model3, vcovHC)[1] + x*coeftest(model3, vcovHC)[3] + 
  x**2*coeftest(model3, vcovHC)[4]
plt_df <- data.frame(x,y)
ggplot(plt_df, aes(x,y)) + geom_smooth(na.rm=T) +
  labs(x = "Experience", y = "log(Wage) ($)", 
       title = "Effect of Experience on log(Wages)")

## @knitr Question4-5-1
model5 <- lm(logWage ~ education + experience + experienceSquare + raceColor + 
               dad_education + mom_education + rural + city, data = data)
# coeftest(model5, vcovHC)
stargazer2(model5, title = "Regression summary", digits = 3, digits.extra = 6,
           dep.var.labels = 'log(Wages)', 
           covariate.labels = c("Education", "Experience", 
                                "$\\text{Expereince}^{2}$",
                                "Race (White or Non-white)", 
                                "Father's Education",
                                "Mother's Education", "Rural (Yes or No)", 
                                "City (Yes or No)"))

## @knitr Question4-5-2 
dad_edu_na <- data[is.na(data$dad_education),]
mom_edu_na <- data[is.na(data$mom_education),]
actual_obs <- data[!is.na(data$mom_education) &
                     !is.na(data$dad_education),]

stargazer(data, header = F,
          summary.stat = c("n", "mean", "sd", "min", "p25", "median", "p75", 
                           "max"), title="All Observations")
stargazer(dad_edu_na, header = F,
          summary.stat = c("n", "mean", "sd", "min", "p25", "median", "p75", 
                           "max"), title="Missing Father's Education")
stargazer(mom_edu_na, header = F,
          summary.stat = c("n", "mean", "sd", "min", "p25", "median", "p75", 
                           "max"), title="Missing Mother's Education")
stargazer(actual_obs, header = F,
          summary.stat = c("n", "mean", "sd", "min", "p25", "median", "p75", 
                           "max"), title="Actual Observations")

## @knitr Question4-5-3
data7 <- data
data7$dad_education <- na.fill(data7$dad_education, mean(data7$dad_education, 
                                                         na.rm=T))
data7$mom_education <- na.fill(data7$mom_education, mean(data7$mom_education, 
                                                         na.rm=T))
model6 <- lm(logWage ~ education + experience + experienceSquare + raceColor + 
               dad_education + mom_education + rural + city, data = data7)

stargazer2(model6, title = "Regression summary", digits = 3, digits.extra = 6,
           dep.var.labels = 'log(Wages)', 
           covariate.labels = c("Education", "Experience", 
                                "$\\text{Expereince}^{2}$", 
                                "Race (White or Non-white)", 
                                "Father's Education", "Mother's Education", 
                                "Rural (Yes or No)", "City (Yes or No)"))

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
           covariate.labels = c("Education", "Experience", 
                                "$\\text{Expereince}^{2}$", 
                                "Race (White or Non-white)", 
                                "Father's Education", "Mother's Education", 
                                "Rural (Yes or No)", "City (Yes or No)"))


## @knitr Question4-5-5
stargazer2(list(model5, model6, model7), dep.var.labels = 'log(Wages)', 
           covariate.labels = c("Education", "Experience", 
                                "$\\text{Expereince}^{2}$", 
                                "Race (White or Non-white)", 
                                "Father's Education", "Mother's Education", 
                                "Rural (Yes or No)", "City (Yes or No)"))

## @knitr Question4-6-1
first_stage_a <- lm(education ~ z1, data = data )
first_stage_b <- lm(education ~ z2, data = data)
second_stage_a <- lm(data$logWage ~ first_stage_a$fitted + data$experience +
                       data$experienceSquare + data$raceColor + 
                       data$dad_education + data$mom_education + data$rural + 
                       data$city)
second_stage_b <- lm(data$logWage ~ first_stage_b$fitted + data$experience +
                       data$experienceSquare + data$raceColor + 
                       data$dad_education + data$mom_education + data$rural + 
                       data$city)
stargazer2(list(first_stage_a, first_stage_b), dep.var.labels = 'education', 
           covariate.labels = c('IV 1', 'IV 2'), 
           title="Step One Regression Summary")

## @knitr Question4-6-3
stargazer2(second_stage_b, dep.var.labels = 'log(Wages)', 
           covariate.labels = c("Education ($z_2$)", "Experience", 
                                "$\\text{Expereince}^{2}$", 
                                "Race (White or Non-white)", 
                                "Father's Education", "Mother's Education", 
                                "Rural (Yes or No)", "City (Yes or No)"),
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
          summary.stat = c("n", "mean", "sd", "min", "p25", "median", "p75", 
                           "max"), title="Summary Statistics for Voting Data")

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
           title = "Regression summary - Parsimonious Model")

## @knitr Question5-1-8
d$wealthSqr <- d$logWealth**2
m2 <- lm(voteshare~ logWealth + wealthSqr + hasWealth , data = d)
stargazer2(m1, dep.var.labels = 'Voteshare', 
           covariate.labels = c("log(Wealth)", "$\\text{log(Wealth)}^{2}$",
                                "Has Wealth = Yes"),
           title = "Regression summary - Quadratic Model")

## @knitr Question5-3-1
ggplot(d, aes(logWealth)) + geom_histogram(aes(fill=region)) +
  facet_wrap(~region,ncol=1) + scale_fill_brewer(palette="Set3") +
  labs( title = "Wealth by Region")

## @knitr Question5-3-2
ggplot(d, aes(voteshare)) + geom_histogram(aes(fill=region)) +
  facet_wrap(~region,ncol=1) + scale_fill_brewer(palette="Set3") +
  labs( title = "Voteshare by Region")

# ## @knitr Question5-1-9
# ggplot(d, aes(urb)) + geom_histogram(aes(fill=region)) +
#   facet_wrap(~region,ncol=1) + scale_fill_brewer(palette="Set3") +
#   labs( title = "Urb by Region")
# 
# ## @knitr Question5-1-9
# ggplot(d, aes(lit)) + geom_histogram(aes(fill=region)) +
#   facet_wrap(~region,ncol=1) + scale_fill_brewer(palette="Set3") +
#   labs( title = "Lit by Region")

## @knitr Question5-3-3
ggplot(d, aes(x=voteshare, y=logWealth)) + geom_point(aes(color=region)) +
  geom_smooth(method = "lm") +
  facet_wrap(~region, ncol=1) + 
  labs(title = "Scatterplot of Voteshare Against Wealth by Region")

## @knitr Question5-3-4
ggplot(d, aes(x=voteshare, y=as.numeric(hasWealth))) + 
  geom_point(aes(color=region)) + 
  geom_smooth(method = "lm") +
  facet_wrap(~region, ncol=1) + 
  labs(title = "Scatterplot of Voteshare Against Having Wealth by Region")

## @knitr Question5-3-5
m3 <- lm(voteshare~ logWealth + hasWealth + region, data = d)
stargazer2(m3, dep.var.labels = 'Voteshare', 
           covariate.labels = c("log(Wealth)", "Has Wealth = Yes", 
                                "Region = 2", "Region = 3"), 
           title = "Regression summary - Region Model")

## @knitr Question5-3-6
model_comp <- anova(m1, m3)
rownames(model_comp) <- c("Parsimonious Model", "Region Model")
stargazer(model_comp, header = F, summary=F, title = "Model Comparison")

# autoplot(m)
# autoplot(m2)
# autoplot(m3)
# summary.lm(m)
# summary.lm(m1)
# summary.lm(m2)
# summary.lm(m3)



## @knitr Question6-1
# QUESTION 6 --------------------------------------------------------------
# setwd('Lab2/data')
load("retailSales.Rdata")
data <- retailSales; rm(retailSales)
data$logRev <- log(data$Revenue + 1)
summary(data)

## @knitr Question6-2
data <- data %>% 
  mutate(Year = as.factor(Year))

## @knitr Question6-3
# data_isNA <- as.data.frame(sapply(data, is.na))
data_isNA <- data %>% mutate_each(funs(is.na(.)))
head(data_isNA)
# vars_with_NAs <- apply(data_isNA, 2, sum)
vars_with_NAs <- data_isNA %>% summarise_each(funs(sum))
(vars_with_NAs <- names(vars_with_NAs)[vars_with_NAs>0])
sapply(data_isNA[, vars_with_NAs[-1]], identical, 
       as.vector(data_isNA[, vars_with_NAs[1]]))

## @knitr Question6-4
data_categorical <- data %>% 
  select(which(names(data) %in% names(data)[sapply(data, is.factor)])) %>% 
  mutate_each(funs(as.character(.))) %>% mutate(Revenue = data$Revenue)
data_categorical %>% 
  select(Revenue, Year) %>% 
  group_by(Year) %>% 
  summarise_each(funs(100*mean(is.na(.)))) %>% 
  rename("% of NAs in numerical variables" = Revenue)
data_categorical %>% 
  select(Revenue, Product.line) %>% 
  group_by(Product.line) %>% 
  summarise_each(funs(100*mean(is.na(.)))) %>% 
  rename("% of NAs in numerical variables" = Revenue)
data_categorical %>% 
  select(Revenue, Retailer.country) %>% 
  group_by(Retailer.country) %>% 
  summarise_each(funs(100*mean(is.na(.)))) %>% 
  rename("% of NAs in numerical variables" = Revenue) %>% 
  print(n = Inf)
# data_categorical %>% 
#   select(Revenue, Product.type) %>% 
#   group_by(Product.type) %>% 
#   summarise_each(funs(100*mean(is.na(.)))) %>% 
#   rename("% of NAs in numerical variables" = Revenue) %>% 
#   print(n = Inf)
# data_categorical %>%
#   select(Revenue, Product) %>%
#   group_by(Product) %>%
#   summarise_each(funs(100*mean(is.na(.)))) %>%
#   rename("% of NAs in numerical variables" = Revenue) %>%
#   print(n = Inf)
# data_categorical %>%
#   select(Revenue, Order.method.type) %>%
#   group_by(Order.method.type) %>%
#   summarise_each(funs(100*mean(is.na(.)))) %>% 
#   rename("% of NAs in numerical variables" = Revenue)

## @knitr Question6-5
data <- data %>% na.omit()
data_categorical <- data %>% 
  select(which(names(data) %in% names(data)[sapply(data, is.factor)]))
data_non_categorical <- data %>% 
  select(which(names(data) %in% names(data)[!sapply(data, is.factor)]))
round(stat.desc(data_non_categorical, desc = TRUE, basic = TRUE), 2)

## @knitr Question6-6
data_melt <- data_non_categorical %>% gather(variable, value)
ggplot(data_melt, aes(value)) + 
  geom_histogram(aes(y = ..count..), color = "black", fill = "white", 
                 bins = 40) + 
  facet_wrap(~ variable, scales = "free", ncol = 4) + 
  labs(x = "Variable Value", y = "Number of observations", 
       title = "Histogram of all numerical variables in the dataset")

## @knitr Question6-7
cor(data_non_categorical)
data_subsample <- data_non_categorical %>% sample_n(500)
pairs(data_subsample)

## @knitr Question6-8
ggpairs(data_subsample) + 
  theme(axis.ticks = element_blank(), axis.text =  element_blank()) 

## @knitr Question6-9
# Year back to integer (factor only useful for vizs)
data <- data %>% mutate(Year = as.numeric(levels(Year))[Year] - 2004)
# One dataset per couple of years
# train <- data %>% filter(Year <= 2005)
# test <- data %>% filter(Year > 2005)

train <- data %>% filter(Year <= 1)
test <- data %>% filter(Year > 1)

## @knitr Question6-10
# Re-factor Product (since the levels differ by period)
train$Product <- factor(train$Product)
test$Product <- factor(test$Product)
train_levels <- levels(train$Product)
test_levels <- levels(test$Product)
drop_levels <- setdiff(train_levels, test_levels)
train <- train[!train$Product %in% drop_levels,]
test <- test[!test$Product %in% drop_levels,]
train$Product <- factor(train$Product)
test$Product <- factor(test$Product)
train_levels <- levels(train$Product)
test_levels <- levels(test$Product)
drop_levels <- setdiff(test_levels, train_levels)
train <- train[!train$Product %in% drop_levels,]
test <- test[!test$Product %in% drop_levels,]
train$Product <- factor(train$Product)
test$Product <- factor(test$Product)

## @knitr Question6-11
head(data %>% select(Revenue, Product.cost, Gross.profit) %>% 
       mutate(Revenue2 = Product.cost + Gross.profit))
all(round(data$Revenue - data$Product.cost, 2) == round(data$Gross.profit, 2))
head(data %>% select(Revenue, Unit.sale.price, Quantity) %>% 
       mutate(Revenue2 = Unit.sale.price * Quantity))

## @knitr Question6-12
# Looking at Correlations with log(Revenue)
train_num <- train %>% 
  select(which(names(train) %in% names(train)[!sapply(train, is.factor)]))
cor(train_num$Revenue, train_num)

## @knitr Question6-13
# Examine One factor models
params2 <- c("Product")
model2 <- lm(as.formula(paste("logRev", paste(params2, sep = "", 
                                              collapse = " + "), 
                              sep = " ~ ")), train)
params3 <- c("Product.line")
model3 <- lm(as.formula(paste("logRev", paste(params3, sep = "", 
                                              collapse = " + "), 
                              sep = " ~ ")), train)
params4 <- c("Order.method.type")
model4 <- lm(as.formula(paste("logRev", paste(params4, sep = "", 
                                              collapse = " + "), 
                              sep = " ~ ")), train)
params5 <- c("Retailer.country")
model5 <- lm(as.formula(paste("logRev", paste(params5, sep = "", 
                                              collapse = " + "), 
                              sep = " ~ ")), train)
params6 <- c("Product.type")
model6 <- lm(as.formula(paste("logRev", paste(params6, sep = "", 
                                              collapse = " + "), 
                              sep = " ~ ")), train)
params7 <- c("Product", "Order.method.type")
model7 <- lm(as.formula(paste("logRev", paste(params7, sep = "", 
                                              collapse = " + "), 
                              sep = " ~ ")), train)
params8 <- c("Product", "Order.method.type", "Retailer.country")
model8 <- lm(as.formula(paste("logRev", paste(params8, sep = "", 
                                              collapse = " + "), 
                              sep = " ~ ")), train)
AIC(model7)
AIC(model8)


## @knitr Question6-14
# Examine One factor models
ggplot(train, aes(logRev)) + geom_histogram(aes(fill=Order.method.type)) + 
  facet_wrap(~Retailer.country, ncol=3) +
  labs(title="Histogram of Revenuew By Country and Order Type", 
       x="log(Revenue" )


## @knitr Question6-15
params9 <- c("Product", "Order.method.type", "Retailer.country")
params_plus_interaction <- c(params9, 'Order.method.type*Retailer.country')
vars_of_interest <- c('logRev', params9)
model9 <- lm(as.formula(paste(vars_of_interest[!vars_of_interest %in% params9], 
                              paste(params_plus_interaction, sep = "", 
                                    collapse = " + "), sep = " ~ ")), train)
summary.lm(model9)
AIC(model9)

## @knitr Question6-16
predictions = predict.lm(model8, test[,params8])
test$predictions <- predictions
summary.lm(lm(logRev~predictions, test))

## @knitr Question6-17
params = c("Planned.revenue")
model1 <- lm(as.formula(paste("Revenue", paste(params, sep = "",
                                               collapse = " + "),
                              sep = " ~ ")), train)
model1_full <- lm(as.formula(paste("Revenue", paste(params, sep = "", 
                                                    collapse = " + "), 
                                   sep = " ~ ")), data)
coeftest(model1_full, vcov = vcovHC)
linearHypothesis(model1_full, "Planned.revenue = 0.95", vcov = vcovHC)
linearHypothesis(model1_full, paste("Planned.revenue =", 
                                    coeftest(model1, vcov = vcovHC)[2, 1]), 
                 vcov = vcovHC)

## @knitr Question6-1000
params = c("Year", "Planned.revenue")
model2 <- lm(as.formula(paste("Revenue", paste(params, sep = "", 
                                               collapse = " + "), 
                              sep = " ~ ")), train)
coeftest(model2, vcov = vcovHC)
model2_predictions <- predict(model2, test[, params], 
                              interval = "prediction")
matplot(test[order(test$Planned.revenue) , c("Planned.revenue")], 
        cbind(model2_predictions[order(test$Planned.revenue), ], 
              sort(test$Revenue)), lty = c(2,3,3,1), type = "l", 
        xlab = "Planned Revenue", 
        ylab = "Revenue (observed and predicted)")
(RMSE <- sqrt(sum((model2_predictions[, 1] - test$Revenue)^2) / 
                dim(test)[1]))




data %>% group_by(Year, Order.method.type) %>% 
  summarise(Total.Revenue = sum(Revenue)) %>% 
  mutate("Percentage of Revenue" = 100*Total.Revenue/sum(Total.Revenue)) %>% 
  print(n = Inf)
data %>% group_by(Order.method.type) %>% 
  summarise(Total.Revenue = sum(Revenue)) %>% 
  mutate("Percentage of Revenue" = 100*Total.Revenue/sum(Total.Revenue)) %>% 
  print(n = Inf)



# Full model (without interaction terms)
params = names(data)[-which(names(data) == "Revenue")]
params = c("Year", "Planned.revenue", "Retailer.country", "Product")
params = names(data)[which(!names(data) %in% c("Revenue", "Gross.profit", 
                                               "Product.cost", 
                                               "Unit.sale.price"))]
model3 <- lm(as.formula(paste("Revenue", paste(params, sep = "", 
                                               collapse = " + "), 
                              sep = " ~ ")), train)
coeftest(model3, vcov = vcovHC)
model3_predictions <- predict(model3, test[, params], 
                              interval = "prediction")
matplot(test[order(test$Planned.revenue) , c("Planned.revenue")], 
        cbind(model3_predictions[order(test$Planned.revenue), ], 
              sort(test$Revenue)), lty = c(2,3,3,1), type = "l", 
        xlab = "Planned Revenue in 2006 and 2007", 
        ylab = "Revenue in 2006 and 2007 (observed and predicted)")
RMSE <- sqrt(sum((model3_predictions[, 1] - test$Revenue)^2) / 
               dim(test)[1])
RMSE





coeftest(model1, vcov = vcovHC)
linearHypothesis(model1, "Planned.revenue = 0.95", vcov = vcovHC)
linearHypothesis(model1, 
                 paste("Planned.revenue =", coeftest(model1, vcov = vcovHC)[2, 1]), 
                 vcov = vcovHC)






ggplot(data, aes(Year, Revenue, fill = Product.line)) + 
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", 
               position = position_dodge(width = 0.9), width = 0.2) + 
  facet_wrap(~ Product.line, ncol = 3, scales = "free")

ggplot(data, aes(Year, Revenue, fill = Order.method.type)) + 
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", 
               position = position_dodge(width = 0.9), width = 0.2) + 
  facet_wrap(~ Order.method.type, ncol = 3)

ggplot(data, aes(Product.type, Revenue)) + geom_boxplot()
summary(data$Revenue)
