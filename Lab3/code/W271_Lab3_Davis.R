## MIDS W271-4 Lab3           ##
## Carin, Davis, Levato, Song ##


## @knitr Libraries-Functions-Constants
# LIBRARIES, FUNCTIONS AND CONSTANTS -------------------------------------------------
# Load libraries
# library(e1071)
library(pastecs)
library(lubridate)
library(GGally)
library(forecast)
library(ggplot2)
library(tseries)
library(gtools)
library(plyr)
library(dplyr)
library(cowplot)
library(ggfortify)
library(scatterplot3d)
library(scales)
library(knitr)
library(car)
library(sandwich)
library(lmtest)
library(tidyr)
library(stargazer)
library(pander)
# library(texreg)
# library(weatherData)
library(scales)
library(xts)
library(reshape2)
library(zoo)

library(fGarch)
library(quantmod)
library(lattice)
library(corrgram)
#library(TSA)

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

# Define constants
par(cex.main = 1, cex.lab = 0.9, cex.axis = 0.9)
set.seed(1234)



## @knitr ex1-load
# Loading the Data --------------------------------------------------------
# setwd('./HW8/data')
ex1df <- read.csv("houseValueData.csv")
round(stat.desc(ex1df, desc = FALSE, norm = TRUE)[c("nbr.val", "nbr.na", 
                                                         "skewness", 
                                                         "kurtosis", 
                                                         "normtest.p"),], 3)

## @knitr ex1-summary_table
# Exploratory analysis
stargazer(ex1df, header = F, title ="Summary Statistics of Wage Data",
          summary.stat = c("n", "mean", "sd", "min", "p25", "median", "p75", 
                           "max"))

## @knitr ex1-CrimeRate
# Not sure if this is percentile data, but that's what I'm assuming

crime_plot <- ggplot(data=ex1df, aes(crimeRate_pc)) + 
  geom_histogram(aes(y = (..count..)/sum(..count..)), boundary = 0, 
                 binwidth = 2.5, colour = 'black', fill = 'white') + 
  labs(title = "Histogram of crimeRate", 
       x = "Number of crimes per 1,000 residents", 
       y = "Relative Frequency") + theme_gray()
logCrime_plot <- ggplot(data=ex1df, aes(log(crimeRate_pc))) + 
  geom_histogram(aes(y = (..count..)/sum(..count..)), 
                 binwidth = .25, colour = 'black', fill = 'white') +
  labs(title = "Histogram of log(crimeRate)",
       x = "Log(Number of crimes per 1,000 residents)", 
       y = "Relative Frequency") +  theme_gray()
plot_grid(crime_plot, logCrime_plot)

## @knitr ex1-Business
# Kind of confused what this variable is. My guess is thhe percentage of businesses that are non-retail. 
# Maybe this is a good candidate variabel for instrumental variable??
business_plot <- ggplot(data=ex1df, aes(nonRetailBusiness)) + 
  geom_histogram(aes(y=(..count..)/sum(..count..)), boundary = 0, 
                 binwidth = .01, colour = 'black', fill = 'white')+
  labs(title="Histogram of nonRetailBusiness",
       x = "Proportion of acres dedicated to non-retail business", 
       y="Relative Frequency") + theme_gray()
logBusiness_plot <- ggplot(data=ex1df, aes(log(nonRetailBusiness))) + 
  geom_histogram(aes(y=(..count..)/sum(..count..)), binwidth = .15, 
                 colour='black', fill='white') + 
  labs(title="Histogram of log(nonRetailBusiness)",
       x = "Log(Prop. of acres dedicated to non-retail business)",
       y="Relative Frequency") + theme_gray()
plot_grid(business_plot, logBusiness_plot)

# Perhaps we can group these into  larger grids for the final paper


## @knitr ex1-withWater

#binary variable for water, unclear if this means water within some distance or water on the property
ggplot(data=ex1df, aes(factor(withWater, labels = c('Not near Water', 
                                                         'Near Water')))) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), colour='black', fill='white') + 
  labs(title = "Proportion of houses within 5 miles of a water body", 
       y = "Proportion", x = element_blank()) + theme_gray()


## @knitr ex1-houseAge

# Another possible candidate for an instrumental variable approach? 
house_plot <- ggplot(data=ex1df, aes(ageHouse)) +
  geom_histogram(aes(y=(..count..)/sum(..count..)),
                 boundary=0, binwidth = 5,
                 colour='black', fill='white') +
  labs(title="Histogram of ageHouse",
       x= "Proportion of house built before 1950",
       y= "Relative Frequency") + theme_gray()
logHouse_plot <- ggplot(data=ex1df, aes(log(ageHouse))) +
  geom_histogram(aes(y=(..count..)/sum(..count..)),
                 binwidth = 0.2,
                 colour='black', fill='white') +
  labs(title="Histogram of log(ageHouse)",
       x= "Log(proportion of house built before 1950)",
       y= "Relative Frequency") + theme_gray()
plot_grid(house_plot, logHouse_plot)

## @knitr ex1-cityDistance
# Urban/Rural variable, interesting to see how it tracks with business variable
# Units unclear

city_plot <- ggplot(data=ex1df, aes(distanceToCity)) + 
  geom_histogram(aes(y=(..count..)/sum(..count..)),
                 boundary=0, binwidth = 1.5,
                 colour='black', fill='white') +
  labs(title="Histogram of distanceToCity",
       x = "Distance to nearest city (in miles)",
       y = "Relative Frequency") + theme_gray()
logCity_plot <- ggplot(data=ex1df, aes(log(distanceToCity))) +
  geom_histogram(aes(y=(..count..)/sum(..count..)),
                 binwidth = 0.2,
                 colour='black', fill='white') +
  labs(title="Histogram of log(distanceToCity)",
       x = "Log(Distance to nearest city, in miles)",
       y  = "Relative Frequency") + theme_gray()
plot_grid(city_plot, logCity_plot)

## @knitr ex1-highwayDistance
highway_plot <- ggplot(ex1df, aes(distanceToHighway)) +
  geom_histogram(aes(y=(..count..)/sum(..count..)),
                 boundary=0, binwidth = .75,
                 colour='black', fill='white') +
  labs(title="Histogram of Distance to Highway",
       x="Distance to Highway (miles)",
       y="Relative Frequency") + theme_gray()
highway_plot

# Very few values, could be a factor variable, not sure. 

## @knitr ex1-classSize
class_plot <- ggplot(ex1df, aes(pupilTeacherRatio)) +
  geom_histogram(aes(y=(..count..)/sum(..count..)),
                binwidth = .40,
                 colour='black', fill='white') +
  labs(title="Histogram of Average Pupil to Teacher Ratio",
       x="Pupil to Teacher Ratio",
       y="Relative Frequency") + theme_gray()
class_plot

## @knitr ex1-lowIncome
# Not clear if this is percent low income housing or percent low income households
lowIncome_plot <- ggplot(ex1df, aes(pctLowIncome)) +
  geom_histogram(aes(y=(..count..)/sum(..count..)),
                 binwidth=2,
                 colour='black', fill='white') +
  labs(title="Histogram of Percentage Low Income",
       x="Low Income Percent", 
       y="Relative Frequency") + theme_gray()

logLowIncome_plot <- ggplot(ex1df, aes(log(pctLowIncome))) +
  geom_histogram(aes(y=(..count..)/sum(..count..)),
                 binwidth=0.2,
                 colour='black', fill='white') +
  labs(title='Histogram of log(Percentage Low Income)',
       x='log(Low Income Percent)',
       y='Relative Frequency') + theme_gray()
plot_grid(lowIncome_plot, logLowIncome_plot)

## @knitr ex1-homeValue
homeValue_plot <- ggplot(ex1df, aes(homeValue)) +
  geom_histogram(aes(y=(..count..)/sum(..count..)),
                 binwidth=60000,
                 colour='black', fill='white') +
  labs(title="Histogram of Median Home Values",
       x="Median Home Value ($)",
       y="Relative Frequency") + theme_gray()

logHomeValue_plot <- ggplot(ex1df, aes(log(homeValue))) +
  geom_histogram(aes(y=(..count..)/sum(..count..)),
                 binwidth=.2,
                 colour='black', fill='white') +
  labs(title="Histogram of log(Home Values)",
       x="log(Home Value) ($)",
       y="Relative Frequency") + theme_gray()

plot_grid(homeValue_plot, logHomeValue_plot)

## @knitr ex1-pollution

pollution_plot <- ggplot(ex1df, aes(pollutionIndex)) +
  geom_histogram(aes(y=(..count..)/sum(..count..)),
                 binwidth=2,
                 color='black', fill='white') +
  labs(title="Histogram of Pollution Index",
       x="Pollution Index Score",
       y="Relative Frequency") + theme_gray()

logPollution_plot <- ggplot(ex1df, aes(log(pollutionIndex))) +
  geom_histogram(aes(y=(..count..)/sum(..count..)),
                 binwidth=.075,
                 color='black', fill='white') +
  labs(title="Histogram of log(Pollution Index)",
       x="log(Pollution Index Score)",
       y="Relative Frequency") + theme_gray()

plot_grid(pollution_plot, logPollution_plot)

## @knitr ex1-beds

beds_plot <- ggplot(ex1df, aes(nBedRooms)) +
  geom_histogram(aes(y=(..count..)/sum(..count..)),
                 binwidth = 0.25,
                 colour='black', fill='white') +
  labs(title="Histogram of Average Number of Beds",
       x="Number of Beds",
       y="Relative Frequency") + theme_gray()
beds_plot

## @knitr ex1-houseScatters
s_df <- ex1df
s_df$crimeRate_pc <- log(s_df$crimeRate_pc)
s_df$nonRetailBusiness <- log(s_df$nonRetailBusiness)
s_df$distanceToCity <- log(s_df$distanceToCity)
s_df$pctLowIncome <- log(s_df$pctLowIncome)
s_df$homeValue <- log(s_df$homeValue)
s_df$pollutionIndex <- log(s_df$pollutionIndex)
names(s_df)[c(1,2,5,8,9,10)] <- c("log_crimeRate_pc","log_nonRetailBusiness",
                                  "log_distanceToCity", "log_pctLowIncome",
                                  "log_homeValue", "log_pollutionIndex")
houseScatter_df <- melt(s_df, id.vars="log_homeValue")
univar_scatters_homeVal <- ggplot(houseScatter_df, aes(log_homeValue, value)) +
  geom_point() + geom_smooth(method = "lm") +
  facet_wrap(~variable, scales = "free", ncol=2) +
  labs(title = "Scatterplot of log(Home Value) Against Variables of Interest",
       x="log(Home Value)") +
  theme_gray()
univar_scatters_homeVal

## @knitr ex1-univarRegressions
params <- names(s_df)[c(1:8, 10,11)]
models <- lapply(params, function(x) {
  lm(substitute(log_homeValue ~ i, list(i = as.name(x))), data = s_df)
})
robust_summaries <- lapply(models, coeftest, vcoc=vcovHC)
summaries <- lapply(models, summary.lm)
coefs <- lapply(robust_summaries, function(x) round(x[2],3))
pvals <- lapply(robust_summaries, function(x) round(x[8],3))
r_squareds <- lapply(summaries, function(x) round(x$r.squared,3))
univar_regs <- data.frame(cbind(params, coefs, pvals, r_squareds))
names(univar_regs) <- c("Variable", "Coefficient", "P-value", "R-Squared")
stargazer(univar_regs, title="Univariate Regressions Against House Value",
          summary = F, header = F)

## @knitr ex1-houseCorrelations
# Calculate correlation matrix
corrs <- cor(ex1df)
# Check if any variables other than the diagonals are perfectly correlated
length(corrs[corrs == 1])
# Check if any variables have a correlation coefficient above .9
length(corrs[corrs > 0.9 & corrs != 1])

## @knitr ex1-polluteScatters
polluteScatter_df <- melt(s_df, id.vars="log_pollutionIndex")
univar_scatters_pollute <- ggplot(polluteScatter_df, aes(log_pollutionIndex, value)) +
  geom_point() + geom_smooth(method = "lm") +
  facet_wrap(~variable, scales = "free", ncol=2) +
  labs(title = "Scatterplot of log(Pollution Index) Against Variables of Interest",
       x="log(Pollution Index)") +
  theme_gray()
univar_scatters_pollute

## @knitr ex1-waterScatters
waterScatter_df <- melt(s_df, id.vars="withWater")
univar_scatters_water <- ggplot(waterScatter_df, aes(withWater, value)) +
  geom_point() + geom_smooth(method = "lm") +
  facet_wrap(~variable, scales = "free", ncol=2) +
  labs(title = "Scatterplot of Water Presence/Absence Against Variables of Interest",
       x="Water Present/Absence") +
  theme_gray()
univar_scatters_water

## @knitr ex1-baseModel
baseModel <- lm(log_homeValue~log_pollutionIndex + withWater + nBedRooms +
                log_pctLowIncome, data = s_df)
# coeftest(baseModel, vcov=vcovHC)
stargazer2(baseModel, title = "Base Model Regression summary", digits = 3, digits.extra = 6, 
           dep.var.labels = 'log(home value)', 
           covariate.labels = c("log(PollutionIndex)", "Water Absence/Presence",
                                "Number of Bedrooms", "log(Percentage Low Income Housing"))

## @knitr ex1-baseModelPlot
autoplot(baseModel) + theme_gray()

## @knitr ex1-stepWise
base_model <- lm(log_homeValue~log_pollutionIndex + withWater + nBedRooms +
                   log_pctLowIncome, data = s_df)
full <- lm(log_homeValue~., data=s_df)
step(base_model, scope=list(lower=base_model, upper=full), direction="both")
stepModel <- lm(formula = log_homeValue ~ log_pollutionIndex + withWater + 
                    nBedRooms + log_pctLowIncome + pupilTeacherRatio + log_distanceToCity + 
                    log_crimeRate_pc + ageHouse, data = s_df)

## @knitr ex1-outSampleFit

# Split data into training and testing portions
set.seed(1099)
train <- sample_frac(s_df, 0.8)
r_id <- as.numeric(rownames(train))
test <- s_df[-r_id,]

# Define the base model and add parameters
base_params <- colnames(s_df)[c(10, 3, 11, 8)]
base <- lm(log_homeValue~log_pollutionIndex + withWater + nBedRooms +
             log_pctLowIncome, data = train)
plus_one_params <- colnames(s_df)[c(10, 3, 11, 8, 7)]
plus_one <- lm(log_homeValue~log_pollutionIndex + withWater + nBedRooms +
                 log_pctLowIncome
               + pupilTeacherRatio, data = train)
plus_two_params <- colnames(s_df)[c(10, 3, 11, 8, 7, 5)]
plus_two <- lm(log_homeValue~log_pollutionIndex + withWater + nBedRooms +
                 log_pctLowIncome +
               pupilTeacherRatio + log_distanceToCity, data = train)
plus_three_params <- colnames(s_df)[c(10, 3, 11, 8, 7, 5, 1)]
plus_three <- lm(log_homeValue~log_pollutionIndex + withWater + nBedRooms +
                   log_pctLowIncome +
                   pupilTeacherRatio + log_distanceToCity + log_crimeRate_pc,
                 data = train)
plus_four_params <- colnames(s_df)[c(10, 3, 11, 8, 7, 5, 1, 4)]
plus_four <- lm(log_homeValue~log_pollutionIndex + withWater + nBedRooms +
                  log_pctLowIncome +
                  pupilTeacherRatio + log_distanceToCity + log_crimeRate_pc +
                  ageHouse, data = train)

# Use each model to predict the outcome variable in the test data
base_preds <- predict(base, test[,base_params], interval = "prediction")
plus_one_preds <- predict(plus_one, test[, plus_one_params],
                          interval = "prediction")
plus_two_preds <- predict(plus_two, test[, plus_two_params],
                          interval = "prediction")
plus_three_preds <- predict(plus_three, test[, plus_three_params],
                            interval = "prediction")
plus_four_preds <- predict(plus_four, test[, plus_four_params],
                           interval = "prediction")

# Summarize the model predictions and AIC, BIC in a dataframe
pred_df <- data.frame(rbind(accuracy(base_preds[,1],
                                     test$log_homeValue)[,c("RMSE", "MAE")],
      accuracy(plus_one_preds[,1], test$log_homeValue)[,c("RMSE", "MAE")],
      accuracy(plus_two_preds[,1], test$log_homeValue)[,c("RMSE", "MAE")],
      accuracy(plus_three_preds[,1],test$log_homeValue)[,c("RMSE", "MAE")],
      accuracy(plus_four_preds[,1], test$log_homeValue)[,c("RMSE", "MAE")]))

pred_df$model <- c("base", "plus_one", "plus_two", "plus_three", "plus_four")
diag_df <- data.frame(
  cbind(BIC(base, plus_one, plus_two, plus_three, plus_four), 
        AIC=AIC(base, plus_one, plus_two, plus_three, plus_four)[, 2]))
rownames(diag_df) <- c(1:5)
model_df <- data.frame(cbind(model =pred_df[,3],diag_df[, 1:3], pred_df[,c(1,2)]))

## @knitr ex1-forecastTable
# View(model_df)
stargazer(model_df, title="Summary of Model Diagnoistics and Out-Of-Sample Fit",
          summary = F, header = F)

## @knitr ex1-model3
model3 <- lm(log_homeValue~log_pollutionIndex + withWater + nBedRooms + log_pctLowIncome +
               pupilTeacherRatio + log_distanceToCity + log_crimeRate_pc, data = s_df)
stargazer2(model3, title = "Perfered Model Regression summary", digits = 3, digits.extra = 6, 
           dep.var.labels = 'log(home value)', 
           covariate.labels = c("log(PollutionIndex)", "Water Absence/Presence",
                                "Number of Bedrooms", "log(Percentage Low Income Housing)",
                                "Pupil to Teacher Ratio", "log(Distance to City)",
                                "log(Average Crime Rate)"))
# model_check <- s_df[-c(370,378),]
# model3_check <- lm(log_homeValue~log_pollutionIndex + withWater + nBedRooms + log_pctLowIncome +
#                      pupilTeacherRatio + log_distanceToCity + log_crimeRate_pc, data = model_check)
# 
# coeftest(model3_check, vcov=vcovHC)
# coeftest(model3, vcov=vcovHC)

## @knitr ex1-model3Plot
autoplot(model3) + theme_gray()

## @knitr ex1-interaction
inter_model <- lm(log_homeValue~log_pollutionIndex + withWater + nBedRooms + log_pctLowIncome +
                    pupilTeacherRatio + log_distanceToCity + log_crimeRate_pc + 
                    log_pollutionIndex * log_pctLowIncome, data=s_df)
# coeftest(inter_model, vcov=vcovHC)
stargazer2(inter_model,title = "Interaction Effect Regression summary", digits = 3, digits.extra = 6, 
           dep.var.labels = 'log(home value)', 
           covariate.labels = c("log(PollutionIndex)", "Water Absence/Presence",
                                "Number of Bedrooms", "log(Percentage Low Income Housing)",
                                "Pupil to Teacher Ratio", "log(Distance to City)",
                                "log(Average Crime Rate)", "log\\_PI x log\\_LIH"))

# poll <- seq(3, 5, .01)
# Income_interp <- 10.6 + (1.38 - .481 * poll)
# pollut_interp <- 10.6 + (1.05 - .481 * x)
# plot(poll, Income_interp, type='l')
# plot(x, pollut_interp, type='l')
# lines(pollut_interp)
# model3 <- lm(log_homeValue~log_pollutionIndex + withWater + nBedRooms + log_pctLowIncome +
#                    pupilTeacherRatio + log_distanceToCity + log_crimeRate_pc, data = s_df)
# coeftest(model3, vcov=vcovHC)

## @knitr ivReg

step_one <- lm(log_pollutionIndex ~ distanceToHighway, data = s_df)
step_two <- lm(log_homeValue~step_one$fitted + withWater + nBedRooms + log_pctLowIncome +
                 pupilTeacherRatio + log_distanceToCity + log_crimeRate_pc, data = s_df)
coeftest(step_one, vcov=vcovHC)
coeftest(step_two, vcov=vcovHC)

step_onea <- lm(withWater ~ log_nonRetailBusiness, data = s_df)
step_twoa <- lm(log_homeValue~log_pollutionIndex + step_onea$fitted + nBedRooms + log_pctLowIncome +
                 pupilTeacherRatio + log_distanceToCity + log_crimeRate_pc, data = s_df)

coeftest(step_onea, vcov=vcovHC)
summary.lm(step_onea)
coeftest(step_twoa, vcov=vcovHC)

step_onec <- lm(log_pollutionIndex ~ log_nonRetailBusiness, data = s_df)
step_twoc <- lm(log_homeValue~step_onec$fitted + withWater + nBedRooms + log_pctLowIncome +
                  pupilTeacherRatio + log_distanceToCity + log_crimeRate_pc, data = s_df)

coeftest(step_onec, vcov=vcovHC)
summary.lm(step_onec)
coeftest(step_twoc, vcov=vcovHC)

## @knitr ex1-IVRegTable
step_onec <- lm(log_pollutionIndex ~ log_nonRetailBusiness, data = s_df)
step_twoc <- lm(log_homeValue~step_onec$fitted + withWater + nBedRooms + log_pctLowIncome +
                  pupilTeacherRatio + log_distanceToCity + log_crimeRate_pc, data = s_df)
stargazer2(step_twoc, title = "Instrumental Variable Regression summary", digits = 3, digits.extra = 6, 
           dep.var.labels = 'log(home value)', 
           covariate.labels = c("log(PollutionIndex)", "Water Absence/Presence",
                                "Number of Bedrooms", "log(Percentage Low Income Housing)",
                                "Pupil to Teacher Ratio", "log(Distance to City)",
                                "log(Average Crime Rate)"))

## @knitr scratch
plot(ex1df$crimeRate_pc, ex1df$homeValue)
abline(lm(homeValue~crimeRate_pc, data=ex1df))

plot(ex1df$nonRetailBusiness, ex1df$homeValue)
abline(lm(homeValue~nonRetailBusiness, data=ex1df))

plot(ex1df$withWater, ex1df$homeValue)
abline(lm(homeValue~withWater, data=ex1df))

plot(ex1df$ageHouse, ex1df$homeValue)
abline(lm(homeValue~ageHouse, data=ex1df))

plot(ex1df$distanceToCity, ex1df$homeValue)
abline(lm(homeValue~distanceToCity, data=ex1df))

plot(ex1df$distanceToHighway, ex1df$homeValue)
abline(lm(homeValue~distanceToHighway, data=ex1df))

plot(ex1df$pupilTeacherRatio, ex1df$homeValue)
abline(lm(homeValue~pupilTeacherRatio, data=ex1df))

plot(ex1df$pctLowIncome, ex1df$homeValue)
abline(lm(homeValue~pctLowIncome, data=ex1df))

plot(ex1df$pollutionIndex, ex1df$homeValue)
abline(lm(homeValue~pollutionIndex, data=ex1df))

plot(ex1df$nBedRooms, ex1df$homeValue)
abline(lm(homeValue~nBedRooms, data=ex1df))

# Note there doesn't seem to be any variables that are uncorrelated with homeValue, the variable of interest
# This leads me to believe none of these variables are suitable for an instrumental variable approach.

corrs <- cor(ex1df)
length(corrs[corrs == 1])
length(corrs[corrs > 0.9 & corrs != 1])
# At least for single-variable comparisons, there doesn't seem to be an issue with multi-colinearity. 

# Construct a data frame of all univariate regressors with homeValue, their coefficients, and their p-values
params <- names(ex1df)[c(1:8, 10,11)]
models <- lapply(params, function(x) {
  lm(substitute(log(homeValue) ~ i, list(i = as.name(x))), data = ex1df)
})
robust_summaries <- lapply(models, coeftest, vcoc=vcovHC)
summaries <- lapply(models, summary.lm)
coefs <- lapply(robust_summaries, function(x) x[2])
pvals <- lapply(robust_summaries, function(x) x[8])
r_squareds <- lapply(summaries, function(x) x$r.squared)
univar_regs <- data.frame(cbind(params, coefs, pvals, r_squareds))
stargazer(univar_regs, title="Univariate Regressions Against House Value", 
          flip=T, summary = F, header = F)




# Note that non of the uni-variate regressors are non signifact in their relationship with homeValue. While this does
# not preclude their use as instruments, it gives us pause about using them in an instrumental variables approach.
# Certainly, if we end up estimating a model with numerous controls, and the coefficients for the potential
# instruments are still significant, they do not represent suitable insturments.

polluteScatter_df <- melt(s_df, id.vars="log_pollutionIndex")
univar_scatters_pollute <- ggplot(polluteScatter_df, aes(log_pollutionIndex, value)) +
  geom_point() + geom_smooth(method = "lm") +
  facet_wrap(~variable, scales = "free", ncol=2) +
  labs(title = "Scatterplot of log(Pollution Index) Against Variables of Interest",
       x="log(Pollution Index)") +
  theme_gray()
univar_scatters_pollute

