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



## @knitr Question6-1
# QUESTION 6 --------------------------------------------------------------
# setwd('Lab2/data')
load("retailSales.Rdata")
data <- retailSales; rm(retailSales)
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
# data_200405 <- data %>% filter(Year <= 2005)
# data_200607 <- data %>% filter(Year > 2005)
data_200405 <- data %>% filter(Year <= 1)
data_200607 <- data %>% filter(Year > 1)

## @knitr Question6-10
# Re-factor Product (since the levels differ by period)
products_200405 <- data.frame(Product = levels(droplevels(data_200405$Product)))
products_200607 <- data.frame(Product = levels(droplevels(data_200607$Product)))
continuing_products <- intersect(products_200405, products_200607)
(new_or_discontinuted_products <- union(products_200405, products_200607) %>% 
  setdiff(continuing_products))
# Products present in one period and not the other are labelled as "Other"
data_200405 <- data_200405 %>% 
  mutate(Product = ifelse(Product %in% new_or_discontinuted_products$Product, 
                          "Other", as.character(Product))) %>% 
  mutate(Product = factor(Product))
data_200607 <- data_200607 %>% 
  mutate(Product = ifelse(Product %in% new_or_discontinuted_products$Product, 
                          "Other", as.character(Product))) %>% 
  mutate(Product = factor(Product))

## @knitr Question6-11
head(data %>% select(Revenue, Product.cost, Gross.profit) %>% 
       mutate(Revenue2 = Product.cost + Gross.profit))
all(round(data$Revenue - data$Product.cost, 2) == round(data$Gross.profit, 2))
head(data %>% select(Revenue, Unit.sale.price, Quantity) %>% 
       mutate(Revenue2 = Unit.sale.price * Quantity))

## @knitr Question6-12
# Simplest model
params = c("Planned.revenue")
model1 <- lm(as.formula(paste("Revenue", paste(params, sep = "", 
                                               collapse = " + "), 
                              sep = " ~ ")), data_200405)
coeftest(model1, vcov = vcovHC)
new_data <- data.frame(data_200607[, params])
names(new_data) <- params
model1_predictions <- predict(model1, new_data, interval = "prediction")
matplot(data_200607[order(data_200607$Planned.revenue) , c("Planned.revenue")], 
        cbind(model1_predictions[order(data_200607$Planned.revenue), ], 
              sort(data_200607$Revenue)), lty = c(2,3,3,1), type = "l", 
        xlab = "Planned Revenue", 
        ylab = "Revenue (observed and predicted)")

## @knitr Question6-13
(RMSE <- sqrt(sum((model1_predictions[, 1] - data_200607$Revenue)^2) / 
                dim(data_200607)[1]))

## @knitr Question6-14
model1_full <- lm(as.formula(paste("Revenue", paste(params, sep = "", 
                                               collapse = " + "), 
                              sep = " ~ ")), data)
coeftest(model1_full, vcov = vcovHC)
linearHypothesis(model1_full, "Planned.revenue = 0.95", vcov = vcovHC)
linearHypothesis(model1_full, paste("Planned.revenue =", 
                                    coeftest(model1, vcov = vcovHC)[2, 1]), 
                 vcov = vcovHC)

## @knitr Question6-15
params = c("Year", "Planned.revenue")
model2 <- lm(as.formula(paste("Revenue", paste(params, sep = "", 
                                               collapse = " + "), 
                              sep = " ~ ")), data_200405)
coeftest(model2, vcov = vcovHC)
model2_predictions <- predict(model2, data_200607[, params], 
                              interval = "prediction")
matplot(data_200607[order(data_200607$Planned.revenue) , c("Planned.revenue")], 
        cbind(model2_predictions[order(data_200607$Planned.revenue), ], 
              sort(data_200607$Revenue)), lty = c(2,3,3,1), type = "l", 
        xlab = "Planned Revenue", 
        ylab = "Revenue (observed and predicted)")
(RMSE <- sqrt(sum((model2_predictions[, 1] - data_200607$Revenue)^2) / 
                dim(data_200607)[1]))



## @knitr Question6-1000
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
params = c("Year", "Planned.revenue", "Retailer.country", "Product.cost", 
           "Quantity", "Unit.cost", "Unit.price", "Gross.profit", "Product", 
           "Unit.sale.price")
params = names(data)[which(!names(data) %in% c("Revenue", "Gross.profit", 
                                               "Product.cost", 
                                               "Unit.sale.price"))]
model3 <- lm(as.formula(paste("Revenue", paste(params, sep = "", 
                                               collapse = " + "), 
                              sep = " ~ ")), data_200405)
new_data <- data.frame(data_200607[, params])
names(new_data) <- params
model3_predictions <- predict(model3, new_data, interval = "prediction")
matplot(data_200607[order(data_200607$Planned.revenue) , c("Planned.revenue")], 
        cbind(model3_predictions[order(data_200607$Planned.revenue), ], 
              sort(data_200607$Revenue)), lty = c(2,3,3,1), type = "l", 
        xlab = "Planned Revenue in 2006 and 2007", 
        ylab = "Revenue in 2006 and 2007 (observed and predicted)")
RMSE <- sqrt(sum((model3_predictions[, 1] - data_200607$Revenue)^2) / 
               dim(data_200607)[1])
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
