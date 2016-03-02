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
library(zoo)
library(grid)


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

######### Function below to show multiple plots on a grid from  source 
#http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
###########

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
#load the data
d<-read.csv("WageData2.csv")
summary(d)
head(d)

## @knitr Question4-1-2
#create the historgrams for the variables for analysis
p1<-ggplot(d, aes(x=wage)) + geom_histogram(binwidth=100)
p2<-ggplot(d, aes(x=education)) + geom_histogram(binwidth=2)
p3<-ggplot(d, aes(x=experience)) + geom_histogram(binwidth=2)
p4<-ggplot(d, aes(x=age)) + geom_histogram(binwidth=1)
p5<-ggplot(d, aes(x=raceColor)) + geom_histogram(binwidth=.5)
p6<-ggplot(d, aes(x=dad_education)) + geom_histogram(binwidth=1)
p7<-ggplot(d, aes(x=mom_education)) + geom_histogram(binwidth=1)
p8<-ggplot(d, aes(x=rural)) + geom_histogram(binwidth=.5)
p9<-ggplot(d, aes(x=city)) + geom_histogram(binwidth=.5)
multiplot(p1, p2, p3, p4, p5, p6, p7, p8, p9, cols=3)

## @knitr Question4-1-3
#create the new variables
d$logWage<-log(d$wage)
d$experienceSquare<-d$experience^2


## @knitr Question4-2-1
p2.1<-ggplot(d, aes(wage, education)) + 
  geom_point() +
  labs(x = "Wage", 
       y = "Education") + 
  geom_smooth(method = "lm")
p2.2<-ggplot(d, aes(log(wage), education)) + 
  geom_point() +
  labs(x = "Log(Wage)", 
       y = "Education") + 
  geom_smooth(method = "lm")
p2.3<-ggplot(d, aes(wage, experience)) + 
  geom_point() +
  labs(x = "Wage", 
       y = "Experience") + 
  geom_smooth(method = "lm")
p2.4<-ggplot(d, aes(log(wage), experience)) + 
  geom_point() +
  labs(x = "Log(Wage)", 
       y = "Experience") + 
  geom_smooth(method = "lm")
p2.5<-ggplot(d, aes(wage, age)) + 
  geom_point() +
  labs(x = "Wage", 
       y = "Age") + 
  geom_smooth(method = "lm")
p2.6<-ggplot(d, aes(log(wage), age)) + 
  geom_point() +
  labs(x = "Log(Wage)", 
       y = "Age") + 
  geom_smooth(method = "lm")
p2.7<-ggplot(d, aes(wage, raceColor)) + 
  geom_point() +
  labs(x = "Wage", 
       y = "Race") + 
  geom_smooth(method = "lm")
p2.8<-ggplot(d, aes(log(wage), raceColor)) + 
  geom_point() +
  labs(x = "Log(Wage)", 
       y = "Race") + 
  geom_smooth(method = "lm")
p2.9<-ggplot(d, aes(wage, dad_education)) + 
  geom_point(na.rm = T) +
  labs(x = "Wage", 
       y = "Father's Education") + 
  geom_smooth(method = "lm", na.rm = T)
p2.10<-ggplot(d, aes(log(wage), dad_education)) + 
  geom_point(na.rm = T) +
  labs(x = "Log(Wage)", 
       y = "Father's Education") + 
  geom_smooth(method = "lm", na.rm = T)
p2.11<-ggplot(d, aes(wage, mom_education)) + 
  geom_point(na.rm = T) +
  labs(x = "Wage", 
       y = "Mother's Education") + 
  geom_smooth(method = "lm", na.rm = T)
p2.12<-ggplot(d, aes(log(wage), mom_education)) + 
  geom_point(na.rm = T) +
  labs(x = "Log(Wage)", 
       y = "Mother's Education") + 
  geom_smooth(method = "lm", na.rm = T)
p2.13<-ggplot(d, aes(wage, rural)) + 
  geom_point(na.rm = T) +
  labs(x = "Wage", 
       y = "Location - Rural") + 
  geom_smooth(method = "lm", na.rm = T)
p2.14<-ggplot(d, aes(log(wage), rural)) + 
  geom_point(na.rm = T) +
  labs(x = "Log(Wage)", 
       y = "Location - Rural") + 
  geom_smooth(method = "lm", na.rm = T)
p2.15<-ggplot(d, aes(wage, city)) + 
  geom_point(na.rm = T) +
  labs(x = "Wage", 
       y = "Location - City") + 
  geom_smooth(method = "lm", na.rm = T)
p2.16<-ggplot(d, aes(log(wage), city)) + 
  geom_point(na.rm = T) +
  labs(x = "Log(Wage)", 
       y = "Location - City") + 
  geom_smooth(method = "lm", na.rm = T)
p2.17<-ggplot(d, aes(wage, IQscore)) + 
  geom_point(na.rm = T) +
  labs(x = "Wage", 
       y = "IQ") + 
  geom_smooth(method = "lm", na.rm = T)
p2.18<-ggplot(d, aes(log(wage), IQscore)) + 
  geom_point(na.rm = T) +
  labs(x = "Log(Wage)", 
       y = "IQ") + 
  geom_smooth(method = "lm", na.rm = T)

## @knitr Question4-2-2
multiplot(p2.1, p2.2, p2.3, p2.4, cols=2)
multiplot(p2.5, p2.6, p2.7, p2.8, cols=2)
multiplot(p2.9,p2.10, p2.11, p2.12, cols=2)
multiplot(p2.13, p2.14, p2.15, p2.16, cols=2)
multiplot(p2.17, p2.18, cols=2)

## @knitr Question4-3
model4.3<-lm(logWage~education + experience + age + raceColor, d)
stargazer(model4.3, type="latex", title="Question 4.3-1")

## @knitr Questions4-4
model4.4<-lm(logWage~education + experience + experienceSquare + raceColor, data=d)
stargazer(model4.3, model4.4, type="latex", title="Question 4.4")

## @knitr Questions4-4-1
#pull out the coefficients from the model
coefs<-coef(model4.4)
#set x values to be the experence data
x<-d$experience
#set y to be just the effect from experience
#so we pull the ceofficient for expereience and for experienceSquared
y<-coefs[3]*x+coefs[4]*x^2
#put the data in a new dataframe to use for plotting
dat<-data.frame(x,y)
#plot the estimated effect of expereience (with the squared term) on the log(wage)
ggplot(dat, aes(x,y))+
  geom_smooth(na.rm=T) +
  labs(x="experience", y="log(wage)"))


## @knitr Questions4-5
model4.5<-lm(logWage~education + experience + experienceSquare + raceColor +
               dad_education + mom_education + rural + city, data=d)
stargazer(model4.3, model4.4, model4.5, type="latex", title="Question 4.5")

## @knitr Questions4-5-1-1
#check which variables have the missing data
sum(is.na(d$education))
sum(is.na(d$experience))
sum(is.na(d$experienceSquare))
sum(is.na(d$raceColor))
sum(is.na(d$dad_education))
sum(is.na(d$mom_education))
sum(is.na(d$rural))
sum(is.na(d$city))

## @knitr Questions4-5-1-2
#identify rows that have an NA value
row.has.na <- apply(d, 1, function(x){any(is.na(x))})
#make a dataframe of only those rows with missing values to look at
d_missing<-d[row.has.na,]


## @knitr Questions4-5-1-3
#create plots to try and identify any patterns
p4.1<-ggplot(d_missing, aes(wage, education)) + 
  geom_point() +
  labs(x = "Wage", 
       y = "Education") + 
  geom_smooth(method = "lm")
p4.2<-ggplot(d_missing, aes(log(wage), education)) + 
  geom_point() +
  labs(x = "Log(Wage)", 
       y = "Education") + 
  geom_smooth(method = "lm")
p4.3<-ggplot(d_missing, aes(wage, experience)) + 
  geom_point() +
  labs(x = "Wage", 
       y = "Experience") + 
  geom_smooth(method = "lm")
p4.4<-ggplot(d_missing, aes(log(wage), experience)) + 
  geom_point() +
  labs(x = "Log(Wage)", 
       y = "Experience") + 
  geom_smooth(method = "lm")
p4.5<-ggplot(d_missing, aes(wage, raceColor)) + 
  geom_point() +
  labs(x = "Wage", 
       y = "Experience") + 
  geom_smooth(method = "lm")
p4.6<-ggplot(d_missing, aes(log(wage), raceColor)) + 
  geom_point() +
  labs(x = "Log(Wage)", 
       y = "raceColor") + 
  geom_smooth(method = "lm")
#create historgrams to try and idenfiy patterns
p4.7<-ggplot(d_missing, aes(x=education)) + geom_histogram(binwidth=2)
p4.8<-ggplot(d_missing, aes(x=experience)) + geom_histogram(binwidth=2)
p4.9<-ggplot(d_missing, aes(x=raceColor)) + geom_histogram(binwidth=.5)
#show the plots
multiplot(p4.7, p4.8, p4.9)
multiplot(p4.1, p4.2, p4.3, p4.4, p4.5, p4.6, cols=2)



## @knitr Questions4-5-3
#create a copy of the dataset for this problem
d4.3<-d
#replace the missing values with the means of those values
d4.3$dad_education<-na.fill(d4.3$dad_education, mean(d4.3$dad_education, na.rm=T))
d4.3$mom_education<-na.fill(d4.3$mom_education, mean(d4.3$mom_education, na.rm=T))
#re-run the regression
model4.5.3<-lm(logWage~education + experience + experienceSquare + raceColor +
                 dad_education + mom_education + rural + city, data=d4.3)
stargazer(model4.5, model4.5.3, type="latex", title="Question 4.5-3")

## @knitr Questions4-5-4-1
#copy data to replace values for the part of the question
d4<-d
#regress dad_education and mom_education on selected variables
model_dad<-lm(dad_education~education+experience+raceColor, data=d4)
model_mom<-lm(mom_education~education+experience+raceColor, data=d4)
#pull out the coefficients
coef_dad<-coef(model_dad)
coef_mom<-coef(model_mom)

## @knitr Questions4-5-4-2
#create a function
rep<-function(d) {
  for (i in 1:nrow(d)) {
    if (is.na(d$dad_education[i])==TRUE) {
      d$dad_education[i]= coef_dad[1]+coef_dad[2]*d$education[i]+
        coef_dad[3]*d$experience[i]+coef_dad[4]*d$raceColor[i]
    }
    if (is.na(d$mom_education[i])==TRUE) {
      d$mom_education[i]= coef_mom[1]+coef_mom[2]*d$education[i]+
        coef_mom[3]*d$experience[i]+coef_mom[4]*d$raceColor[i]
    }
  }
}
#call the function
rep(d4)

## @knitr Questions4-5-4-3
#re-run the origional regression
model4.5.4<-lm(logWage~education + experience + experienceSquare + raceColor +
                 dad_education + mom_education + rural + city, data=d4)
#show the results of the the origional regression the final regression and the two sub regressions
stargazer(model4.5, model4.5.4, model_dad, model_mom, type="latex", title="Question 4.5-4")

## @knitr Questions4-5-5
stargazer(model4.5, model4.5.3, model4.5.4, type="latex", title="Question 4.5-5")


## @knitr Question5
# QUESTION 5 --------------------------------------------------------------

## @knitr Questions5-1-1
data<-read.csv('wealthy_candidates.csv')
summary(data)
head(data)
#look at the variables
hist(data$voteshare)
hist(data$absolute_wealth)

## @knitr Questions5-1-2
#subset the data and only keep the complete cases 
data2<-data[, c("voteshare", "absolute_wealth", "region")]
data2<-data2[complete.cases(data),]
data2$logwealth<-log(data2$absolute_wealth)
min(data2$logwealth)
hist(data2$logwealth, breaks=20)
#eliminate the large column on near 0 absoluate wealth
data2<-subset(data2, logwealth>1, )

## @knitr Questions5-1-3
#plot the data to get a sense if there might be a linear relationship
scatter1<-ggplot(data2, aes(voteshare, logwealth))
scatter1+geom_point()

## @knitr Questions5-1-4
#run the basic model
model1<-lm(voteshare~logwealth, data=data2)
stargazer(model1, type="latex", title="Question 5.1")

## @knitr Questions5-2
#run the model with the squared term added in
model2<-lm(voteshare~logwealth+ logwealth*logwealth, data=data2)
stargazer(model1, model2, type="latex", title="Question 5.2")

## @knitr Questions5-3
model3<-lm(voteshare~logwealth+factor(region), data=data2)
stargazer(model1, model2, model3, type="latex", omit="region", 
          add.lines = list(c("Region Fixed effects", "No", "No", "Yes")), title="Question 5.3")

## @knitr Question
# QUESTION 6 --------------------------------------------------------------

