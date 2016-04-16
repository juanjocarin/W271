##setwd('.\\MIDS\\Semester3\\W271\\Homework'1)
load("birthweight_w271.rdata")

#Question 1
str(data)
desc
summary(data)

#Question 3
attach(data)
require(ggplot2)
summary(bwght)
qnt <-  c(.01, .05, .1, .25, .5, .75, .9, .95, .99)
quantile(bwght, probs = qnt)
# Regular bin wdith
p1 <- ggplot(data, aes(bwght)) + geom_histogram(binwidth = 5) + 
  ggtitle('Birthweight in Ounces, binwidth = 5')
p1 +theme(plot.title = element_text(size=20, face="bold", 
                                    margin = margin(10, 0, 10, 0)))
#Narrow bin width
p2 <- ggplot(data, aes(bwght)) + geom_histogram(binwidth = 1) + 
  ggtitle('Birthweight in Ounces, binwidth = 1')
p2 +theme(plot.title = element_text(size=20, face="bold", 
                                    margin = margin(10, 0, 10, 0)))
#Wide bin width
p3 <- ggplot(data, aes(bwght)) + geom_histogram(binwidth = 15) + 
  ggtitle('Birthweight in Ounces, binwidth = 15')
p3 +theme(plot.title = element_text(size=20, face="bold", 
                                    margin = margin(10, 0, 10, 0)))

outliers <- data[data$bwght < 10 | data$bwght > 200,]
outliers[,c("bwght", "lbwght", "bwghtlbs")]

#Question 4
summary(cigs)
quantile(cigs, qnt)
c1 <- ggplot(data, aes(cigs)) + geom_histogram(binwidth = 5) + 
  ggtitle('Daily Cigarettes Smoked While Pregnant, binwidth = 5')
c1 +theme(plot.title = element_text(size=14, face="bold", 
                                    margin = margin(10, 0, 10, 0)))
c2 <- ggplot(data, aes(cigs)) + geom_histogram(binwidth = 1) + 
  ggtitle('Daily Cigarettes Smoked While Pregnant, binwidth = 1')
c2 +theme(plot.title = element_text(size=14, face="bold", 
                                    margin = margin(10, 0, 10, 0)))
c3 <- ggplot(data, aes(cigs)) + geom_histogram(binwidth = 15) + 
  ggtitle('Daily Cigarettes Smoked While Pregnant, binwidth = 15')
c3 +theme(plot.title = element_text(size=14, face="bold", 
                                    margin = margin(10, 0, 10, 0)))
c4 <- ggplot(data[!cigs==0,], aes(cigs[!cigs==0])) + geom_histogram(binwidth= 1) + 
  ggtitle('Daily Cigarettes Smoked While Pregnant, Smokers Only')
c4 + theme(plot.title = element_text(size=14, face="bold", 
                                     margin = margin(10, 0, 10, 0)))
c5 <- ggplot(data[!cigs==0,], aes(log(cigs[!cigs==0] + 1))) + geom_histogram(binwidth= 0.5) + 
  ggtitle('Log of Daily Cigarettes Smoked While Pregnant, Smokers Only')
c5 + theme(plot.title = element_text(size=14, face="bold", 
                                     margin = margin(10, 0, 10, 0)))
#Hartigan's dip test for multimodality
require(diptest)
dip.test(log(cigs[!cigs==0] + 1))

#Question 5
s <- ggplot(data, aes(cigs, bwght)) + geom_point() +
  ggtitle('Scatterplot of Birght Weight and \n Cigarettes Smoked During Pregnancy') + 
  theme(plot.title = element_text(size=14, face="bold", margin = margin(10, 0, 10, 0)))
s

#Question 6
# Exclude any data where there is no observation for cigs or bwght
data = data[complete.cases(data$bwght, data$cigs),]
# Exclude the upper outlier for bwght
data = data[data$bwght < 200, ]
m <- lm(data$bwght~data$cigs)
summary.lm(m)

#Question 7
summary(faminc)
quantile(faminc, qnt)
h <- ggplot(data, aes(faminc)) + geom_histogram(binwidth= 5) +
  ggtitle('Family Income') + 
  theme(plot.title = element_text(size=14, face="bold", margin = margin(10, 0, 10, 0)))
h
require(car)
scatterplotMatrix(data[, c("bwght", "cigs", "faminc")], smoother = F)

#Question 8
data = data[complete.cases(data$bwght, data$cigs, data$faminc),]
m2 <- lm(bwght~cigs + faminc, data = data)
summary.lm(m2)