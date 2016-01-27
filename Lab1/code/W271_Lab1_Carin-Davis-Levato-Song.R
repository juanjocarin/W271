## MIDS W271-4 Lab1           ##
## Carin, Davis, Levato, Song ##



# LIBRARIES AND CONSTANTS -------------------------------------------------
## @knitr Libraries
#### LOAD LIBRARIES AND DEFINE CONSTANTS USED IN MULTIPLE CHUNKS
library(knitr)
library(plot3D)
num_sim <- 100e3 # number of simulations



# QUESTION 3 --------------------------------------------------------------

## @knitr Question3-part2
#### PLOT CDF F_X(X)
## Define the CDF of X
F <- function(x) {
    ifelse(x<0, 0, 
           ifelse(x < 3, 1/4, 
                  ifelse(x < 4, 1/2, 
                         ifelse(x < 2*sqrt(7), x^2/32, 
                                ifelse(x < 30, 1-1/8, 1)))))
    }
## x-axis
x=seq(-5, 35, by = 0.01)
## Plot
plot(x, F(x), type='l', xaxt='n', yaxt='n')
axis(side = 2, at = seq(0, 1, .25))
axis(side = 1, at = seq(0, 30, 5))
## Alternative plot
# library(ggplot2)
# ggplot(data.frame(x), aes(x)) + stat_function(fun=F, geom = 'step')



# QUESTION 6 ----------------------------------------------------------

## @knitr Question6-part1-1st
#### PLOT Z=f(X,Y) (2 WAYS)
## x- and y-axes
x <- y <- seq(0, 1, by = 0.01)
## Build 2-D region
grid <- mesh(x, y)
## Define Z (= X +Y)
z <- with(grid, x + y)
par(mfrow = c(1, 2))
## 1st plot: contour lines
contour(x, y, z, asp=1)
## 2nd plot: 3-D
persp3D(z = z, x = x, y = y, theta = 0)

## @knitr Question6-part1-2nd
#### PLOT REGION OF THE X-Y PLANE WHERE Z < z (FOR A GIVEN z)
## Create (X,Y) points
X <- runif(num_sim, min=0, max=1)
Y <- runif(num_sim, min=0, max=1)
## Define Z
Z <- X+Y
## Specify z
z = 0.8
## Plot X-Y region in which Z < z
plot(X[Z<z], Y[Z<z], col='red', asp = 1)
## Delimit the region
polygon(x=c(0,z,0), y=c(0,0,z))
## Alternate way to delimit the region
# abline(a=z, b=-1); abline(h=0); abline(v=0)


## @knitr Question6-part2
#### PLOT HISTOGRAM AND PDF OF Z
par(mfrow = c(1, 2))
## Histogram
hist(Z, freq = FALSE)
## PDF
plot(density(Z), main = 'Density distribution of Z')
## Both show triangular form



# QUESTION 7 --------------------------------------------------------------

## @knitr Question7-part1
#### CREATE A TABLE WITH ALL POSSIBLE ROLLS
games <- sapply(seq(1,6), function(x) x+seq(1,6))
colnames(games) <- sapply(seq(1,6), function(x) paste("**Die 1=", x, '**', 
                                                      sep=""))
rownames(games) <- sapply(seq(1,6), function(x) paste("**Die 2=", x, '**', 
                                                      sep=""))
kable(games, align = 'c', caption = 'Possible games')
## Alternative way
# library(pander)
# panderOptions('table.split.table', Inf)
# set.caption("Possible games")
# pander(games, style = 'rmarkdown')


## @knitr Question7-parts1and2
#### SIMULATE num_sim GAMES TO CHECK THE VALUES OF E[Y|Z=1], E[Y|Z=0] AND 
    #### E[PAYOFF]
Y <- rep(0, num_sim) # value of Y (number of rolls for every simulation)
Z <- rep(0, num_sim) # value of Z (0: house wins; 1: I win)
P <- rep(0, num_sim) # payoff
for (i in 1:num_sim) {
    y <- 0 # initialize number of rolls
    x <- sample(seq(6), 1) + sample(seq(6), 1) # simulate two dice
    y <- y+1 # update number of rolls
    first_x <- x # store x (will be used if it's 4, 5, 6, 8, 9, 10)
    # House wins if 2, 3, 12
    # I win if 7, 11
    # Otherwise, keep that value of x (and insert in z)
    z <- ifelse(x==2 | x==3 | x==12, 0, 
                ifelse(x==7 | x==11, 1, first_x))
    p <- ifelse(z==1, 100, 0) # if I win 1st time, payoff is 100
    # While z > 1 (i.e., if nobody wins)
    j <- 2 # keep count of rolls
    while (z>1) {
        x <- sample(seq(6), 1) + sample(seq(6), 1) # simulate other roll
        y <- y+1 # update number of rolls
        # House wins if 7
        # I win if same value of 1st roll
        z <- ifelse(x==7, 0, ifelse(x==first_x , 1, first_x))
        # Compute payoff if I win (with less than 5 rolls)
        p <- ifelse(z==1 & j<5, 100-20*(j-1), 0)
        j <- j+1 # update count of rolls
    }
    # When finished, store values for that simulation
    Y[i] <- y
    Z[i] <- z
    P[i] <- p
}
mean(Z) # E[Z] (=Pr(Z=1) since Z is binary); should be close to 0.4929 (244/495)
mean(Y[Z==0]) # E[Y|Z]; should be close to 3.8010
mean(Y[Z==1]) # E[Y|not Z]; should be close to 2.9383
mean(P) # E[P]; should be close to 33.2694



# QUESTION 9 --------------------------------------------------------------

## @knitr Question9-part2
#### PLOT MAXIMUM VALUE OF n AS FUNCTION OF k for Var(W1) TO BE LOWER THAN 
    #### Var(W2) (ONLY IF k > 0.5 AND n>=2)
## Define possible values of k
k <- seq(0.001, .999, .001)
## Define maximum value of n as a function of k
n <- floor(1/(1-k))
## Plot (condition n>=2 forces k > 0.5)
plot(k[n>=2], n[n>=2])



# QUESTION 12 -------------------------------------------------------------

## @knitr Question12-partiii
#### CALCULATE t STATISTIC AND p VALUE FOR H0 VS H1
## Sample size, estimate of y and sd
n <- 900; y <- -32.8; s <- 466.4 
## t statistic
(t <- y*sqrt(n)/s)
## p-value using t distribution
(p <- pt(t, n-1))
## p-value using standardad normal distribution (Z)
(pnorm(t))



# QUESTION 13 -------------------------------------------------------------

## @knitr Question13-partiii
#### TEST H0: THETA = .5 against H1: THETA < .5 FOR MARK PRICE
n <- 429 # sample size
theta <- 188/n # theta
theta_0 <- 0.5 # theta_0 (null hypothesis)
(se <- sqrt(theta*(1-theta)/n)) # standard error
(t <- (theta-theta_0)/se) # t statistic
(p <- pt(t, n-1)) ## p-value


## @knitr Question13-part4
#### PROBABILITY OF TYPE II ERROR
true_theta <- 0.45 # true population proportion
sig <- 0.01 # significance level
(t_critical <- qt(sig, n-1)) # critical value of t
(theta_critical <- t_critical*se + theta_0) # critical value of theta
(t <- (true_theta-theta_critical)/se) # t statistic for a type II error
(beta <- pt(t, n-1))*100 # beta = type II error rate
