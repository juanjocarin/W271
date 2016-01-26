##  W271-4 Lab1                 ##
##  Carin, Davis, Levato, Song  ##


## @knitr Libraries
library(knitr)
library(plot3D)



## @knitr Question3-part2
F <- function(x) {
    ifelse(x<0, 0, 
           ifelse(x < 3, 1/4, 
                  ifelse(x < 4, 1/2, 
                         ifelse(x < 2*sqrt(7), x^2/32, 
                                ifelse(x < 30, 1-1/8, 1)))))
}
x=seq(-5, 35, by = 0.01)
plot(x, F(x), type='l', xaxt='n', yaxt='n')
axis(side = 2, at = seq(0, 1, .25))
axis(side = 1, at = seq(0, 30, 5))
# library(ggplot2)
# ggplot(data.frame(x), aes(x)) + stat_function(fun=eq, geom = 'step')



## @knitr Question6-part1-a
x <- y <- seq(0, 1, by = 0.01)
grid <- mesh(x, y)
z <- with(grid, x + y)
par(mfrow = c(1, 2))
contour(x, y, z, asp=1)
persp3D(z = z, x = x, y = y, theta = 0)



## @knitr Question6-part1-b
N <- 100e3 # number of simulations
X <- runif(N, min=0, max=1)
Y <- runif(N, min=0, max=1)
Z <- X+Y

z = 0.8

plot(X[Z<z], Y[Z<z], col='red', asp = 1)
polygon(x=c(0,z,0), y=c(0,0,z))
# abline(a=z, b=-1); abline(h=0); abline(v=0)



## @knitr Question6-part2
par(mfrow = c(1, 2))
hist(Z, freq = FALSE)
plot(density(Z), main = 'Density distribution of Z')



## @knitr Question7-part1-a
games <- sapply(seq(1,6), function(x) x+seq(1,6))
colnames(games) <- sapply(seq(1,6), function(x) paste("**Die 1=", x, '**', sep=""))
rownames(games) <- sapply(seq(1,6), function(x) paste("**Die 2=", x, '**', sep=""))
kable(games, align = 'c', caption = 'Possible games')
# library(pander)
# panderOptions('table.split.table', Inf)
# set.caption("Possible games")
# pander(games, style = 'rmarkdown')



## @knitr Question7-part1-b
N <- 100e3 # number of simulations
Y <- rep(0, N) # value of Y (number of rolls for every simulation)
Z <- rep(0, N) # value of Z (0: house wins; 1: I win)
for (i in 1:N) {
    y <- 0 # initialize number of rolls
    x <- sample(seq(6), 1) + sample(seq(6), 1) # simulate two dice
    y <- y+1 # update number of rolls
    first_x <- x # store x (will be used if it's 4, 5, 6, 8, 9, 10)
    # House wins if 2, 3, 12
    # I win if 7, 11
    # Otherwise, keep that value of x (and insert in z)
    z <- ifelse(x==2 | x==3 | x==12, 0, 
                ifelse(x==7 | x==11, 1, first_x))
    # While z > 1 (i.e., if nobody wins)
    while (z>1) {
        x <- sample(seq(6), 1) + sample(seq(6), 1) # simulate other roll
        y <- y+1 # update number of rolls
        # House wins if 7
        # I win if same value of 1st roll
        z <- ifelse(x==7, 0, ifelse(x==first_x , 1, first_x))
    }
    # When finished, store values for that simulation
    Y[i] <- y
    Z[i] <- z
}

mean(Z) # E[Z] (=Pr(Z=1) since Z is binary); should be close to 0.493 (244/495)

mean(Y[Z==0]) # E[Y|Z]; should be close to 3.8010
mean(Y[Z==1]) # E[Y|not Z]; should be close to 2.9383



## @knitr Question7-part2
N <- 100e3 # number of simulations
Y <- rep(0, N) # value of Y (number of rolls for every simulation)
Z <- rep(0, N) # value of Z (0: house wins; 1: I win)
P <- rep(0, N) # payoff
for (i in 1:N) {
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

mean(P) # E[P]; should be close to 33.2694



## @knitr Question9-part2
k <- seq(0.001, .999, .001)
n <- floor(1/(1-k))
plot(k[n>=2], n[n>=2])



## @knitr Question12-partiii
n <- 900; y <- -32.8; s <- 466.4
(t <- y*sqrt(n)/s)
(p <- pt(t, n-1))
(pnorm(t))



## @knitr Question13-partiii-a
n <- 429
theta <- 188/n
theta_0 <- 0.5
(se <- sqrt(theta*(1-theta)/n))
(t <- (theta-theta_0)/se)



## @knitr Question13-partiii-b
(alpha <- pt(t, n-1))



## @knitr Question13-part4
true_theta <- 0.45
sig <- 0.01
(t_critical <- qt(sig, n-1))
(theta_critical <- t_critical*se + theta_0)
(t <- (true_theta-theta_critical)/se)
(beta <- pt(t, n-1))*100
