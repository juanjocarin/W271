library(forecast)
library(gtools)
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scatterplot3d)

# hw08 <- arima.sim(n=500, list(ar = c(.8, -.6, 0, .2), 
#                                 ma = c(.2, .1, .4, 0.3)))
hw08 <- read.csv('hw08_series.csv')
hw08 <- hw08[, -1]

max_coef <- 12
orders <- data.frame(permutations(n = max_coef + 1, r = 2, v = 0:max_coef, 
                                  set = FALSE, repeats.allowed = TRUE))
dim(orders)[1] # Number of models up to max_coef
colnames(orders) <- c("p", "q")
orders <- orders %>% filter(p + q <= max_coef & p + q > 0)
dim(orders)[1] # Number of models considered
orders %>% sample_n(10) # A 10-sample of the possible orders

aic_list <- orders %>% rowwise() %>% 
  mutate(aic = try_default(AIC(Arima(hw08, order = c(p, 0, q))), NA, TRUE))
aic_list <- aic_list %>% filter(!is.na(aic))
dim(aic_list)[1] # Number of models estimated

# # # # Another way # # # #
# aic_list2 <- orders %>% mutate(aic = mapply(function(p, q) 
#   try_default(AIC(Arima(hw08, order = c(p, 0, q))), NA, TRUE), p, q))
# aic_list2 <- aic_list2 %>% filter(!is.na(aic))
# # # # Yet another way # # # #
# aic_list3 <- matrix(rep(NA, (max_coef + 1)^2), ncol = max_coef + 1)
# for (i in 0:max_coef) 
#   for (j in 0:max_coef) 
#     if (i + j > 0 & i + j <= max_coef) 
#       aic_list3[i + 1, j + 1] <- try_default(AIC(Arima(hw08, 
#                                                        order = c(i, 0, j))), 
#                                              NA, TRUE)
# aic_list3 <- data.frame(aic_list3, row.names = 0:max_coef)
# colnames(aic_list3) <- 0:max_coef
# aic_list3 <- aic_list3 %>% gather() %>% rename(q = key,  aic = value) %>% 
#   mutate(p = rep(0:max_coef, max_coef + 1)) %>% select(p, q, aic) %>% 
#   arrange(p) %>% filter(!is.na(aic))
# # # # And yet another way # # # #
# i <- rep(0:max_coef, times = max_coef + 1)
# j <- rep(0:max_coef, each = max_coef + 1)
# aic_list4 <- mapply(function(p, q) ifelse(p + q > 0 & p + q <= max_coef, 
#                                           try_default(AIC(Arima(hw08, 
#                                                                 order = c(p, 0, 
#                                                                           q))), 
#                                                       NA, TRUE), NA), i, j)
# aic_list4 <- matrix(aic_list4, nrow = max_coef + 1)
# aic_list4 <- data.frame(aic_list4, row.names = 0:max_coef)
# colnames(aic_list4) <- 0:max_coef
# aic_list4 <- aic_list4 %>% gather %>% rename(q = key,  aic = value) %>% 
#   mutate(p = rep(0:max_coef, max_coef + 1)) %>% select(p, q, aic) %>% 
#   arrange(p) %>% filter(!is.na(aic))
# # # # The 4 methods yield the same result # # # #
# all(aic_list == aic_list2); all(aic_list== aic_list3); all(aic_list ==
#                                                                aic_list4)

# Add the BIC (and the corresponding family)
aic_list <- aic_list %>% 
  mutate(bic = BIC(Arima(hw08, order = c(p, 0, q))), 
         family = ifelse(q == 0, "AR", ifelse(p == 0, "MA", "ARMA")))

par(mfrow = c(1, 2))
boxplot(aic_list$aic ~ aic_list$family, xlab = "Model family", ylab = "AIC", 
        main = "Boxplot of the AIC value per model family")
boxplot(aic_list$bic ~ aic_list$family, xlab = "Model family", ylab = "BIC", 
        main = "Boxplot of the BIC value per model family")
par(mfrow = c(1, 1))

par(mar = c(5, 4, 4, 5) + 0.1)
par(mfrow = c(1, 2))
plot(aic_list %>% filter(family == "AR") %>% select(p, aic), col = "blue", 
     main = "AIC of AR(p) model vs. p", ylab = "AIC", type = "o", lty = 2, 
     pch = 1)
plot(aic_list %>% filter(family == "MA") %>% select(q, aic), col = "blue", 
     main = "AIC of MA(q) model vs. q", ylab = "AIC", type = "o", lty = 2, 
     pch = 1)
par(mar = c(5, 4, 4, 2) + 0.1)
par(mfrow = c(1, 1))

par(mar = c(5, 4, 4, 5) + 0.1)
par(mfrow = c(2, 2))
plot(aic_list %>% filter(family == "AR") %>% select(p, aic), col = "blue", 
     main = "AIC of AR(p) model vs. p", ylab = "AIC", type = "o", lty = 2, 
     pch = 1)
plot(aic_list %>% filter(family == "MA") %>% select(q, aic), col = "blue", 
     main = "AIC of MA(q) model vs. q", ylab = "AIC", type = "o", lty = 2, 
     pch = 1)
plot(aic_list %>% filter(family == "AR") %>% select(p, bic), col = "blue", 
     main = "BIC of AR(p) model vs. p", ylab = "BIC", type = "o", lty = 2, 
     pch = 1)
plot(aic_list %>% filter(family == "MA") %>% select(q, bic), col = "blue", 
     main = "BIC of MA(q) model vs. q", ylab = "BIC", type = "o", lty = 2, 
     pch = 1)
par(mar = c(5, 4, 4, 2) + 0.1)
par(mfrow = c(1, 1))

aic_plot <- data.frame(aic_list %>% select(p, q, aic))
scatterplot3d(aic_plot, pch = 4, lwd = 3, type = "h", 
              color = grey(dim(aic_list)[1]:1 / 
                             (ceiling(dim(aic_plot)[1] / 15) * 15)), 
              label.tick.marks = TRUE, lty.axis = 2, angle = 60, 
              main = "AIC of the models depending on the order (p and q)")

aic_plot <- data.frame(aic_list %>% filter(family == "ARMA") %>% 
                         select(p, q, aic))
scatterplot3d(aic_plot, pch = 4, lwd = 3, type = "h", 
              color = grey(dim(aic_plot)[1]:1 / 
                             (ceiling(dim(aic_plot)[1] / 15) * 15)), 
              label.tick.marks = TRUE, lty.axis = 2, angle = 75, 
              main = "AIC of the ARMA models depending on the order (p and q)")

kable(aic_list %>% arrange(aic) %>% group_by(family) %>% top_n(-4, aic))
kable(aic_list %>% arrange(bic) %>% group_by(family) %>% top_n(-4, aic))
aic_list %>% arrange(bic) %>% group_by(family) %>% top_n(-4, aic) == aic_list %>% arrange(aic) %>% group_by(family) %>% top_n(-4, aic)
