# libraries:
library(forecast)
library(dplyr)
library(lubridate)
library(broom)
library(daymetr)
# consistent results from random number generator
set.seed(103)

days <- 0:9
mass <- vector(length = 10)
for(t in seq_along(days)){
  mass[t] <- 1 + 2 * days[t] + rnorm(1, 0, 1)
}

# fit a linear model
linear_data <- data.frame(day = days, mass = mass)
plot(mass~day,
     data = linear_data,
     ylab = 'mass (g)',
     ylim = c(1, 20),
     xlim = c(0, 15))

mod_linear <- lm(mass ~ 1 + days, data = linear_data)

# print the fitted model
tidy(mod_linear)

summary(mod_linear)

# check that the assumptions of the model are being met
plot(mod_linear)

# we can simulate this process many times
n_sims <- 1000
sim_data <- list()
fits <- list()
days <- 0:9
n <- length(days)


for(i in 1:n_sims){
  sim_data[[i]] <- vector(mode = 'double', length = n)
  for(t in seq_along(days)){
    mass[t] <- 1 + 2 * days[t] + rnorm(1, 0, 1)
  }
  sim_data[[i]] <- mass
  fits[[i]] <- lm(mass ~ 1 + days)
}

v <- vector(length = n_sims)
stats <- data.frame(intercept = v, slope = v, sigma = v, p_i = v, p_s = v)
for(i in 1:n_sims){
  stats[i, c('intercept', 'slope')] <- coef(fits[[i]])
  stats[i, 'sigma']                 <- sigma(fits[[i]])
  stats[i, c('p_i', 'p_s')]         <- summary(fits[[i]])$coefficients[1:2, 4]
}

data.frame(slope = c(mean(stats$slope), sd(stats$slope)),
           intercept = c(mean(stats$intercept), sd(stats$intercept)),
           row.names = c('mean', 'sd'))

# frequency of significant intercept:
# how many times was it non-significant?
sum(stats$p_i > 0.05)/nrow(stats)

# how many times was it significant?
sum(stats$p_i < 0.05)/nrow(stats)

# frequency of significant slope:
# how many times was the slope non-significant?
sum(stats$p_s > 0.05)/nrow(stats)

# how many times was the slope significant?
sum(stats$p_s < 0.05)/nrow(stats)

# you can also visualize the outcomes from these simulations in a histogram
hist(stats$slope)

hist(stats$intercept)

# checking the class of the statistical object
class(mod_linear)

# the `predict()` function
newdays <- 11:15
newdat <- data.frame(days = newdays)
preds <- predict(mod_linear, newdata = newdat)

## approx. equivalent to:
# preds <- 1 + 2 * newdays

# this gives us the ability to predict future growth using the fitted model
plot(mass ~ day,
     data = linear_data,
     ylab = 'mass (g)',
     ylim = c(0, 30),
     xlim = c(0, 15)) +
  points(newdays, preds, col = 'red') +
  abline(coef(mod_linear)) +
  abline(confint(mod_linear)[,1], lty = 2) +
  abline(confint(mod_linear)[,2], lty = 2)

# time series objects in R
set.seed(210)
months <- 1:240

# 1. just noise
noise <- rnorm(length(months))

# 2. lag of one step
lag   <- vector()
for(t in 1:length(months)){
  if(t == 1){
    lag[t]  <- rnorm(1)
  } else {
    lag[t]  <- (lag[t-1] + noise[t]) / 2
  }
}

# 3. lag of one step + trend, slope of 1/48 per month
lag_trend <- lag + months / 48

# 4. seasonal patterns
seasonal <- 2*sin(2*pi*months/12) + noise

# 5. seasonal patterns + trend, slope of 1/48 per month
seasonal_trend <- seasonal + months / 48

all <- ts(data = data.frame(noise, lag, lag_trend, seasonal, seasonal_trend),
          start = 1982,
          end = 2001,
          frequency = 12)

plot(all)

# visualizing lag
lag.plot(all, set.lags = c(1, 3, 6, 9))

# visualizing auto-correlation
acf(all[,'noise'], xlab = 'Lag (years), noise')

acf(all[,'lag'], xlab = 'Lag (years), lag 1')

acf(all[,'lag'], xlab = 'Lag (years), lag 1 and trend')

acf(all[,'seasonal'], xlab = 'Lag (years), seasonal')

# decompose
dec <- decompose(all[,'seasonal_trend'])

plot(dec)
