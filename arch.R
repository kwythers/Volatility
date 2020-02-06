##### Measuring Volatility Using the ARCH Model #####

##### auto-regressive conditional heteroskedasticity

require(tidyverse)
require(tidyquant)
require(timetk)
require(sweep)
require(forecast)
library(ggplot2)
library(grid)
library(gridExtra)
library(rnoaa)
library(kableExtra)
library(knitr)
library(dynlm)
library(FinTS)
library(fGarch)
library(mice)
theme_set(theme_bw())

startDate = as.Date("2004-12-01") #Specify period of time we are interested in
endDate = as.Date("2019-09-30")


# get producer price index by commodity monthly for corn, soybeans, wheat, rice
corn <- tq_get("WPU012202",
                        get  = "economic.data",
                        from = startDate,
                        to   = endDate)

soybeans <- tq_get("WPU01830131",
               get  = "economic.data",
               from = startDate,
               to   = endDate)

wheat <- tq_get("WPU0121",
               get  = "economic.data",
               from = startDate,
               to   = endDate)

rice <- tq_get("WPU01230103",
               get  = "economic.data",
               from = startDate,
               to   = endDate)

# plot data
corn_plot <- corn %>% ggplot(aes(y = price, x = date)) + geom_line(col = 'blue') + labs(ylab = 'return', xlab = 'Time', title = 'Producer Price Index - Corn')
rice_plot <- rice %>% ggplot(aes(y = price, x = date)) + geom_line(col = 'blue') + labs(ylab = 'return', xlab = 'Time', title = 'Producer Price Index - Rice')
soybeans_plot <- soybeans %>% ggplot(aes(y = price, x = date)) + geom_line(col = 'blue') + labs(ylab = 'return', xlab = 'Time', title = 'Producer Price Index - Soybeans')
wheat_plot <- wheat %>% ggplot(aes(y = price, x = date)) + geom_line(col = 'blue') + labs(ylab = 'return', xlab = 'Time', title = 'Producer Price Index - Wheat')

grid.arrange(corn_plot, rice_plot, soybeans_plot, wheat_plot, ncol = 2 )

# coerce to ts
corn_ts <- tk_ts(corn, start = 2004, freq = 12, silent = TRUE)
rice_ts <- tk_ts(rice, start = 2004, freq = 12, silent = TRUE)
soybeans_ts <- tk_ts(soybeans, start = 2004, freq = 12, silent = TRUE)
wheat_ts <- tk_ts(wheat, start = 2004, freq = 12, silent = TRUE)

##### get a daily climate data to test with ARCH
# msp_tmax <- ncdc(datasetid='GHCND', stationid='GHCND:USW00014922', datatypeid='TMAX', startdate = '2018-01-01', enddate = '2018-12-29', limit=500, token = "SfMEFpTfDeJQpTfPXCzmUSvIsXOdCUga")
# 
# msp_tmax <- as_tibble(msp_tmax$data)
# msp_tmax <- msp_tmax %>% 
#   select(date, value)
# msp_tmax_ts <- tk_ts(msp_tmax)

##### daily data for diesel prices
diesel <- tq_get("DDFUELUSGULF",
               get  = "economic.data",
               from = startDate,
               to   = endDate)

diesel$price <- ts(diesel$price)
diesel$date <- seq.Date(as.Date('2006-01-01'), by = 'day', length.out = length(diesel$date))
ggplot(diesel, aes(y = price, x = date )) + geom_line(col = 'red') +
  labs(title = 'Diesel Daily Price', ylab = 'return')

summary(diesel)

# impute missing data with mice
# imputed <- mice(diese$price, m=5, maxit = 50, method = 'pmm', seed = 500)

# Step 1: Estimate mean equation r = beta + error
diesel_mean <- dynlm(price ~ 1, data = diesel)

# Step 2: Retrieve the residuals from the former model and square them
ehatsq <- ts(resid(diesel_mean)^2)

# Step 3: regress squared residuals on one-lagged squared residuals
diesel_arch <- dynlm(ehatsq ~ L(ehatsq), data = ehatsq)
summary(diesel_arch)

##### check for ARCH effects by using the ArchTest() function from the FinTS package - use a significance level 
##### of α=0.05 for our null hypothesis test
diesel_archTest <- ArchTest(diesel$price, lags = 1, demean = TRUE)
diesel_archTest ##### the p-value is < 0.05, we reject the null hypothesis and conclude the presence of ARCH(1) effects

##### Estimating ARCH Models #####
##### estimate an ARCH(1) model using the garchFit() function from the fGarch package in R - specifically, we need 
##### to estimate the variance given in equation (1.2c) - ARCH models are estiamted using the maximum likelihood method

# replace na with 2, simple remove missing values
diesel <- diesel %>% replace_na(list(price = 2))
summary(diesel)
arch_fit <- garchFit(~ garch(1, 0), data = diesel$price, trace = FALSE)
summary(arch_fit)

# plot the conditional variance 
diesel$ht <- arch_fit@h.t
ggplot(diesel, aes(y = ht, x = date)) + geom_line(col = '#ff9933') + ylab('Conditional Variance') + xlab('Date')







