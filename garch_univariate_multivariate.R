##### Time-Varying Volatility #####

#install.packages(c("quantmod","rugarch","rmgarch"))   # only needed in case you have not yet installed these packages
library(quantmod)
library(rugarch)
library(rmgarch)

##### the generalized autoregressive conditional heteroskedasticity (ARCH) model concerns time series with 
##### time-varying heteroskedasticity, where variance is conditional on the information existing 
##### at a given point in time

# replace with your directory and uncomment
# setwd('~/R_code/')

# get some data
startDate = as.Date("2007-01-03") #Specify period of time we are interested in
endDate = as.Date("2018-04-30")

getSymbols("IBM", from = startDate, to = endDate)
getSymbols("GOOG", from = startDate, to = endDate)
getSymbols("BP", from = startDate, to = endDate)

# look at one of these dataframes to understand what data these are
head(IBM)
str(IBM)

# fancy looking chart with the following command (also part of the quantmod package) 
chartSeries(GOOG)

# when we are estimating volatility models, we work with returns - there is a function that transforms the data to returns

rIBM <- dailyReturn(IBM) # weeklyReturn is also an option
rBP <- dailyReturn(BP)
rGOOG <- dailyReturn(GOOG)

# put all data into a data frame for use in the multivariate model
rX <- data.frame(rIBM, rBP, rGOOG)
names(rX)[1] <- "rIBM"
names(rX)[2] <- "rBP"
names(rX)[3] <- "rGOOG"

##### Univariate GARCH Model #####
# you need to do is to ensure you know what type of GARCH model you want to estimate and then let R know about this 
# it is the ugarchspec( ) function which is used to let R know about the model type - there is in fact a default 
# specification and the way to invoke this is as follows 
ug_spec <- ugarchspec()

# list which contains all the relevant model specifications. Let's look at them
ug_spec

##### the key issues here are the spec for the Mean Model (here an ARMA(1,1) model) and the specification for the 
##### GARCH Model, here an sGARCH(1,1) which is basically a GARCH(1,1) - to get details on all the possible specifications 
##### and how to change them it is best to consult the documentation of the rugarch package
##### let's say you want to change the mean model from an ARMA(1,1) to an ARMA(1,0), i.e. an AR(1) model
ug_spec <- ugarchspec(mean.model =list(armaOrder = c(1,0)))

##### specification for an example of the EWMA Model 
# ewma_spec <- ugarchspec(variance.model=list(model="iGARCH", garchOrder=c(1,1)), 
#                        mean.model=list(armaOrder=c(0,0), include.mean=TRUE),  
#                        distribution.model="norm", fixed.pars=list(omega=0))

# find the best parameters, i.e. we need to estimate the model - this step is achieved by the ugarchfit function
ugfit <- ugarchfit(spec = ug_spec, data = rIBM)

# list that contains a range of results from the estimation - look at the results 
ugfit

##### ar1 is the AR1 coefficient of the mean model (here very small and basically insignificant), 
##### alpha1 is the coefficient to the squared residuals in the GARCH equation 
##### beta1 is the coefficient to the lagged variance

##### often you will want to use model output for some further analysis - it is therefore important to understand 
##### how to extract information such as the parameter estimates, their standard errors or the residuals - the object 
##### ugfit contains all the information - in that object you can find two drawers (or in technical speak slots, 
##### @fit and @model) - each of these drawers contains a range of different things - what they contain you can 
##### figure out by asking for the element names

paste("Elements in the @model slot")
names(ugfit@model)
paste("Elements in the @fit slot")
names(ugfit@fit)

##### extract the estimated coefficients you would do that in the following way

ugfit@fit$coef

ug_var <- ugfit@fit$var   # save the estimated conditional variances
ug_res2 <- (ugfit@fit$residuals)^2   # save the estimated squared residuals

# plot the squared residuals and the estimated conditional variance
plot(ug_res2, type = "l")
lines(ug_var, col = "green")

# use an estimated model to subsequently forecast the conditional variance - the function used for this purpose is the 
# ugarchforecast function
ugfore <- ugarchforecast(ugfit, n.ahead = 10)
ugfore

##### these are forecasts for the next ten days, both for the expected returns (Series) and for the conditional 
##### volatility (square root of the conditional variance) - similar to the object created for model fitting, 
##### ugfore contains two slots (@model and @forecast) and you can use names(ugfore@forecast) to figure out under 
##### which names the elements are saved. For instance you can extract the conditional volatility forecast as follows - 
##### note the volatility is the square root of the conditional variance
ug_f <- ugfore@forecast$sigmaFor
plot(ug_f, type = "l")

#  put these forecasts into context let's display them together with the last 50 observations used in the estimation
ug_var_t <- c(tail(ug_var,20),rep(NA,10))  # gets the last 20 observations
ug_res2_t <- c(tail(ug_res2,20),rep(NA,10))  # gets the last 20 observations
ug_f <- c(rep(NA,20),(ug_f)^2)

# the forecast of the conditional variance picks up from the last estimated conditional variance - in fact it decreases 
# from there, slowly, towards the unconditional variance value
plot(ug_res2_t, type = "l")
lines(ug_f, col = "orange")
lines(ug_var_t, col = "green")

##### Multivariate GARCH models #####
# using the rmgarch package which has a lot of useful functionality - estimate a multivariate volatility model for 
# the returns of BP, Google/Alphabet and IBM shares

# assume that we are using the same univariate volatility model specification for each of the three assets
# DCC (MVN)
uspec.n = multispec(replicate(3, ugarchspec(mean.model = list(armaOrder = c(1,0)))))
##### recognise that ugarchspec(mean.model = list(armaOrder = c(1,0))) specifies an AR(1)-GARCH(1,1) model - using 
##### replicate(3, ugarchspec...) we replicate this model 3 times (as we have three assets, IBM, Google/Alphabet and BP)

# now estimate these univariate GARCH models using the multifit command
multf = multifit(uspec.n, rX)
multf

##### dynamic conditional correlation (DCC model) which allows thecorrelation matrix to depend of the time - 
# specify the correlation specification we use the dccspec function
# this specification we have to state how the univariate volatilities are modeled (as per uspec.n) and how complex the dynamic structure 
# of the correlation matrix is (here we are using the most standard dccOrder = c(1, 1) specification)
spec1 = dccspec(uspec = uspec.n, dccOrder = c(1, 1), distribution = 'mvnorm')

# estimate the model using the dccfit function
fit1 = dccfit(spec1, data = rX, fit.control = list(eval.se = TRUE), fit = multf)

##### estimate the model as specified in spec1, using the data in rX - the option fit.control = list(eval.se = TRUE) 
##### ensures that the estimation procedure produces standard errors for estimated parameters - importantly fit = multf 
##### indicates that we ought to use the already estimated univariate models as they were saved in multf

##### estimate a multivariate volatility model like the DCC model you are typically interested in the estimated 
##### covariance or correlation matrices. After all it is at the core of these models that you allow for time-variation 
##### in the correlation between the assets 

# get the model based time varying covariance (arrays) and correlation matrices
cov1 = rcov(fit1)  # extracts the covariance matrix
cor1 = rcor(fit1)  # extracts the correlation matrix

# tounderstand the object we have at our hands here we can have a look at the dimension
dim(cor1)
cor1[,,dim(cor1)[3]]

# plot the time-varying correlation between Google and BP, which is 0.275244 on that last day - in our matrix with 
# returns rX BP is the second asset and Google the 3rd - in any particular correlation matrix we want the element 
# in row 2 and column 3
cor_BG <- cor1[2,1,]   # leaving the last dimension empty implies that we want all elements
cor_BG <- as.xts(cor_BG)  # imposes the xts time series format - useful for plotting
plot(cor_BG)

# plot all three correlations between the three assets
par(mfrow=c(3,1))  # this creates a frame with 3 windows to be filled by plots
plot(as.xts(cor1[1,2,]),main="IBM and BP")
plot(as.xts(cor1[1,3,]),main="IBM and Google")
plot(as.xts(cor1[2,3,]),main="BP and Google")

##### Forecasts #####
# use your estimated model to produce forecasts for the covariance or correlation matrix 
dccf1 <- dccforecast(fit1, n.ahead = 10)
dccf1

# actual forecasts for the correlation can be addresse via 
Rf <- dccf1@mforecast$R    # use H for the covariance forecast
str(Rf)

##### the object Rf is a list with one element - it turns out that this one list item is then a 3 dimensional 
##### matrix/array which contains the the 10 forecasts of 3×3 correlation matrices. If we want to extract, say, 
##### the 10 forecasts for the correlation between IBM (1st asset) and BP (2nd asset), we have to do this 
##### in the following way
corf_IB <- Rf[[1]][1,2,]  # Correlation forecasts between IBM and BP
corf_IG <- Rf[[1]][1,3,]  # Correlation forecasts between IBM and Google
corf_BG <- Rf[[1]][2,3,]  # Correlation forecasts between BP and Google

# [ [1] ] tells R to go to the first (and here only) list item and then [1,2,] instructs R to select 
# the (1,2) element of all available correlation matrices
# display the forecast along with the last in-sample estimates of correlation
par(mfrow=c(3,1))  # this creates a frame with 3 windows to be filled by plots
c_IB <- c(tail(cor1[1,2,],20),rep(NA,10))  # gets the last 20 correlation observations
cf_IB <- c(rep(NA,20),corf_IB) # gets the 10 forecasts
plot(c_IB,type = "l",main="Correlation IBM and BP")
lines(cf_IB,type = "l", col = "orange")

c_IG <- c(tail(cor1[1,3,],20),rep(NA,10))  # gets the last 20 correlation observations
cf_IG <- c(rep(NA,20),corf_IG) # gets the 10 forecasts
plot(c_IG,type = "l",main="Correlation IBM and Google")
lines(cf_IG,type = "l", col = "orange")

c_BG <- c(tail(cor1[2,3,],20),rep(NA,10))  # gets the last 20 correlation observations
cf_BG <- c(rep(NA,20),corf_BG) # gets the 10 forecasts
plot(c_BG,type = "l",main="Correlation BP and Google")
lines(cf_BG,type = "l", col = "orange")





