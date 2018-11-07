##Note this code is messy at best and hard to follow
##Using the Titles should help navigate this monstrosity.

rm(list = ls())


############# DATA CLEANING ###########
station_2_ <- read.csv("C:/Users/michael/Documents/Virginia Tech/STAT 4534 Time Series/station.csv")
View(station_2_)

stat2 = station_2_[,2:13]

x = as.vector(t(stat2))
years =rep(1901:2017,12)
year= years[order(years)]
Month=rep(paste("M",1:12,sep=""),length=1404)
weather_dat = cbind(year,Month,x)

which(x==999.9)
weather_dat[1213,]

weather_dat[1392,]

length(1:333)
length(as.numeric(weather_dat[,3]))


#Plotting Full DATA
plot(1:1404, as.numeric(weather_dat[,3]), 'o',
     main = "Weather in Williamsburg (1901-2017)", ylab = "Degrees in Celcius",
     xlab = "Time (1901-2017)", ylim = c(0, 30))


y =subset(weather_dat,year>=2002&year<=2016)
dataNoTrain <- as.numeric(y[,3])

##Separate into Training/Test Data
data <- dataNoTrain[-(169:180)]
dataTest = dataNoTrain[169:180]


library(datasets)
library(tseries)
#data=as.vecdatator(AirPassengers)

##Plotting the Training Data
plot(1:length(as.numeric(data)), as.numeric(data), 'o', xlab = "Time (2002-2015)",
     ylab = "Temperature (deg C)", main = "Weather in Williamsburg")
y = as.numeric(data)

########## Acf/PAcf graphs of original data #######
par(mfrow = c(1,2))
acf(data,lag.max = 60,ylim=c(-1,1), main = "Acf: No differences")
pacf(data,lag.max = 60,ylim=c(-1,1), main = "PAcf: No differences")



# Firstly take the seasonal difference for that monthly data
y.star1=diff(y,12)
ts.plot(y.star1, main="Time Serties plot for the log Air Passangers after seasonal difference")

#seems stationary already


## if we assume we aready have no seasonal effect after taking the seasonal difference
## check the arima (0,1,3) for the non seasonal part and make sure that
# "WE USE THE y.star WHICH IS seseasonalized series"

model1=arima(y.star1, order = c(0,1,3)) # estimate ARIMA(0,1,3) model using ARIMA function
print(model1)

library(lmtest)
#install.packages("lmtest")

coeftest(model1) # to test the parameters significance

# ACf and PACF for the residual to check they are white noise
acf(residuals(model1),lag.max = 60,ylim=c(-1,1))
pacf(residuals(model1),lag.max = 60,ylim=c(-1,1))

## so far, we just took care of the non seasonal part
# but we still can observe the seasonal pattern in the residuals
## so we would fit SARIMA model to take care of that part in the model
## Try fit SARIMA [(0,1,3) (0,1,2) 12] and
# make sure you use the data with the log tranformation without any differences
library(astsa)
library(lmtest)
#install.packages("astsa", dependencies = TRUE)




dev.off() #fixes graphical errors
par(mar = rep(2, 4)) #fixes margins too big
model2 <- sarima(as.numeric(y[,3]),2,0,2,1,0,2,12)
#model2 <- sarima(y,0,2,2,0,2,2,12)
BIC(model2$fit)
coeftest(model2$fit) # test the significance of the model


?sarima

## this suggests estimating just SARIMA(0,1,1)(0,1,1)12
model3=sarima(y,0,1,1,0,1,1,12)
coeftest(model3$fit)
summary(model3$fit)
BIC(model3$fit)


## Ljung Box test for white noise of the residuals
# H0: residuals have white noise model
# H1: residuals don't have white noise model
# We want to accept H0 so our fitted model was appropriate
residuals=resid(model3$fit) ## watch the residuals is caclulated in different function
Box.test(residuals,type="Ljung",lag=20,fitdf=0)



# ACf and PACF for the residual to check they are white noise
acf(residuals,lag.max = 60,ylim=c(-1,1))
pacf(residuals,lag.max = 60,ylim=c(-1,1))

# forecast for specific model
library(forecast)

predict(model3$fit,25) # USE MODEL 3 TO FORECAST 5 STEPS A HEAD IN THE FUTURE

y.hat=y-residuals   # TO GET THE FITTED VALUES FROM THE MODEL

par(mfrow=c(1,1))
plot(y,y.hat,ylab="Fitted",xlab="Original") # plot the fitted versus the original observations


## time series plot for the fitted values
ts.plot(exp(y.hat),ylab="Passengers", main="Time Serties Plot for \n the Fitted and Original Values") # I exponentiate the fitted values to return to original units
lines(data,col="red")  ## add red line for the original data



plot(1:length(data), data, 'o')
par(mfrow = c(1,2))
acf(data,lag.max = 60,ylim=c(-1,1))
pacf(data,lag.max = 60,ylim=c(-1,1))
model04=arima(dataDeTrend1, order = c(0,0,4), include.mean = FALSE) # estimate ARMA(1,1)

par(mfrow = c(1,2))
acf(resid(model04),lag.max = 60,ylim=c(-1,1))
pacf(resid(model04),lag.max = 60,ylim=c(-1,1))

#Propose MA(4) but has seasonality
################### SARIMA MODELS #################

library(astsa)
library(lmtest)

dev.off() #fixes graphical errors
par(mar = rep(2, 4)) #fixes margins too big
modelSarima14 <- sarima(data,1,1,4,1,0,2,12, no.constant = TRUE)
modelSarima13 <- sarima(data,1,1,3,1,0,2,12, no.constant = TRUE)
modelSarima12 <- sarima(data,1,1,2,1,0,2,12, no.constant = TRUE)
modelSarima112 <- sarima(data,2,1,1,1,0,2,12, no.constant = TRUE)

modelSarima111 <- sarima(data,1,1,1,1,0,1,12, no.constant = TRUE)
modelSarima112 <- sarima(data,1,0,1,1,0,2,12, no.constant = TRUE)
modelSarima112 <- sarima(data,1,0,1,1,1,1,12, no.constant = TRUE)


modelSarimaSean <- sarima(data,1,0,0,0,1,1,12, no.constant = TRUE)
modelSarimaSean <- sarima(data,1,0,0,1,1,2,12, no.constant = FALSE)
modelSarimaSean <- sarima(data,0,0,6,1,0,2,12, no.constant = TRUE)
modelSarimaSean2 <- sarima(data,0,0,2,1,0,1,12, no.constant = TRUE)


modelSarimaRework1 <- sarima(data,1,0,1,1,0,2,12, no.constant = TRUE)

modelSarimaRework2 <- sarima(as.numeric(data),1,0,2,1,0,2,12, no.constant = TRUE)
modelSarimaRework2.reduced <- sarima(as.vector(data),1,0,2,1,0,1,12, no.constant = TRUE)
modelSarimaRework3 <- sarima(as.numeric(data),2,0,1,1,0,2,12, no.constant = TRUE)
modelSarimaRework4 <- sarima(as.numeric(data),2,0,2,1,0,2,12, no.constant = TRUE)

View(data)
as.vector(data)

predict(modelSarimaRework2.reduced, 12)


modelSarimaReworkBruteForce <- sarima(data,1,0,0,0,1,1,12, no.constant = TRUE)




coeftest(modelSarima14$fit) # test the significance of the model
coeftest(modelSarima13$fit) # test the significance of the model
coeftest(modelSarima12$fit) # test the significance of the model
coeftest(modelSarima112$fit) # test the significance of the model


coeftest(modelSarima111$fit) # test the significance of the model
coeftest(modelSarima112$fit) # test the significance of the model
coeftest(modelSarimaSean$fit) # test the significance of the model
coeftest(modelSarimaSean2$fit) # test the significance of the model


coeftest(modelSarimaRework1$fit) # test the significance of the model
coeftest(modelSarimaRework2$fit) # test the significance of the model
coeftest(modelSarimaRework3$fit) # test the significance of the model
coeftest(modelSarimaRework4$fit) # test the significance of the model


coeftest(modelSarimaReworkBruteForce$fit) # test the significance of the model
coeftest(modelSarimaRework2.reduced$fit) # test the significance of the model


library(forecast)
predict(modelSarimaRework2.reduced$fit, 12)


SarimaReworkfit <- data - resid(modelSarimaRework$fit)
SarimaReworkRes <- resid(modelSarimaRework$fit)



modelSarimaSean$fit
modelSarima111$fit
modelSarima112$fit

############## BIC Calculations ##########
n <- length(data)
BICSarima111 <- n*log(sum(resid(modelSarima111$fit)^2/n)) + 4*log(n) + n + n*log(2*pi)
BICSarima112 <- n*log(sum(resid(modelSarima112$fit)^2/n)) + 4*log(n) + n + n*log(2*pi)
BICSarima112

BICSarimaSean <- n*log(sum(resid(modelSarimaSean$fit)^2/n)) + 5*log(n) + n + n*log(2*pi)
BICSarimaSean


n <- length(data)
BICSarimaRework2 <- n*log(sum(resid(modelSarimaRework2$fit)^2/n)) + 6*log(n) + n + n*log(2*pi)
BICSarimaRework2.reduced <- n*log(sum(resid(modelSarimaRework2.reduced$fit)^2/n)) + 4*log(n) + n + n*log(2*pi)
BICSarimaRework1 <- n*log(sum(resid(modelSarimaRework1$fit)^2/n)) + 5*log(n) + n + n*log(2*pi)
BICSarimaRework3 <- n*log(sum(resid(modelSarimaRework3$fit)^2/n)) + 6*log(n) + n + n*log(2*pi)
BICSarimaRework4 <- n*log(sum(resid(modelSarimaRework4$fit)^2/n)) + 7*log(n) + n + n*log(2*pi)

BICSarimaReworkBruteForce <- n*log(sum(resid(modelSarimaReworkBruteForce$fit)^2/n)) + 2*log(n) + n + n*log(2*pi)

######### MSPE Calculations #########
predict(modelSarimaRework2.reduced$fit, 12)


residSarima111 = resid(modelSarima111$fit)
fittedSarima111 = data - residSarima111
predSarima111 = predict(modelSarima111$fit,60)$pred
predSarima111Test = predict(modelSarima111$fit,12)$pred

residSarima112 = resid(modelSarima112$fit)
fittedSarima112 = data - residSarima112
predSarima112 = predict(modelSarima112$fit,60)$pred
predSarima112Test = predict(modelSarima112$fit,12)$pred

predSarima112Test = predict(modelSarima112$fit,12)$pred

residSarimaRework2.reduced = resid(modelSarimaRework2.reduced$fit)
fittedSarimaRework2.red = data - residSarimaRework2.reduced
predSarimaRework2.reduced = predict(modelSarimaRework2.reduced$fit,60)$pred
predSarimaRework2.redTest = predict(modelSarimaRework2.reduced$fit,12)$pred



residSarimaSean = resid(modelSarimaSean$fit)
fittedSarimaSean = data - residSarimaSean
predSarimaSean = predict(modelSarimaSarimaRework2.reduced$fit,60)$pred
predSarimaSeanTest = predict(modelSarimaSean$fit,12)

predSARIMA = c()

for (i in 1:12)
{
  predSARIMA[i] = predSarimaSeanTest[[i]]
}

predict(modelSarimaSean$fit, 12)


#### Plotting Informative SARIMA #######
#Note that predictions did not work for some reason~ see Conclusions
plot(1:length(data), data , 'o', main = 'SARIMA (1,1,1)(1,0,1)12', xlim = c(0, 250))
lines(1:length(fittedSarima111), fittedSarima111, 'o',col = 'green')
lines(181:240, predSarima111, col = 'red')
points(181:240, predSarima111, col = 'red')


#Finally Solved the prediction problem
predSARIMA <- as.vector(sarima.for(data,60,1,0,2,1,0,1,12)$pred)


MSPE111 <- sum((dataTest - predSARIMA)^2)/length(predSarima111Test)
MSPE112 <- sum((dataTest - predSarima112Test)^2)/length(predSarima112Test)
MSPESean <- sum((dataTest - predSARIMA)^2)/length(predSARIMA)
MSPESarimaRework2.reduced <- sum((dataTest - predSARIMA)^2)/length(predSARIMA)



############ Various Acf Plots and other code ###########
par(mfrow = c(1,2))
acf(resid(modelSarima112$fit),lag.max = 60,ylim=c(-1,1))
pacf(resid(modelSarima112$fit),lag.max = 60,ylim=c(-1,1))

par(mfrow = c(1,2))
acf(dataLag12,lag.max = 60,ylim=c(-1,1))
pacf(dataLag12,lag.max = 60,ylim=c(-1,1))

modelSarima111$BIC

dataLag12 = diff(data, 12)
plot(1:length(dataLag12), dataLag12, 'o', main = "Deseasonalized Lag 12")

adf.test(dataLag12, k=0)

dataDeTrend1 = diff(dataLag12, 1)
plot(1:length(dataDeTrend1), dataDeTrend1, 'o', main = "Deseasonalized/Detrend")


plot(1:length(data), data, 'o')



par(mfrow = c(1,2))
acf(data,lag.max = 60,ylim=c(-1,1))
pacf(data,lag.max = 60,ylim=c(-1,1))

diffLag12.2 <- (diff(diff(data,12)))
par(mfrow = c(1,2))
acf(diffLag12.2,lag.max = 60,ylim=c(-1,1))
pacf(diffLag12.2,lag.max = 60,ylim=c(-1,1))





par(mfrow = c(1,2))
acf(dataDeTrend1,lag.max = 60,ylim=c(-1,1))
pacf(dataDeTrend1,lag.max = 60,ylim=c(-1,1))


model10=arima(dataLag12, order = c(1,0,0), include.mean = FALSE) # estimate ARMA(1,1)

############ Residuals ##########
par(mfrow = c(1,2))
acf(resid(modelSarimaSean$fit),lag.max = 60,ylim=c(-1,1))
pacf(resid(modelSarimaSean$fit),lag.max = 60,ylim=c(-1,1))


#########################################################

dataDeTrend2 = diff(dataDeTrend1, 1)
plot(1:length(dataDeTrend2), dataDeTrend2, 'o')

dataDeTrend3 = diff(dataDeTrend2, 1)
plot(1:length(dataDeTrend3), dataDeTrend3, 'o')

dataDeTrend4 = diff(dataDeTrend3, 1)
plot(1:length(dataDeTrend4), dataDeTrend4, 'o')



############# Plotting/Predictions ##############
modelSarimaSean <- sarima(c(data),1,0,0,0,1,1,12, no.constant = TRUE)

residSarimaSean = resid(modelSarimaSean$fit)
fittedSarimaSean = data - residSarimaSean
predSarimaSean = predict(modelSarimaSean$fit,60)$pred
predSarimaSeanTest = predict(modelSarimaSean$fit,12)


predSarimaSeanTest2 = predict(modelSarimaSean$fit,60, interval = "prediction")

predSARIMA = as.vector(predSarimaSean)

#THis part did not work
newhf4= data.frame(Time=c(169:229))
pred_hf4=predict(modelSarimaSeannewdata=newhf4, interval = 'prediction')

#Had to use sarima.for() function
for.sarima <- sarima.for(data, 60, 1, 0, 0, 0, 1, 1, 12)
for.sarima$pred


plot(1:(length(data) + length(dataTest)), c(data,dataTest) , 'o',
     main = 'SARIMA (1,0,0)(0,1,1)12', xlim = c(0, 250), xlab = "Time (2002-2020)",
     ylab = "Degrees Celcius")
lines(1:length(fittedSarimaRework2.reduced), fittedSarimaRework2.reduced, col = 'green')
lines(181:240, predSarimaRework2.reduced, col = 'red')
points(181:240, predSarimaRework2.reduced, col = 'red')



for (i in 1:12)
{
  predSARIMA[i] = predSarimaRework2.reducedTest[[i]]
}

predict(modelSarimaSean$fit, 12)


plot(1:(length(data) + length(dataTest)), c(data,dataTest) , 'o',
     main = 'SARIMA (1,0,2)(1,1,1)12', xlim = c(0, 250), xlab = "Time (2002-2020)",
     ylab = "Degrees Celcius")
lines(1:length(fittedSarimaRework2.reduced), fittedSarimaRework2.reduced, col = 'green')
lines(181:240, predSarimaRework2.reduced, col = 'red')
points(181:240, predSarimaRework2.reduced, col = 'red')

######## Attempting to solve the PREDICT() problem ###############
modelSarimaSean <- sarima(c(data),1,0,2,1,1,1,12, no.constant = TRUE)

residSarimaRework2.reduced = resid(modelSarimaRework2.reduced$fit)
fittedSarimaRework2.reduced = data - residSarimaRework2.reduced
predSarimaRework2.reduced = predict(modelSarimaRework2.reduced$fit,60)$pred
predSarimaRework2.reducedTest = predict(modelSarimaRework2.reduced$fit,12)


######### Graphing the prediction intervals #########
sarima.for(data,60,1,0,0,0,1,1,12)
