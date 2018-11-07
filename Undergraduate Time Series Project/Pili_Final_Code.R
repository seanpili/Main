# read in our data 

library(readr)
station_2_ <- read_csv("C:/Users/Sean/Downloads/station (2).csv",col_types = cols(APR = col_number(),  AUG = col_number(), `D-J-F` = col_number(), DEC = col_number(), FEB = col_number(),  `J-J-A` = col_number(), JAN = col_number(), JUL = col_number(), JUN = col_number(), `M-A-M` = col_number(), MAR = col_number(), MAY = col_number(), NOV = col_number(), OCT = col_number(), `S-O-N` = col_number(),  SEP = col_number(), metANN = col_number()))

#View(station_2_)

# take only the necessary rows and columns... i.e the actual surface temperatures

stat2 = station_2_[,2:13]
# unfortunatley each month has it's own column in the dataset.
# we will convert all of the data values into a vector.

x = as.vector(t(stat2))
View(x)
# create a separate vector for the year of each observation.... 
years =rep(1901:2017,12)
year= years[order(years)]
# create a month variable (the name was saved as a column title previously)
Month=rep(paste("M",1:12,sep=""),length=1404)
# for some reason x would read in as a factor after using the cbind function
weather_dat = cbind(year,Month,x)
weather_dat =data.frame(weather_dat)
# so we have to make a small adjustment
x1 = as.numeric(paste(x))
# the same thing seems to happen for the year variable. 
weather_dat = weather_dat[,-3]
#View(weather_dat)
weather_dat =cbind(weather_dat,x1)
weather_dat = weather_dat[,-1]
weather_dat = cbind(year,weather_dat)

###Rcode..... 



ts.plot(x1)
# we decided to choose the most recent range of obervations that did not have 
# any missing values, as represented by 999.9 


############################ SELECTING OUR CURRENT RANGE############

weather_dat[1213,] # 2002

weather_dat[1392,]# 2016
View(weather_dat)
class(weather_dat[,3])
y =subset(weather_dat,year>=2002&year<=2016)

class(y[,3])

##### MESSING AROUND


# TAKING OUR SEASONAL DIFFERENCE
y.star1=diff(y[,3],12)
pacf(y.star1,lag.max= 60,ylim=c(-1,1))
acf(y.star1,lag.max= 60,ylim=c(-1,1))

class(y.star1)
class(y[,3])
library(tseries)
library(forecast)
train = y[1:168,3]
test = y[169:180,3]
train1 = y.star1[1:156]
test1 = y.star1[157:168]


### ARIMA(1,0)
model1=arima(train1, order = c(1,0,0))
 #BIC
residz1 = sum((model1$residuals^2))
n1 = 156
bic1.12 <- n1*log(residz1/n1) + 1*log(n1) + n1 + n1*log(2*pi)
# BIC = 701.891
fitted(model1)
predz1 = predict(model1,12)
predz1 = predz1$pred
sum((test1-predz1)^2)/12

###### Arima (0,0,2)
model1.0=arima(train1,order=c(0,0,2),include.mean =FALSE)
predz1.0 = predict(model1.0,12)
sum((test1-predz1.0$pred)^2)/12

resid1.0 = model1.0$residuals
residz1.0 = sum((resid1.0)^2)
n1.0 = 156
bic1.0 <- n1.0*log(residz1.0/n1.0) + 2*log(n1.0) + n1.0 + n1.0*log(2*pi)
bic1.0 
###### ARIMA(0,0,1)
model0.1= arima(train1,order=c(0,0,1),include.mean =FALSE)
predz0.1 = predict(model0.1,12)
sum((test1-predz0.1$pred)^2)/12 

resid0.1 = model0.1$residuals
residz0.1 = sum((resid0.1)^2)
n0.1 = 156
bic0.1 <- n1.0*log(residz0.1/n1.0) + 1*log(n1.0) + n1.0 + n1.0*log(2*pi)
bic0.1 
702.3125
######### ARIMA (2,0,2)
model1.12= arima(train1,order=c(2,0,2),include.mean=FALSE)
fitted(model1.12)
resid1.12 = model1.12$residuals
residz1.12 = sum((model1.12$residuals)^2)
n1.12 = 156
n1.12
bic1.12 <- n1.12*log(residz1.12/n1.12) + 4*log(n1.12) + n1.12 + n1.12*log(2*pi)

bic1.12 
predz1.12 = predict(model1.12,12)
sum((test1-predz1.12$pred)^2)/12


##### ARIMA (1,0,1)

model1.1 = arima(train1,order=c(1,0,1),include.mean=FALSE)
n1.1 = 156
resid1.1 = model1.1$residuals
residz1.1 = sum((resid1.1)^2)
bic1.1 <- n1.1*log(residz1.1/n1.1) + 2*log(n1.1) + n1.1 + n1.1*log(2*pi)
bic1.1
predz1.1 = predict(model1.1,12)
predz1.1 = predz1.1$pred
sum((test1-predz1.1)^2)/12


######### ARIMA (1,0,2)
model1.2 = arima(train1,order=c(1,0,2),include.mean=FALSE)
resid1.2 = model1.2$residuals
residz1.2 = sum((resid1.1^2))
bic1.2 <- n1.1*log(residz1.2/n1.1) + 3*log(n1.1) + n1.1 + n1.1*log(2*pi)

predz1.2 = predict(model1.2,12)
predz1.2 = predz1.2$pred
sum(test1-predz1.2)


#### ARIMA 1,0,2
model2.1 = arima(train1,order=c(2,0,1),include.mean=FALSE)
resid2.1 = model2.1$residuals
residz2.1 = sum((resid2.1^2))
n2.1 = 156
bic2.1 <- n2.1*log(residz2.1/n2.1) + 3*log(n2.1) + n2.1 + n2.1*log(2*pi)
bic2.1 
predz2.1 = predict(model2.1,12)
predz2.1 = predz2.1$pred
sum(test1-predz2.1)


Box.test(model1.0$residuals,type="Ljung",lag=60,fitdf=0)
Box.test(model1.0$residuals,type="Ljung",lag=60,fitdf=0)
Box.test(model1.0$residuals,type="Ljung",lag=60,fitdf=0)
Box.test(model1.0$residuals,type="Ljung",lag=60,fitdf=0)
# ARMA NEEDS STATIONARY, ARIMA JUST NEEDS CONSTANT VARIANCE 
pacf(model1$residuals)
acf(model1$residuals)
par(mfrow=c(1,2))
pacf(model1.0$residuals)
acf(model1.0$residuals)
pacf(model1.2$residuals)
acf(model1.2$residuals)
pacf(model1.1$residuals)
acf(model1.1$residuals)
pacf(model1.12$residuals)
acf(model1.12$residuals)
coeftest(model1)
summary(model1)
####### SARIMA TIME
library(astsa)
library(lmtest)
plotdev.off() #Fixes Graphics error
par(mar = rep(2,4)) #fixes plot margins too large error
# SARIMA (1,0,0)(0,1,1)12 without an intercept 
model2=sarima(train,1,0,0,0,1,1,12,no.constant = TRUE)
coeftest(model2$fit)


sarima_predz=predict(model2$fit,12)
sarima_predz = as.vector(sarima_predz$pred)

class(as.vector(sarima_predz))

sum((test-sarima_predz)^2)/12

sarima_predz60=predict(model2$fit,60)
sarima_predz60 = as.vector(sarima_predz60$pred)

time = 1:180

plot(time[1:168],train,pch=20,xlim=c(min(time),228),ylim=c(min(train),max(sarima_predz)),"o") 
points(169:228,sarima_predz60,pch=20,col="red","o")

# MSPE# of 2.441992
resid2s=resid(model2$fit)
residz2s = sum((resid2s^2))
n2s = 168
bic2s <- n2s*log(residz2s/n2s) + 2*log(n2s) + n2s + n2s*log(2*pi)
bic2s # 654.5588




###########################################################

#let's try the decompositional method. 



ts.plot(y[,3],type="o")
time = c(1:180)
y2 = cbind(y,time)
#class(y2$time)
#y2 = data.frame(y2)
#y2[168,]
win.graph()
ts.plot(train.set[,3],type="o")
train.set = y2[1:168,]
test.set = y2[169:180,]
#y2[,4]=as.integer(y2[,4])
#y2[,3]=as.numeric(y2[,3])
trainnumz = train.set[,3]
testnumz= test.set[,3]
#numz2 = as.numeric[]

class(y2[,4]) 
class(y2[,3])
###### harmonic regression. 
ts.plot(y2[,3])
L=12
model3.0 = lm(trainnumz~I(sin(2*pi*time/L))+I(cos(2*pi*time/L)),data=train.set)
lines(model3$fitted.values,col='red') 
summary(model3)
win.graph()
par(mfrow=c(1,2))
resid3.0s= model3.0$residuals
residz3.0s = sum((resid3s^2))
n3s = 168
bic3.0s <- n3s*log(residz3.0s/n3s) + 2*log(n3s) + n3s + n3s*log(2*pi)
bic3.0s # 658.6687

model3 = lm(trainnumz~time+I(sin(2*pi*time/L))+I(cos(2*pi*time/L)),data=train.set)
summary(model3)
summary(model3.0)
resid3s= model3.0$residuals

plot(model3.0$fitted,resid3.0s, main = 'fitted values v. residuals for decomposition')
residz3s = sum((resid3s^2))
n3s = 168
bic3s <- n3s*log(residz3s/n3s) + 3*log(n3s) + n3s + n3s*log(2*pi)
bic3s # 663.7926



######## seasonal adjustment models



ts.plot(y2[,3])



######## Seasonal Adjustment With time feature
ts.plot(y2[,3])
model4 = lm(trainnumz~time+Month,data=train.set)
summary(model4)

### BIC

resid4= model4$residuals
residz4 = sum((resid4^2))
n4 = 168
bic4. <- n4*log(residz4/n4) + 13*log(n4) + n4 + n4*log(2*pi)
bic4. # 705.2272

#############################Seasonal Adjustment############################

model4not = lm(trainnumz~Month,data=train.set)
summary(model4not)
resid4not= model4not$residuals
residz4not = sum((resid4not^2))
n4 = 168
bic4not <- n4*log(residz4not/n4) + 12*log(n4) + n4 + n4*log(2*pi)
bic4not
#700.1069

########## DECOMPOSITION MSPE'S#############

pred1.0 = predict(model3,newdata=test.set)
pred2 = predict(model4,newdata=test.set)
#pred3 = predict(model4.halflog,newdata=test.set)
#pred4 = predict(model4.notimeh,newdata=test.set)
pred5 = predict(model4not,newdata=test.set)
pred6 = predict(model3.0,newdata=test.set)
sum((y2[169:180,3]-pred1.0)^2)/12      # 2.608179
sum((y2[169:180,3]-pred2)^2)/12  # 2.192646
#sum((y2[169:180,3]-exp(pred3))^2)/12 # 2.31888
#sum((y2[169:180,3]-exp(pred4))^2)/12 # 2.33296
sum((y2[169:180,3]-pred5)^2)/12 # 2.2167
sum((y2[169:180,3]-pred6)^2)/12 # 2.63479


View(y2[169:180,3])

BIC(model4)
BIC(model3)
BIC(model4.log)
dim(y2)

#### predictions with our final Decomposition model. 
x = rep(2017,12) 
y = rep(2018,12)
z = rep(2019,12)
a = rep(2020,12)
b = rep(2021,12)
new=data.frame(time=c(169:228),Season=rep(paste("M",1:12,sep=""),5), year=as.vector(c(x,y,z,a,b)))
 
View(new)
predz7 = predict(model3.0,newdata=new,interval='prediction')
predz7[,2]


mean(predz7)
y2$x1
plot(y2$time,y2$x1,pch=20,xlim=c(min(y2$time),228),ylim=c(min(y2$x1),max(predz7)),"o",main="Fits and 5 year predictions for Decomposition Model")
length(predz7)
points(169:228,predz7[,1],pch=20,col="red","o")
lines(model3.0$fitted.values,col='green')
lines(169:228,predz7[,2],pch=20,col='blue')
lines(169:228,predz7[,3],pch=20,col='blue')
