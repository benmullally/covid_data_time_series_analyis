library(dplyr)
install.packages("tseries")
library(tseries)
#Find the India Dataset
dataset=read.csv('owid-covid-data (2).csv')
india=filter(dataset, location == "India")
india_total_cases=india[,5]
#Make it a time series
india_total_cases.ts=ts(india_total_cases)
#PLot time series + ACF's
plot(india_total_cases.ts,main="India Total COVID Cases",ylab="Total Cases",xlab="Days Since Start of Pandemic")
acf(india_total_cases.ts,main="ACF of the Total Cases")
pacf(india_total_cases.ts)
#adf tests
adf.test(india_total_cases.ts,alternative='stationary',k=28)
adf.test(diff(log(india_total_cases.ts)),alternative='stationary',k=28)
adf.test(diff(diff(log(india_total_cases.ts))),alternative = 'stationary',k=28)
#Plotting diff(diff(log)) and ACF's
inddia=diff(diff(log(india_total_cases.ts)))
plot(inddia,main="Double-Differenced, Logged Total Cases",ylab="")
acf(inddia,lag.max=25,main="Total Cases ACF After Differencing")
pacf(inddia,lag.max=25,main="Total Cases PACF After Differencing")
#Test model for the time series
ma3.mod=arima(inddia,order=c(0,0,3))
ma3.mod
mean(ma3.mod$residuals)
acf(ma3.mod$residuals)
pacf(ma3.mod$residuals)
tsdiag(ma3.mod,gof.lag=25)
#Perform Ljung-Box
resi=(ma3.mod$residuals)
resi.acf=acf(resi)
length(resi.acf$acf)-1
Lb.result = Box.test(resi, lag = length(resi.acf$acf)-1,
                     type = "Ljung-Box", fitdf = 3)
Lb.result
#Predict final 25 values
new.ma3.mod=arima(india_total_cases.ts[1:715],order=c(0,2,3))
ma3.mod.f = predict(new.ma3.mod, n.ahead=25)
xf = ma3.mod.f$pred
xf.l = ma3.mod.f$pred - 1.96*ma3.mod.f$se
xf.u = ma3.mod.f$pred + 1.96*ma3.mod.f$se
n=800
m=740
M=601:800
plot(700:760, india_total_cases.ts[700:760], xlim=c(700,760),type="l",ylim=c(30000000,50000000),main="Predicting for 25 Previous Days",xlab="Days Since Start of Pandemic",ylab="Total Cases")
lines(xf,col=6)
lines(xf.l,col=3)
lines(xf.u,col=3)
#Differences between actual and prediction
length(india_total_cases.ts)
comp.s=(india_total_cases.ts[716:740]-xf[1:25])
plot(comp.s,type='l',main="Differences between Prediction and Actual Values",xlab="Days After Day 715", ylab="Differences")
MAE2.s=mean(abs(comp.s))
MAE2.s
RMSE2.s=sqrt(mean(comp.s^2))
RMSE2.s
#Predict final 140 values
new.ma3.mod.bs=arima(india_total_cases.ts[1:600],order=c(0,2,3))
ma3.mod.f.bs = predict(new.ma3.mod.bs, n.ahead=140)
xf.bs = ma3.mod.f.bs$pred
xf.l.bs = ma3.mod.f.bs$pred - 1.96*ma3.mod.f.bs$se
xf.u.bs = ma3.mod.f.bs$pred + 1.96*ma3.mod.f.bs$se
n.bs=800
m.bs=740
M.bs=601:800
plot(1:m.bs, india_total_cases.ts[1:m], xlim=c(1,n.bs),type="l",ylim=c(0,60000000))
lines(xf.bs,col=6)
lines(xf.l.bs,col=3)
lines(xf.u.bs,col=3)
#Differences between actual and predicted values
length(india_total_cases.ts)
comp=(india_total_cases.ts[601:740]-xf.bs[1:140])
plot(comp,type='l')
MAE2=mean(abs(comp))
MAE2
RMSE2=sqrt(mean(comp^2))
RMSE2
#Forecast for the future cases
future.ma3.mod=arima(india_total_cases.ts,order=c(0,2,3))
ma3.mod.future = predict(future.ma3.mod, n.ahead=100)
xf2 = ma3.mod.future$pred
xf.l2 = ma3.mod.future$pred - 1.96*ma3.mod.future$se
xf.u2 = ma3.mod.future$pred + 1.96*ma3.mod.future$se
n2=850
m2=740
M2=741:850
plot(1:m2, india_total_cases.ts[1:m2],xlim=c(0,n2),type="l",ylim=c(0,60000000),main="Forecast for the Next 100 Days", xlab="Days Since Start of Pandemic",ylab="Total Cases")
lines(xf2,col=6)
lines(xf.l2,col=3)
lines(xf.u2,col=3)


