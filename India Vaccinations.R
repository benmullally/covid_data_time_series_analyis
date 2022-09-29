library(dplyr)
library(tseries)
dataset = read.csv("owid-covid-data.csv")
filtered_data = filter(dataset, location == "India")
india_data = select(filtered_data, total_vaccinations)
india_data = na.omit(india_data)
india_data = (india_data$total_vaccinations)
#remove first 100 entries as was affecting the residuals so couldn't get accurate model
india_data = india_data[101:369]
#df test, p-value=0.01 after 2 diffs
#log used as it is removing exponential variance, stabilise variance
#k=sqrt(n)
adf.test(log(india_data), alternative = "stationary", k=16)
adf.test(diff(log(india_data)), alternative = "stationary", k=16)
adf.test(diff(diff(log(india_data))), alternative = "stationary", k=16)
vax_ts = ts(india_data)
plot(vax_ts,xlab="Days since vaccinations became available (without first 100 days)", ylab="Vaccines", main="Time series of vaccines in India")
vax_ts = diff(diff(log(vax_ts)))
plot(vax_ts,main="Double-Differenced, Logged Vaccinations",xlab="Days since vaccinations became available (without first 100 days)",ylab="")
acf(vax_ts, main="Vaccinations ACF After Differencing")
pacf(vax_ts, na.action = na.pass, main="Vaccinations PACF After Differencing")
#MA1 is model chosen by looking at acf and pacf
vax.mod = arima(vax_ts, order=c(0,0,1))
vax.mod.res = vax.mod$residuals
plot(vax.mod.res)
vax.mod.res.acf = acf(vax.mod.res)
mean(vax.mod.res)
Lb.result = Box.test(vax.mod.res, lag = length(vax.mod.res.acf$acf)-1 , type = "Ljung-Box", fitdf = 1)
Lb.result$p.value
tsdiag(vax.mod,gof.lag=25)

#forecasting using test data
par(mfrow=c(1,1))
test_set = india_data[219:269]
training_set = india_data[1:219]
MA1 = arima((training_set),order=c(0,2,1))
MA1.pred = predict(MA1,n.ahead=50)
pred.x = MA1.pred$pred
plot(training_set,type='l', xlim=c(210,270), ylim=c(1100000000,1800000000), xlab="Days since vaccinations became available (without first 100 days)", ylab="Vaccines", main="Predicting Total Vaccines for Last 50 Days")
lines(pred.x,type='l',col='blue')
lines(seq(219,269),test_set,type='l',col='red')
pred.x.l = pred.x - 1.96*MA1.pred$se
pred.x.u = pred.x + 1.96*MA1.pred$se
lines(pred.x.l,col='green')
lines(pred.x.u,col='green')

#errors
differences = as.numeric(pred.x) - test_set
MAE2 = mean(abs(differences))
RMSE2 = sqrt(mean(differences^2))
#extremely large due to handling of massive numbers, but not bad when taken in to context

#forecasting future by 100 days
MA1_future = arima(india_data,order=c(0,2,1))
pred.future = predict(MA1_future,n.ahead=100)
plot(india_data, type='l', xlim=c(0,400), ylim=c(0,2500000000),xlab="Days since vaccinations became available (without first 100 days)", ylab="Vaccines", main="Forecasting Next 100 days of India's Vaccines")
lines(pred.future$pred,type='l', col='blue')
pred.future.l = pred.future$pred - 1.96*pred.future$se
pred.future.u = pred.future$pred + 1.96*pred.future$se
lines(pred.future.l,col='green')
lines(pred.future.u,col='green')
