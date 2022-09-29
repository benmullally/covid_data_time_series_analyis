# install.packages("dplyer")
# install.packages("tseries")
# install.packages("forecast")
# 
library(dplyr)
library(tseries)
#library(forecast)

# read data
#data <- read.csv("owid-covid-data.csv")

# filter italy total cases
italy <- filter(data, data$location == "Italy")

# casesItaly <- italy$total_cases
casesItaly <- italy$total_cases


plot(ts(casesItaly), ylab="Total Cases", xlab="Time (days)", main="Graph of Total Cases of Covid in Italy", col="blue")
# create a time series object
italy.ts = ts(casesItaly)
acf(italy.ts, main = "ACF graph of Total Cases") # acf gradually decreasing but no cut off

# log it to stabilize variance, can do this since we have positive values
litaly.ts = log(italy.ts)

# dicky fuller test to get diff of 2
adf.test(litaly.ts, alternative = "stationary", k=28)
d_litaly.ts = diff(litaly.ts)

adf.test(d_litaly.ts, alternative = "stationary", k=28)
d2_litaly.ts = diff(diff(litaly.ts))

adf.test(d2_litaly.ts, alternative = "stationary", k=28)

statItaly.ts = d2_litaly.ts # needed 2 


#Analysing to specify a suitable model
plot(statItaly.ts)
mean(statItaly.ts)

par(mfrow=c(1,2))
acf(statItaly.ts, main = "ACF Total Cases (Logged and Differenced)") 
pacf(statItaly.ts, main = "PACF Total Cases (Logged and Differenced)") 

#Our model
mod1 <- arima(litaly.ts, c(13,2,0))
tsdiag(mod1,gof.lag = 100)
pacf(mod1$residuals)
mean(mod1$residuals)

#Ljung Box Test
mod1.acf <- acf(mod1$residuals)
Lb.result = Box.test(mod1$residuals, lag = length(mod1.acf$acf)-1,
                     type = "Ljung-Box", fitdf = 15)
Lb.result$p.value

# prediction using tesitng data
testing_data <- casesItaly[1:639]
mod1 <- arima(testing_data, c(13,2,0))
mod1.f = predict(mod1, n.ahead = 100)


# 95% interval
xf1 = mod1.f$pred
xf.l = mod1.f$pred - 1.96*mod1.f$se
xf.u = mod1.f$pred + 1.96*mod1.f$se


n = length(testing_data)
m= n+100

M = (n):(n+m) # index for forecasting region


#plot(1:639,italy.ts[1:639], type="l",col="blue", xlab="t", ylab ="Total cases")
plot(1:639,italy.ts[1:639], xlim = c(500,900), ylim = c(100000,17000000), type="l",col="blue", xlab="t", ylab ="Total cases", main="Predicted data using an ARIMA(13,2,0) model")

lines(640:739, italy$total_cases[640:739], type="l",col="red", lty=2)
lines(640:739, xf1, type="l", col="green")

#se
lines(640:739, xf.u, type="l", col="magenta", lty=3)
lines(640:739, xf.l, type="l", col="magenta", lty=3)
legend(x = "topleft",           # Position
       legend = c("Actual data", "Actual data", "Predicted from model", "95% confidence intervals"),  # Legend texts
       lty = c(1, 2, 1, 3, 3),           # Line types
       col = c("blue", "red", "green", "magenta"),           # Line colors
       lwd = 2)  

#errors

d1 <- mod1.f$pred - italy$total_cases[640:739]
MAE = mean(abs(d1))
RMSE = sqrt(mean(d1^2))

MAE
RMSE

#forecast
mod2 <- arima(casesItaly, c(13,2,0))
mod2.f = predict(mod2, n.ahead = 100)

xf2 = mod2.f$pred
xf2.l = mod2.f$pred - 1.96*mod2.f$se
xf2.u = mod2.f$pred + 1.96*mod2.f$se


par(mfrow=c(1,2))
plot(1:(length(casesItaly)-100),casesItaly[1:639], xlim = c(500,900), ylim = c(100000,17000000), type="l",col="blue", xlab="t", ylab ="Total cases", main="Predicted data using an ARIMA(13,2,0) model")
lines(640:739, italy$total_cases[640:739], type="l",col="red", lty=2)
lines(640:739, xf1, type="l", col="green")
lines(640:739, xf.u, type="l", col="magenta", lty=3)
lines(640:739, xf.l, type="l", col="magenta", lty=3)
legend(x = "topleft",           # Position
       legend = c("Actual data", "Actual data", "Predicted from model", "95% confidence intervals"),  # Legend texts
       lty = c(1, 2, 1, 3, 3),           # Line types
       col = c("blue", "red", "green", "magenta"),           # Line colors
       lwd = 2,
       cex=0.60) 
plot(1:length(casesItaly),casesItaly, xlim = c(500,900), ylim = c(100000,17000000), type="l",col="blue", xlab="t", ylab ="Total cases", main="Predicted data using an ARIMA(13,2,0) model")

lines(740:839, xf2, type="l", col="green")
lines(740:839, xf2.u, type="l", col="magenta", lty=3)
lines(740:839, xf2.l, type="l", col="magenta", lty=3)
legend(x = "topleft",           # Position
       legend = c("Actual data", "Predicted from model", "95% confidence intervals"),  # Legend texts
       lty = c(1, 1, 3, 3),           # Line types
       col = c("blue", "green", "magenta"),           # Line colors
       lwd = 2,
       cex=0.60)
