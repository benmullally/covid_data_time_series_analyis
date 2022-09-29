Thedata = read.csv(file = "covidata.csv")
library(dplyr)
install.packages("tseries")
library(tseries)


#####italy vaccinations
italy = filter(Thedata,location == "Italy")
italyvac = ts(select(italy,total_vaccinations)) #time series the data
italynona = na.omit(italyvac)   #take out all zero values(before vaccine was available)
italynona = italynona
italynona = ts(italynona)
par(mfrow= c(1:2))
plot(italynona,xlab = "Days since vaccine became available",ylab = "Number of vaccines given", main = "Number of covid vaccinations in Italy")
acf(italynona,main = "ACF before log+differencing")     #looking at the data
pacf(italynona,lag.max= 100)



slogG = log(italynona)  #log ts to make graph easier
plot(logG)             
diff1 = diff(logG) #do diff to see the small differences in the logged time series
plot(diff1,xlab = "Days since vaccine became available",ylab ="")        #looking at the diffed data
par(mfrow=c(1,2))
acf(diff1,main = "ACF after log+differencing")
pacf(diff1,main = "PACF after log+differencing")


AR3diff = arima(diff1,c(3,0,0))   #decreasing acf from lag 3 so try AR3 model



diff1res = AR3diff$residuals   #getting residuals of AR3 data
tsdiag(AR3diff,lag.max = 20)    #high LB pvalues
plot(diff1res)     #looking at residuals
acf(diff1res)
pacf(diff1res)
mean(diff1res)     #very close to 0


m = 50
z =length(italynona)-m



ARpred = arima(italynona[1:z],order = c(3,1,0))

diffpred = predict(ARpred, n.ahead = m)      #predict last 50 days 
xf1 = diffpred$pred
xf1.l = xf1 - 1.96*diffpred$se #confidence intervals
xf1.u = xf1 + 1.96*diffpred$se
plot(1:length(italynona),italynona[1:length(italynona)],ylim = c(80000000,140000000),xlim = c(300,405),type = "l",xlab = "Days since vaccine became available",ylab = "Number of vaccines given",main = "Original time series with 50 days predicted")  #plot aginst actual values
lines(xf1,type = "l",col = "red")
lines(xf1.u, type="l", col="green")
lines(xf1.l, type="l", col="green")


d1 = vector() #working out errors for prediction 
last50val = italynona[356:405]
for (i in 1:50){
  d1[i] = xf1[i]-italynona[355+i]
  
}
MAE1 = mean(abs(d1))  #mean absolute error.
RMSE1 = sqrt(mean(d1^2))   #real mean squared error.



#forecasting
forecast = length(italynona)+75      #total length for graph
ARfore = arima(italynona, order = c(3,1,0))
forepred = predict(ARfore,n.ahead = 75)  #forecast 
xfore1 = forepred$pred 


plot(1:length(italynona),italynona,type = "l",xlim = c(0,500),ylim = c(0,150000000),xlab = "Days since vaccine became available",ylab = "Number of vaccines given",main = "Original time series+75 days forecasted")
lines(xfore1,type = "l", col = "red")










































