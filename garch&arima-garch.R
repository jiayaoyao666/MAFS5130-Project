# Project
# price
price=BYD[,2]
price <- as.numeric(price$BYD)
plot(price,main=" ",ylab="",xlab="",type="l")

# return
rt_byd=BYD[,3]
rt_byd <- as.numeric(rt_byd$return)
plot(rt_byd,main=" ",ylab="",xlab="",type="l")
Box.test(rt_byd,lag=12,type="Ljung")
lr=100*log(1+rt_byd)
plot(lr,main=" ",ylab="",xlab="",type="l")
Box.test(lr,lag=12,type="Ljung")

# diff
plot(lr,main=" ",ylab="",xlab="",type="l")

# Serial correlation
library(tseries)
library(forecast)
adf.test(lr)
acf(lr)
pacf(lr,30,ylim=c(-0.5,0.5),main="")
# p = 0.01, reject the Null hypothesis, the series is stationary.

# ARCH effect
library(FinTS)
ArchTest(lr, lags=12)
plot(lr^2,main=" ",ylab="",xlab="",type="l")
acf(lr^2,30,ylim=c(-0.5,0.5),main="")
pacf(lr^2,30,ylim=c(-0.5,0.5),main="")
Box.test(lr^2,lag=12,type="Ljung")
# p-value < 2.2e-16, reject the Null Hypothesis, it exists arch effect.

#fit ARIMA model
auto.arima(lr)# we ensure the arima(2,0,0) because it has the lowest aic.
m1=arima(lr,order=c(2,0,0))
m1
stresi=residuals(m1,standardize=T)
plot(stresi,xlab=" ",ylab=" ",main="Time plot of daily residual for BYD stock",type="l",ylim=c(-15,15))
Box.test(stresi,10,type="Ljung")
Box.test(stresi,20,type="Ljung")
Box.test(stresi^2,10,type="Ljung")
Box.test(stresi^2,20,type="Ljung")
# p-value < 2.2e-16, reject the Null Hypothesis, it exists arch effect.
# we consider some forms of GARCH model because we have arch effect.

# eacf
library(TSA)
eacf(abs(lr))
eacf(lr^2)
t.test(lr)

# GARCH model
library(fGarch)
m3=garchFit(lr~arma(0,0)+garch(1,1),data=lr,include.mean = FALSE,trace=F)
summary(m3)
Res=residuals(m3,standardize=T)
Box.test(Res,10,type="Ljung")
Box.test(Res,20,type="Ljung")
Box.test(Res^2,10,type="Ljung")
Box.test(Res^2,20,type="Ljung")

# ARMA-GARCH model
m3=garchFit(lr~arma(2,2)+garch(1,1),data=lr,include.mean = FALSE,trace=F)
summary(m3)


lr_diff=diff(lr)
m3=garchFit(lr_diff~arma(1,1)+garch(1,1),data=lr_diff,include.mean = FALSE,trace=F)
summary(m3)

lr_diff=diff(lr)
m3=garchFit(lr_diff~arma(0,1)+garch(1,2),data=lr_diff,include.mean = FALSE,trace=F)
summary(m3)

#Volatility Forecast
lr_train<-lr[1:1453]
lr_test<-lr[1454:1694]

library(rugarch)
spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                   mean.model = list(armaOrder = c(2, 2), include.mean = FALSE))
m1 <- ugarchfit(spec = spec, data = lr_train)
fore <- ugarchforecast(m1, n.ahead = 241)
print(fore)
plot(lr_train, type = "l", ylab = "lr_train", main = "lr_train and Forecast", xlim = c(0, 1693))
lines(x = 1454:1694, y = lr_test, col = "grey", lty = 1)
lines(x = 1454:1694, y = fore@forecast$seriesFor, col = "red", lty = 1)
lines(x = 1454:1694, y = fore@forecast$seriesFor + 1.96 * fore@forecast$sigmaFor, col = "blue", lty = 1)
lines(x = 1454:1694, y = fore@forecast$seriesFor - 1.96 * fore@forecast$sigmaFor, col = "green", lty = 1)

# price prediction
lr_pred<- fore@forecast$seriesFor
lr_pred <- as.numeric(lr_pred)
simple_rt<-exp(lr_pred/10)-1

initial_price <- price[1453]
price_true <-price[1454:1694]
cu_rt <- cumprod(1 + simple_rt)
price_list <- initial_price * cu_rt

plot(price_true, col = "blue",type = "l", xlab = "Days",ylab = "Price", main = "True Price vs.Predicted Price", xlim = c(0, 250),lwd = 2)
lines(price_list, col = "red", lty = 1,lwd = 2)
