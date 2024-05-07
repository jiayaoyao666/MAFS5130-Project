library(tseries)
library(fGarch)
library(timeSeries)
library(forecast)
library(caret) 
library(stats)

Daily_BYD = read.csv("E:/沈睿琳/研/研一下/时间序列/project/BYD.csv", header = T)
daily_BYD = as.data.frame(Daily_BYD)

# use the log_return
#options(max.print = 2000)
train_BYD = daily_BYD[c(1:1453),4] # training set: 2012-2017 year
#train_BYD

test_BYD = daily_BYD[c(1454:1694),4] # testing set: 2018 year
#test_BYD

# use percentage log_return
log_BYD = diff(train_BYD)*100
#log_BYD

summary(log_BYD)
acf(log_BYD, lag=12)
pacf(log_BYD, lag=12)

adf.test(log_BYD) # ADF test


#### ARIMA Model####

#model_arima <- auto.arima(log_BYD, lambda = "auto") 
#model_arima
#Box.test(model_arima$residuals,lag=12,type='Ljung')
#problem: data not all positive
#需手动确认,自动给出的是(1,0,1),其结果与手动给出的（1,0,1）不一样

# best ar model
ar19=arima(log_BYD,order=c(19,0,0)) # aic = 4924.28
ar19
Box.test(ar19$residuals,lag=24,type='Ljung') #lag必须比参数数量大
pv=1-pchisq(25.598,5) #24-19=5
pv # 0.0001067668
#p值太小,舍去

# best ma model
# ma model全都不行
ma1=arima(log_BYD,order=c(0,0,1)) #aic = 4875.73
ma1
# 参数ma1=1,舍去

# best arma model
# 要测太多，已测完ar<=3，p值都很小，没必要再往下试了 下面给两个在p值测试前可行的例子
arma2_3=arima(log_BYD,order=c(2,0,3)) #aic = 4878.96
arma2_3
Box.test(arma2_3$residuals,lag=12,type='Ljung')
pv=1-pchisq(36.923,7) #12-5=7
pv # 4.850844e-06
#p值太小,舍去

arma3_4=arima(log_BYD,order=c(3,0,4),fixed=c(NA,NA,NA,NA,0,NA,NA,NA)) #aic = 4869.6
arma3_4
Box.test(arma3_4$residuals,lag=12,type='Ljung')
pv=1-pchisq(22.449,5) #12-7=5
pv # 0.0004300652
#p值太小,舍去

# best arima model
#在测试的时候发现只有ma=0是可行的，固定q=0, 然后d越大，aic越大，因此没必要往后试了
arima1=arima(log_BYD,order=c(2,1,0)) #aic = 6197.88
arima1

arima2=arima(log_BYD,order=c(1,2,0)) #aic = 7984.35
arima2

###综上，arima model全被舍弃


#### GARCH(1,1) Model Forecasting ####

m=garchFit(log_BYD~garch(1,1),data=log_BYD,include.mean = FALSE,trace=F)
summary(m)
predict(m, n.ahead = 241, trace = FALSE, mse = c("cond","uncond"),
        plot=TRUE, nx=NULL, crit_val=NULL, conf=NULL)

# get sigma_t
forecast <- predict(m, n.ahead = 241, trace = FALSE, mse = c("cond","uncond"),
        plot=TRUE, nx=NULL, crit_val=NULL, conf=NULL)
sigma_t2 <- forecast$meanError
sigma_t <- sqrt(as.numeric(sigma_t2)) 
sigma_t 

#simulation 
data=function(n,alpha){
  m=1454
  noise=rnorm(n+m)
  a=numeric(m+n)
  h=numeric(m+n)
  r=numeric(m+n)
  for (i in 2:(m+n))
    h[i]=alpha[1]+(alpha[2]*noise[i-1]*noise[i-1]+alpha[3])*h[i-1]
  for (i in 2:(m+n))
    a[i]=sqrt(h[i])*noise[i]
  for (i in 2:(m+n))
    r[i]=alpha[4]*r[i-1]+a[i]
  return(r[-(1:m)])
}

n=241
alpha=c(0.0558,0.07474,0.92,0.0)
sample=data(n, alpha)
plot(sample,main=" ",ylab="",xlab="",type="l")

p0=64.83
p_pre <- numeric(242)
p_pre[1] <- p0

for (i in 2:242) {
  p_pre[i] <- p_pre[i-1] * exp((sample[i-1])/100)
}

Daily_BYD = read.csv("E:/沈睿琳/研/研一下/时间序列/project/BYD.csv", header = T)
daily_BYD = as.data.frame(Daily_BYD)
p_true = daily_BYD[c(1454:1694),2]

plot(p_true, type = "l", col = "blue", lwd = 2, ylim = c(min(p_true, p_pre), max(p_true, p_pre)),
     main = "True Price vs. Predicted Price", xlab = "Days", ylab = "Price")
lines(p_pre, col = "red", lwd = 2)


#### ARMA(2,2)-GARCH(1,1) Model Forecasting ####

m=garchFit(log_BYD~arma(2,2)+garch(1,1),data=log_BYD,include.mean = FALSE,trace=F)
summary(m)
predict(m, n.ahead = 241, trace = FALSE, mse = c("cond","uncond"),
        plot=TRUE, nx=NULL, crit_val=NULL, conf=NULL)
