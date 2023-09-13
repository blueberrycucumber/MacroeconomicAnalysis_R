install.packages(c("vars","MTS","VAR.etp","openxlsx"))
library(tseries)
library("zoo")
library("vars")
library(readxl)
library(openxlsx)
data=read.xlsx("SSINCOME.xlsx")
head(data)

ss<-ts(data$ss, frequency=12, start=c(1990,1))
ts.plot(ss)
lnss=log(ss)
ss2<-decompose(lnss) 
ss3<-lnss-ss2$seasonal 
plot(ss3)

library(urca)
library(tseries)
library(vars)
adf.test(ss3)
summary(ur.df(diff(ss3), type = "trend",selectlags = "AIC"))
#(1)SARIMA建模
#par(mfrow=c(1,2))
acf(diff(ss3),lag=100)
pacf(diff(ss3),lag=20)
library(forecast)
tsdisplay(diff(ss3),xlab="month",ylab="ssratio")
fit1 <- Arima(ss3, order=c(0,1,1),seasonal=c(0,1,1))
fit2 <- Arima(ss3, order=c(1,1,1),seasonal=c(1,1,1))
arima1<-auto.arima(ss3,trace=T)
library(TTR)
AIC(fit1,fit2)
BIC(fit1,fit2)#根据AIC和BIC准则，我们选择SARIMA(0,1,1)(0,1,1)
tsdisplay(residuals(fit1))
plot(forecast(Arima(ss3,order=c(0,1,1),seasonal=c(0,1,1)),h=12))

#波动率模型建立
#1. ARCH效应检验
#上述建立模型后，对残差进行ARCH效应检验。Ljung-Box统计量Q ( m ) Q(m)Q(m)对残差序列进行自相关检验。原假设是序列不存在自相关，在残差的平方序列中可以检验条件异方差。
install.packages("MTS")
library(rugarch) 
library(devtools) 
library(FinTS) 
library(TSA)
Box.test(residuals(fit1),type="Ljung-Box",lag=20)
Box.test(residuals(fit1)^2,type="Ljung-Box",lag=20)#拒绝原假设，存在ARCH效应
rr <-residuals(fit1)
head(rr)
par(mfrow=c(2,2)) 
acf(rr,lag=20) 
pacf(rr,lag=20) 
acf(rr^2,lag=20) 
pacf(rr^2,lag=20)
par(mfrow=c(1,1))
#定阶后对两种情况进行建模（1 1）+（2 2）
xfit1=garch(rr, order = c(1,1))
xfit2=garch(rr, order = c(2,2))
summary(xfit1)
summary(xfit2)
#选出最优模型CARCH(1,1)
xpred=predict(xfit1)
xpred
plot(xpred)








