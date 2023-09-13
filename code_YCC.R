install.packages(c("vars","MTS","VAR.etp"))
library(tseries)
library("zoo")
library("vars")
setwd('C:/Users/Lenovo')
#中间变量（RC\BC） 目标变量（CPI、ER)两个目标变量所以构建两个var模型
#先绘图查看数据特征，如果有指数特征则要取对数
data=read.csv("YCCdata.csv")
head(data)

RC<-ts(data$RC, frequency=12, start=c(2016,1)) 
BC<-ts(data$BC, frequency=12, start=c(2016,1))
CPI<-ts(data$CPI, frequency=12, start=c(2016,1))
ER<-ts(data$ER, frequency=12, start=c(2016,1))
par(mfcol=c(2,2))
ts.plot(RC)
ts.plot(BC)
ts.plot(CPI)
ts.plot(ER)

#对时间序列数据进行分解
RC2<-decompose(RC) 
BC2<-decompose(BC)
CPI2<-decompose(CPI)
ER2<-decompose(ER) 

#去除数据的季节性因素 
RC3<-RC-RC2$seasonal 
BC3<-BC-BC2$seasonal
CPI3<-CPI-CPI2$seasonal
ER3<-ER-ER2$seasonal

#输出去除季节性因素后的图像3 
par(mfcol=c(2,2))
plot(RC3) 
plot(BC3)
plot(CPI3)
plot(ER3)

#分别对BC、ER做对数处理
lnBC=log(BC3)
lnER=log(ER3)

#对数据进行平稳性检验
#diff(x)就是论文中的D(x)部分，表示差分后的数据
adf.test(lnBC)
adf.test(diff(lnBC))
adf.test(RC3)
adf.test(diff(RC3))
adf.test(CPI3)
adf.test(diff(CPI3))
adf.test(lnER)
adf.test(diff(lnER))
#经过一阶差分之后都实现了平稳，因此变量都是一阶单整

data1=data.frame(lnBC,RC3,CPI3)
data2=data.frame(lnBC,RC3,lnER)

#进行johansen检验
#CPI
a=ca.jo(data1,type = c('eigen'),K=3)
summary(a)
#lnER
b=ca.jo(data2,type = c('eigen'),K=3)
summary(b)
#当r=0时，大于临界值说明各变量之间存在协整关系
#在这里即说明各指标变量之间存在协整关系，可以直接建立VAR模型

#var模型初步定阶
#CPI
VARselect(data1,lag.max = 5,type='const')
#lnER
VARselect(data2,lag.max = 5,type='const')
#采用AIC和SC准则，确定阶数为2阶

#构建模型
#CPI
var1 <- VAR(data1, p=2, type="const")
normality.test(var1, multivariate.only = TRUE) 
#lnER
var2 <- VAR(data2, p=2, type="const")
normality.test(var2, multivariate.only = TRUE) 

#残差相关性检验
serial.test(var1,lags.pt = 10, type="PT.asymptotic")

serial.test(var2,lags.pt = 10, type="PT.asymptotic")
#通过serial.test函数检验模型残差的序列相关性，发现p都大于0.05，不能拒绝残差序列不相关的假设

#格兰杰因果检验
lnBC4=diff(lnBC)
RC4=diff(RC3)
CPI4=diff(CPI3)
lnER4=diff(lnER)
#目标变量为CPI
grangertest(lnBC~CPI3,order=2)
grangertest(RC4~CPI4,order=2)
grangertest(lnBC+RC3~CPI3,order=2)
#目标变量为lnER
grangertest(lnER~lnBC,order=2)
grangertest(lnER4~RC4,order=2)
grangertest(lnBC4+RC4~lnER4,order=2)

#对滞后两阶的var模型进行稳定性检验
#图中绘出两条临界线，如果累积和超出了这两条临界线，则说明参数不具有稳定性
sta1 = stability(var1, type = c("OLS-CUSUM"), h = 0.15, dynamic = FALSE, rescale = TRUE)
plot(sta1)  

sta2 = stability(var2, type = c("OLS-CUSUM"), h = 0.15, dynamic = FALSE, rescale = TRUE)
plot(sta2)  ##结果稳健

#脉冲响应分析，运用脉冲响应函数分析模型变量之间的动态影响

var1.irf20<-irf(var1, n.ahead = 20)
plot(var1.irf20)

var2.irf20<-irf(var2, n.ahead = 20)
plot(var2.irf20)

