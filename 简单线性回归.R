#蔡煜
#24-12-09

x <- c(60,62,64,65,66,67,68,70,72,74)
y <- c(63.6,65.2,66,65.5,66.9,67.1,67.4,68.3,70.1,70)

dat <- data.frame(x=x,y=y)
plot(dat)

fit = lm(y~x)
summary(fit)

coefficients(fit) #获取参数

anova(fit) #得到回归分析表

#求拒绝域
n = length(x)
alpha = 0.05
qt(1-alpha/2,n-2)
#t 值是14.0 > 2.306所以拒绝原假设，认为回归效果是显著的
#F 值可以验证自变量x是否显著

confint(fit,parm="x",level=0.95) #置信度是95 %,计算置信区间
#绘制回归的拟合线
plot(dat)
abline(fit)

#导入新数据集
rm(list=ls())
setwd("D:/R/RWorkPlace/应用数理统计")
dat <- read.csv("eg-1.csv",header=T)
attach(dat)
fit <- lm(y~x)
summary(fit)

#预测（计算x0=5的时候，E(y0)的区间估计和y0的预测区间）
newdata = data.frame(x=5)
predict(fit,newdata,interval = "confidence") #估计区间

predict(fit,newdata,interval = "predict") #预测区间

#绘图
newdata1 <- data.frame(x=seq(0,52,1))
conf <- predict(fit, newdata1, interval="confidence")  #估计区间
pre <- predict(fit, newdata1, interval="predict") #预测区间
xx <- seq(0,52,1)
plot(dat)
lines(xx,conf[,1])
lines(xx,conf[,2],lty=2)
lines(xx,conf[,3],lty=2)
lines(xx,pre[,2],lty=3,col=2)
lines(xx,pre[,3],lty=3,col=2)
legend(35,5,c("confidence","prediction"),lty=c(2,3),col=c(1,2))


#合金钢强度y和钢材中碳含量x有密切关系
rm(list=ls())
setwd("D:/R/RWorkPlace/应用数理统计")
dat <- read.csv("eg-2.csv",header=T)
#（1）画出散点图
attach(dat)
plot(x,y)
#（2）设μ(x)=alpha+betax,求alpha，beta的估计值
fit <- lm(y~x)
summary(fit)
#alpha = 35.451,beta=92.641 
#（3）求误差方差的估计，画出残差图 residual = y - y.fit
y.res = fit$residuals
plot(x,y.res)
#残差图的第二种画法,两种画法画出来的散点都一样
y.fit = predict(fit)
plot(y.fit,y.res)

plot(x,y)
abline(fit)
#从残差图中我们可以发现，模型中应该包含二次型比较好，所以加入二次项。
#（4）检验回归系数beta是否为0，（alpha=0.05）
confint(fit)
predict(fit,data.frame(x=0.06),interval="confidence")
predict(fit,data.frame(x=0.06),interval="predict")
dat #x2是x的平方项

#第一次修改模型
fit2 = lm(y~x+x2)
summary(fit2)
anova(fit2) #得到x这项的p值是0.29代表不显著应该把它再剔除出去。

#第二次修改模型
fit3 = lm(y~x2) #等价写法是fit3 = lm(y~I(x^2))
summary(fit3)
anova(fit3) #得到x2显著则保留
