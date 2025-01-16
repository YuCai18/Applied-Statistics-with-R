#蔡煜
#24-12-09

#常用的绘图代码

x <- c(159, 280, 101, 212, 224, 379, 179, 264, 222, 362, 168, 250, 149, 260, 485, 170)
test <- t.test(x,mu=225,alternative="two.sided") #双侧

n <- length(x)
t.q <- qt(1-alpha,df=n-1) #计算t值
t.q

## 画出拒绝域,红的代表观测值，黑的代表拒绝域
t <- 0.6685
xx <- seq(-4,4,0.01)
plot(xx, dt(xx,df=n-1), type="l", lwd=2, col="red",
     xlim=c(-4, 4), ylim=c(-0.01, 0.4),
     ylab = "Density",
     main = "Rejection Region")
     abline(v=t.q)
     abline(v=t,lty=2,col=2) #红的
abline(h=0, v=0) #原点附近
# 画出拒绝域
xx1 <- seq(t.q, 4, by=0.01)
polygon(c(xx1, t.q), c(dt(xx1,df=n-1), dt(4,df=n-1)), col="red")
text(t.q, -0.015, expression(t[alpha]), adj=0.4, cex=1.1)
text(2.2, 0.02, expression(alpha), adj=0.5, cex=1)

#画出p值对应的区域
pval <- 1-pt(t,df=n-1)
pval
plot(xx, dt(xx,df=n-1), type="l", lwd=2, col="red",
     xlim=c(-4, 4), ylim=c(-0.01, 0.4),
     ylab = "Density",
     main = "P-Value")
abline(h=0, v=0)
xx2 <- seq(abs(t), 4, by=0.01)
polygon(c(xx2, abs(t)), c(dt(xx2,df=n-1), dt(4,df=n-1)), col="blue")
text(t, -0.015, expression(t), adj=0.4, cex=1.1)
text(2, 0.02, expression(pval), adj=0.5, cex=1.2)
abline(v=t.q,lty=2)

#画QQ图
qqnorm(x)
qqline(x)

#画直方图
hist(x,freq=F,breaks=10)
lines(density(x))

#双边假设检验
#36名学生的成绩服从正态分布，平均成绩66.5，标准差15。
#问：在显著性水平0.05下，是否可以认为全体考生的平均成绩为70分？

rm(list=ls()) #删除环境中的数字,清空缓存
xbar <- 66.5
n <- 36
s <- 15
mu0 <- 70
t <- (xbar - mu0)/(s/sqrt(n))
t

alpha = 0.05
t.q = qt(1-alpha/2,df=n-1)
t.q #2.0301
#t = -1.4 q = 2.030 t < q => 接受原假设
## 画出双边检验的拒绝域
x <- seq(-4,4,0.01)
plot(x, dnorm(x), type="l", lwd=2, col="red",
     xlim=c(-4, 4), ylim=c(-0.01, 0.4),
     ylab = "Density",
     main = "Rejection Region")
abline(v=t)
abline(h=0, v=0)
z <- qt(1-0.05/2,df=n-1)
xx <- seq(z, 4, by=0.01)
polygon(c(xx, z), c(dt(xx,df=n-1), dt(4,df=n-1)), col="red")
xx <- seq(-4, -z, by=0.01)
polygon(c(xx, -z), c(dt(xx,df=n-1), dt(-4,df=n-1)), col="red")
text(z, -0.015, expression(Z[alpha/2]), adj=0.4, cex=1.1)
text(2.2, 0.02, expression(alpha/2), adj=0.5, cex=1)

## 画出p值
pval <- 2*pt(t,df=n-1)
pval
## [1] 0.1703156
plot(x, dnorm(x), type="l", lwd=2, col="red",
     xlim=c(-4, 4), ylim=c(-0.01, 0.4),
     ylab = "Density",
     main = "P-Value")
abline(h=0, v=0)
xx <- seq(abs(t), 4, by=0.01)
polygon(c(xx, abs(t)), c(dt(xx,df=n-1), dt(4,df=n-1)), col="blue")
xx <- seq(-4, -abs(t), by=0.01)
polygon(c(xx, -abs(t)), c(dt(xx,df=n-1), dt(-4,df=n-1)), col="blue")
text(t, -0.015, expression(t), adj=0.4, cex=1.1)
text(-2, 0.02, expression(pval/2), adj=0.5, cex=1.2)
abline(v=t.q)
abline(v=-t.q)