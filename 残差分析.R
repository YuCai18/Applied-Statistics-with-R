#蔡煜
#24-12-10

#现有埃及卡拉马村庄每月记录儿童身高的数据，做一元线性回归。
rm(list=ls())
datas<-data.frame(age=18:29,height=c(76.1,77,78.1,78.2,78.8,79.7,79.9,81.1,81.2,81.8,82.8,83.5))
datas

fit <- lm(height~age,data=datas)
summary(fit) #截距和斜率的值都是显著的，且R^2也比较大
plot(datas)
abline(fit)

#绘制回归诊断图
par(mfrow = c(2,2))
plot(fit) 
#残差与拟合值图，二者越无关联越好，若有明显的曲线关系，则说明需要对线性回归模型加上高次项;
#残差的Q-Q图，看是否服从正态分布;
#标准化残差与拟合值图，也叫位置-尺度图纵坐标是标准化残差的平方根，残差越大，点的位置越高，用来判断模型残差是否等方差，若满足则水平线周围的点应随机分布;
#残差与杠杆图，虚线表示Cooks距离(每个数据点对回归线的影响力)等高线，从中可以鉴别出离群点、高杠杆点、强影响点。

#左上角这个图横轴代表yhat，纵轴代表误差项，最好在0左右不要有趋势。
#右上角这个是正态QQ图，是看模型是否符合正态分布
#左下角是帮助锁定异常值和拟合不好的值，以及可以用来判断方差齐性问题。如果点在比较方差大的地方,代表可能存在异常情况.
#右下角是残差的杠杆图，帮助锁定异常或离群点

#保存图片
setwd("D:/R/RWorkPlace/应用数理统计")
pdf("1.pdf")
par(mfrow = c(2,2))
plot(fit) 
dev.off()

attach(datas)
y.res = fit$residuals
y.fit = predict(fit)

par(mfrow = c(2,1))
plot(age,y.res)
abline(h = 0,lty = 3)
plot(y.fit,y.res)
abline(h = 0,lty = 3)

#rs = rstandard(fit) #对残差的标准化处理
#plot(y.fit,rs)

#qqnorm(y.res)
#qqline(y.res)

#检验是否符合正态分布
shapiro.test(y.res)

#检验残差是否独立，用p值检验是否要拒绝原假设
#若误差项不独立，那么回归模型的许多处理，包括误差项估计、假设检验等都将没有推导依据。
#由于残差是误差的合理估计，因此检验统计量通常是建立在残差的基础上。检验误差独立性的最常用方法，是对残差的一阶自相关性进行Durbin-Watson检验。
#DW接近于0，表示残差中存在正自相关:如果DW接近于4，表示残差中存在负自相关:如果DW接近于2，表示残差独立性。
library(car)
durbinWatsonTest(fit) #DW=2.40776

#离群点
#标准化残差值大于2或者小于-2的点可能是离群点。
outlierTest(fit) #Bonferroni p特别小，就说明要注意了。

#高杠杆点

##若观测点的帽子值大于帽子均值p/n的2或3倍，就可以认定为高杠杆值点（X决定，与Y无关）
hat.plot <- function(fit) {
  p <- length(coefficients(fit))
  n <- length(fitted(fit))
  plot(hatvalues(fit), main="Index Plot of Hat Values") 
  abline(h=c(2,3)*p/n, col="red", lty=2)
  identify(1:n, hatvalues(fit), names(hatvalues(fit)))
}
hat.plot(fit)

#强影响点
hatvalues(fit) #leverage
dffits(fit)
cooks.distance(fit)
covratio(fit)

influence.measures(fit)

cutoff = 4/(nrow(datas)-2) #4/(n-p-1) n是样本量大小
cutoff

plot(fit,which = 4,cook.levels = cutoff)
abline(h = cutoff,Ity=2,col="red") #绘制强影响点

#新的残差与杠杆图画法
library(car)
influencePlot(fit,id.method="identify",main="")
