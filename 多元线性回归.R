#蔡煜
#24-12-10

#多元线性回归

rm(list = ls())
setwd("D:/R/RWorkPlace/应用数理统计")
dat = read.csv("new.eg1.csv",header = T)
dat
attach(dat)
fit = lm(y~.,data = dat) #.代表省略写法,只有y是因变量,其他全是自变量

fit
summary(fit) #F-statistic: 8.278 on 4 and 22 DF,  p-value: 0.0003121这里的p值是对四个变量一起测的
anova(fit)

fit1 = lm(y~x1+x2+x3+x4)
fit2 = lm(y~x2+x3+x4)
fit3 = lm(y~x1+x3+x4)
fit4 = lm(y~x1+x2+x4)
fit5 = lm(y~x1+x2+x3)

anova(fit1,fit2)
anova(fit1,fit3)
#看p值是否显著,得到该变量是否有用的结论.

#自变量的筛选
#把显著的自变量留下,不显著的扔掉
#全局择优法
library(leaps)
ss = regsubsets(y~.,data = dat,nbest = 6)
summary(ss) #像矩阵一样,穷举了模型的可能性
summary(ss)$adjr2#穷举了模型对应的矫正的R^2值,越大模型越好,看这个即可
#比如:第十一个矫正的R^2值0.5456253是最大的,那么对应的模型参数应该是找summary(ss)的第十一行x2-x3-x4.

#逐步选择法
selection <- step(fit)
summary(selection) #告诉我们应该选x2-x3-x4

#回归诊断图
par(mfrow=c(2,2))
plot(fit2)

#这部分可以参考残差分析,都是同理的
#残差与拟合值图，二者越无关联越好，若有明显的曲线关系，则说明需要对线性回归模型加上高次项;
#残差的Q-Q图，看是否服从正态分布;
#标准化残差与拟合值图，也叫位置-尺度图纵坐标是标准化残差的平方根，残差越大，点的位置越高，用来判断模型残差是否等方差，若满足则水平线周围的点应随机分布;
#残差与杠杆图，虚线表示Cooks距离(每个数据点对回归线的影响力)等高线，从中可以鉴别出离群点、高杠杆点、强影响点。

#左上角这个图横轴代表yhat，纵轴代表误差项，最好在0左右不要有趋势。
#右上角这个是正态QQ图，是看模型是否符合正态分布
#左下角是帮助锁定异常值和拟合不好的值，以及可以用来判断方差齐性问题。如果点在比较方差大的地方,代表可能存在异常情况.
#右下角是残差的杠杆图，帮助锁定异常或离群点

#检验是否符合正态分布
res = fit2$residuals
shapiro.test(res)

#检验残差是否独立，用p值检验是否要拒绝原假设
#若误差项不独立，那么回归模型的许多处理，包括误差项估计、假设检验等都将没有推导依据。
#由于残差是误差的合理估计，因此检验统计量通常是建立在残差的基础上。检验误差独立性的最常用方法，是对残差的一阶自相关性进行Durbin-Watson检验。
#DW接近于0，表示残差中存在正自相关:如果DW接近于4，表示残差中存在负自相关:如果DW接近于2，表示残差独立性。
library(car)
durbinWatsonTest(fit2) #DW=2.40776

#离群点
#标准化残差值大于2或者小于-2的点可能是离群点。
outlierTest(fit2) #Bonferroni p特别小，就说明要注意了。

#高杠杆点

##若观测点的帽子值大于帽子均值p/n的2或3倍，就可以认定为高杠杆值点（X决定，与Y无关）
hat.plot <- function(fit2) {
  p <- length(coefficients(fit2))
  n <- length(fitted(fit2))
  plot(hatvalues(fit2), main="Index Plot of Hat Values") 
  abline(h=c(2,3)*p/n, col="red", lty=2)
  identify(1:n, hatvalues(fit2), names(hatvalues(fit2)))
}
hat.plot(fit2)

#强影响点
hatvalues(fit2) #leverage
dffits(fit2)
cooks.distance(fit2)
covratio(fit2)

influence.measures(fit2)

cutoff = 4/(nrow(datas)-2) #4/(n-p-1) n是样本量大小
cutoff

plot(fit2,which = 4,cook.levels = cutoff)
abline(h = cutoff,Ity=2,col="red") #绘制强影响点

#新的残差与杠杆图画法
library(car)
influencePlot(fit2,id.method="identify",main="")


