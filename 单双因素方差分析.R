#蔡煜
#24-12-09
#方差分析的时候，假设检验统一都是单边检验
#单因素方差分析 one-way ANOVA
#设有5种治疗荨麻疹的药，要比较它们的疗效。假设将30个病人分成5组每组6人，
#令同组病人使用一种药，并记录病人从使用药物开始到痊愈所需时间，得到下面的记录:(alpha = 0.05)
#问：所有药物的效果都没有差别
#H0:mu1=mu2=mu3=mu4=mu5 H1:mu1,mu2,mu3,mu4,mu5不全相等

x <- c(5,8,7,7,10,8,4,6,6,3,5,6,6,4,4,5,4,3,7,4,6,6,3,5,9,3,5,7,7,6)

group <- c(rep("1",6),rep("2",6),rep("3",6),rep("4",6),rep("5",6))

dat = data.frame(x = x,group=group)
dat
attach(dat) #
table(group) #每个group计数
fit = aov(x~group,data = dat) 
summary(fit)#生成方差分析表 F的比值=MSA/MSE=9.117/2.34=3.896

#by group
aggregate(x,by=list(group),mean)#求组内平均值
aggregate(x,by=list(group),sd)#求组内标准差
mean(x)
sum(x^2)
n = length(x)
k = 5 #组数
#求F的边界值，F是能解释的和不能解释的比值
qf(1-0.05,k-1,n-k) #2.75871 而3.896 > 2.758,则拒绝原假设，药物有差别
#1-pf(3.9,k-1,n-k) #0.0136 计算p值

#方差分析的前提是独立性、正态性和方差齐性
#测试方差齐性
library(car)
leveneTest(x~as.factor(group),data = dat) #然后看p值

#多重比较:Tukey方法
library(gplots)
plotmeans(x~group,xlab = "type",ylab = "days")
#第一组和第三组差距明显，他们直接没有重叠的部分，可以得到他们之间具有显著的区别


#双因素方差分析 two way anova 不考虑交互作用
y <- c(258, 279, 242, 302, 314, 336, 321, 318, 327)
A <- c(rep("A1",3),rep("A2",3),rep("A3",3))
B <- rep(c("B1","B2","B3"),3)
A
B
dat <- data.frame(y=y,A=A,B=B)
dat
#如果数据框dat中有列名为y的列，那么在attach(dat)之后，你可以直接使用y来访问这一列的数据，而不需要写dat$y。
attach(dat) #附加之后，你可以直接使用数据框中的列名作为变量名来访问数据
table(A,B)

fit <- aov(y~A+B, data=dat)
fit
summary(fit)
#因素A的P值是0.019<0.05,所以因素A不显著,B因素显著
table(A,B)

#双因素方差分析 two way anova 考虑交互作用
#为了比较3种松树在4个不同的地区的生长情况有无差别，在每个地区对每种松树随机地选取5株，测量它们的胸径，得到的数据列表如下:
y <- c(23, 15, 26, 13, 21, 
       25, 20, 21, 16, 18, 
       21, 17, 16,  24, 27,
       14, 17, 19, 20, 24,
       28, 22, 25, 19, 26, 
       30, 26, 26, 20, 28,
       19, 24, 19, 25, 29,
       17, 21, 18, 26, 23,
       18, 10, 12, 22, 13,
       15, 21, 22, 14, 12,
       23, 25, 19, 13, 22, 
       18, 12, 23, 22, 19)
##松树
A <- c(rep("A1",20),rep("A2",20),rep("A3",20))
##地区
B <- rep(c(rep("B1",5),rep("B2",5),rep("B3",5),rep("B4",5)),3)

dat <- data.frame(y=y,A=A,B=B)
dat
attach(dat)
fit = aov(y~A*B,data=dat) #交互作用不显著，则需要剔除交互作用回到上面的写法y~A+B
summary(fit)
table(A,B)
#计算拒绝域
alpha = 0.05
qf(1-alpha,2,48) #3.190727 计算F值

#剔除交互作用回到上面的写法y~A+B
fit1 = aov(y~A+B,data=dat) #0.000311是代表因素A显著，0.4790代表因素B不显著。
summary(fit1)
qf(1-alpha,2,54) #3.168246
#分组计算
aggregate(y,by=list(A,B),mean)
aggregate(y,by=list(A,B),sd)
#检验方差齐性
bartlett.test(y~A,data=dat)
bartlett.test(y~B,data=dat)
 
par(mfrow=c(1,2)) #一行两列
interaction.plot(A, B, y, type="b", col = c("red", "blue"), pch = c(16, 18), main="Interaction between A and B")
interaction.plot(B, A, y, type="b", col=c("red", "blue"), pch = c(16, 18), main="Interaction between B and A")
#横坐标代表三种因素A1，A2，A3，不同颜色代表因素B
#左边那个图很明显没有交互作用，不管哪条线都是一个中间高两边低的情况。如果有条线是直接上去的，那就说明在因素A不同的情况下，对于y均值的趋势发生了变化，这时候表明我们可能需要考虑A和B的交互作用。
#右边那个图比左边那个图稍微有点不一样，说明了在改变因素B的时候，因素A出现了比较显著的变换。通过假设检验证明，得到因素A是显著而因素B不显著。
#绘图
library(HH)
interaction2wt(y~A*B)
#左上和右下两张图就是我们上面分析过的两张图
#而左下和右上是单独考虑A和B因素的时候，绘制的箱线图。

