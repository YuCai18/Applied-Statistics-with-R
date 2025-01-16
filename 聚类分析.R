#蔡煜
#24-12-09

#老师给的代码(层次聚类)
Consumer = read.csv("consumer2018.csv", header = TRUE, fileEncoding = "GBK")
Cosumer  = Consumer[, -c(2, 3)]
Cosumer

data.mat = as.matrix(Cosumer[, 2:9]); rownames(data.mat)=Cosumer[, 1]
#### 编写函数计算总离差平方和、类内离差平方和和R^2统计量
#总平方和（Total Sum of Squares, TSS）
tss.cal = function(x) sum(scale(x, scale = F)^2)
#类内的平方和（Within-Cluster Sum of Squares, WSS）
wss.cal = function(x, clst) sum(by(x, INDICES=clst,FUN = tss.cal))
#计算R平方（R^2）。R平方是衡量聚类效果的指标。
rsq.cal = function(x, clst) (tss.cal(x)-wss.cal(x, clst))/tss.cal(x)

rsq = vector("numeric", 14)
res = hclust(dist(scale(data.mat)), method = "ward.D")
res
#kmeans算法
for(ii in 2:15){
  clst = cutree(res, ii)
  rsq[ii-1] = rsq.cal(data.mat, clst)
}
plot(2:15, rsq,  type="b", xlab="K", ylab=expression(R^2), col = "blue", lwd = 3)

m = c("single","complete","median","average","centroid","ward.D")
h = list()
reh = list()
opar = par(mfrow=c(3, 2))

for (i in 1:length(m)){
  h[[i]]=hclust(dist(scale(data.mat)), method = m[i])
  plot(h[[i]], main = paste("Method: ", m[i]), hang = -1)
  reh[[i]] = rect.hclust(h[[i]], k = 5)
}

#我自己的模板
#层次聚类法
rm(list=ls()) #删除环境中的数字,清空缓存
setwd("D:/R/RWorkPlace/应用数理统计")
DM=read.csv("DRfull.csv")
head(DM)
hc = hclust(dist(scale(DM[,-1])),method="ward.D2")
plot(hc,labels = DM[,1],cex=.4,main=("Method:ward.D2"),xlab="")
Id = identify(hc) #手动点击图片选择类别,然后按esc退出
for(i in 1:length(Id)){
  print(DM[Id[[i]],1])
}

#常见的类间距离
#最短距离法:single,最长距离法:complete,类平均法:average,加权类平均法:mcquitty,中位数法median
#中心法 centroid,ward法或最小方差法ward.D2

#Kmeans聚类
rm(list=ls()) #删除环境中的数字,清空缓存
setwd("D:/R/RWorkPlace/应用数理统计")
w = read.csv("kmeansFig.csv", header = TRUE)
attach(w)
w
#choose k
library(NbClust)
choose_k =NbClust(scale(w),distance="euclidean",min.nc=2,max.nc=8,method="complete",index="all")
#choose_k最后一句话会告诉你k取什么值最好

a = kmeans(w,4)
a
plot(w,pch = a$cluster)
text(a$center,expression(c[1],c[2],c[3],c[4]),col=2)
