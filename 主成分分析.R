#蔡煜
#24-12-09
#主成分分析
setwd("D:/R/RWorkPlace/应用数理统计")
library(ggplot2)
library(factoextra)
student=read.table("student.txt",header=TRUE)
#第一种写法（一般用于行数大于列数的情况）
#princomp提供的是载荷矩阵（loadings），这更多用于解释每个主成分的方差贡献
student.pca = princomp(student, cor = TRUE) #如果数据变量具有不同的量纲或数量级，选择cor = TRUE进行标准化
#第二种写法（两种情况都可以用）
#prcomp提供的是旋转矩阵（rotation），可以用来理解主成分与原始变量之间的关系
#student.pca = prcomp(student,center=F,scale. = T)
#student.pca
summary(student.pca, loadings = TRUE)
student.pca$scores
princomp.rank<-function(PCA,m=2,plot=F) 


#绘制折线图
par(mfrow = c(1, 2))
screeplot(student.pca, type = "lines", main = "Scree Plot", lwd = 2)
#加上蓝色的横直线
abline(h = 0.5598064^2, lty = 2, col = "blue", lwd = 3)
biplot(student.pca)

fviz_pca_ind(student.pca,
             col.ind = "cos2",  
             gradient.cols =c("red", "blue", "black"),
             repel = TRUE)


#例子2：用燕尾花数据集做分析
install.packages("factoextra")
library(factoextra)
View(iris)
getwd()  # 查看工作目录
head(iris) # 查看前六行数据
iris.pca=prcomp(iris[,1:4],center=F,scale. = T) # 重点1，主成分分析的结果,去中心化和归一化
iris.pca #有四列，每列代表载荷值，载荷值代表构建主成分时的各个变量的系数
names(iris.pca)  # 表头
# 查看PCA后的结果
summary(iris.pca) #查看各个主成分的重要性
#看第二行，意味着第一主成分表达了原数据中的93.63%的信息，第二主成分表达了原数据中的6.029%的信息
#看第三行，代表主成分的方差累积占比，前两个主成分的方差累积占比为0.99658
#绘制碎石图
fviz_eig(iris.pca, addlabels = T)# 以图表形式来展现选择PC1/PC1,2/PC1,2,3，..N （选择标准：>80%即可） 
fviz_pca_var(iris.pca) #可视化所有变量在第一主成分和第二主成分下的载荷
fviz_pca_biplot(iris.pca) #可视化个体和变量之间的双标图
biplot(iris.pca,scale = 0)
iris.pca$x  # 查看表头x里存的数据，x是经过主成分分析转换后的数据
head(iris.pca$x)
iris.pca$rotation #主成分分析的系数
head(iris.pca$rotation)
sum(iris.pca$rotation[,1]*iris[1,1:4]) #用主成分分析的系数*第一行燕尾花数据
head(iris.pca$x)
# Dessiner
fviz_pca_ind(iris.pca, col.ind= iris$Species, addEllipses = T,geom = ("point")) #addEllipses = T代表画出椭圆形框
