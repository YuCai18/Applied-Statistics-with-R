#智研241 2024540099 蔡煜
#24-12-11
#第一题
# 输入数据
old_drink_scores <- c(7, 8, 6, 7, 8, 8, 7, 8, 7, 9)
new_drink_scores <- c(8, 10, 9, 9, 9, 8, 9, 9, 10, 7)

# 进行样本t检验
#双边的，假设两个样本方差是相同的，置信度是0.05
t_test_result <- t.test(old_drink_scores,new_drink_scores,var.equal=T,alternative = "two.sided",conf.level = 0.95)
# 输出结果
print(t_test_result)

#第二题
# 输入数据
品种 <- factor(rep(c("品种1", "品种2", "品种3"), each = 10, times = 3))
施肥方式 <- factor(rep(c("甲", "乙"),each = 5))
产量 <- c(729, 738, 711, 729, 702, 801, 828, 783, 765, 774,  # 甲品种1
        639, 648, 648, 594, 648, 693, 729, 693, 657, 711,  # 甲品种2
        684, 711, 693,684,702, 801,783, 756, 783, 783)  # 甲品种3, 乙

# 创建数据框
data <- data.frame(品种, 施肥方式, 产量)
data
# 进行双因素方差分析
anova_result <- aov(产量 ~ 品种 * 施肥方式, data = data)

# 输出结果
summary(anova_result)

par(mfrow=c(1,2)) #一行两列
interaction.plot(品种, 施肥方式, 产量, type="b", col = c("red", "blue"), pch = c(16, 18), main="Interaction between A and B")
interaction.plot(施肥方式, 品种, 产量, type="b", col=c("red", "blue"), pch = c(16, 18), main="Interaction between B and A")
#第三题
#餐馆的营业额受多种因素影响，比如客流量、价格、交通便捷程度、服务水平、同业竞争者的数量等。为分析营业额的影响因素，一家市场调查公司在某城市随机抽取25家餐馆，调查得到的有关数据如下表所示。建立多元线性回归模型，并解释各回归系数的含义。
# 输入数据
setwd("D:/R/RWorkPlace/应用数理统计")
data <- read_xlsx("期末数据.xlsx",sheet = 3)
head(data)
# 建立多元线性回归模型
fit = lm(y~.,data = data) #.代表省略写法,只有y是因变量,其他全是自变量
fit
summary(fit) #F-statistic: 8.278 on 4 and 22 DF,  p-value: 0.0003121这里的p值是对四个变量一起测的
anova(fit)
par(mfrow=c(2,2))
plot(fit)
#逐步选择法
selection <- step(fit)
summary(selection)

fit2 = lm(y~x1+x2+x5+I(x1^2)+I(x2^2)+I(x5^2),data = data)
summary(fit2)
anova(fit2) #得到x这项的p值是0.29代表不显著应该把它再剔除出去。

fit3 = lm(y~x1+x2+x5+I(x2^2)+I(x2^3),data = data)
summary(fit3)


#第四题
# 输入数据
setwd("D:/R/RWorkPlace/应用数理统计")
data <- read_xlsx("期末数据.xlsx",sheet = 4)
head(data)
sum(data[is.na(data)])

data.mat = as.matrix(data[,2:8]); rownames(data.mat)=data[,1]
# 使用最短距离法进行层次聚类
hc_single <- hclust(dist(data.mat), method = "single")

# 使用最长距离法进行层次聚类
hc_complete <- hclust(dist(data_matrix), method = "complete")

# 使用离差平方和法进行层次聚类
hc_ward <- hclust(dist(data_matrix), method = "ward.D2")

# 绘制树状图
par(mfrow = c(1, 3))
plot(hc_single, main = "最短距离法")
plot(hc_complete, main = "最长距离法")
plot(hc_ward, main = "离差平方和法")

# 根据树状图选择聚类数（这里以4类为例）
groups_single <- cutree(hc_single, k = 4)
groups_complete <- cutree(hc_complete, k = 4)
groups_ward <- cutree(hc_ward, k = 4)

# 打印聚类结果
print(paste("最短距离法聚类结果:", toString(groups_single)))
print(paste("最长距离法聚类结果:", toString(groups_complete)))
print(paste("离差平方和法聚类结果:", toString(groups_ward)))

#第五题
# 输入数据
setwd("D:/R/RWorkPlace/应用数理统计")
data <- read_xlsx("期末数据.xlsx",sheet = 5)
library(ggplot2)
library(factoextra)
pca = princomp(data[,-1], cor = TRUE) #如果数据变量具有不同的量纲或数量级，选择cor = TRUE进行标准化

summary(pca, loadings = TRUE)
#绘制折线图
par(mfrow = c(1, 2))
screeplot(pca, type = "lines", main = "Scree Plot", lwd = 2)
#加上蓝色的横直线
abline(h = 0.5598064^2, lty = 2, col = "blue", lwd = 3)
biplot(pca)

fviz_pca_ind(pca,
             col.ind = "cos2",  
             gradient.cols =c("red", "blue", "black"),
             repel = TRUE)

#第六题
# 输入数据
setwd("D:/R/RWorkPlace/应用数理统计")
data <- read_xlsx("期末数据.xlsx",sheet = 6)

library(dplyr)

# 定义一个函数来计算极差
calculate_range <- function(data, factor, response) {
  # 计算每个因子水平下的响应变量平均值
  means <- data %>%
    group_by(!!sym(factor)) %>%
    summarize(mean_response = mean(!!sym(response)))
  
  # 计算极差
  range_value <- max(means$mean_response) - min(means$mean_response)
  
  return(range_value)
}

# 列出所有因子（包括交互效应）
factors <- c("A", "B", "AXB", "C", "AXC", "D")
response <- "试验数据"

# 计算所有因子的极差
range_results <- sapply(factors, function(factor) calculate_range(data, factor, response))

# 将结果转换为数据框，便于查看和排序
range_df <- data.frame(
  Factor = names(range_results),
  Range = range_results
)
range_df

# 按极差从大到小排序
range_df <- range_df[order(-range_df$Range), ]

print("各因子的极差结果：")
print(range_df)
#第七题
#文件7中是学生的期末成绩，试利用合适的图表进行描述性分析。
library(readxl)
# 读取xlsx文件

setwd("D:/R/RWorkPlace/应用数理统计")
data <- read_xlsx("期末数据.xlsx",sheet = 7)
#help(read_excel)
head(data)
# 确保数据中没有缺失值
sum(is.na(data))
# 假设期末成绩在名为"期末成绩"的列中
final_scores <- data$期末成绩

# 直方图
hist(final_scores, main = "期末成绩直方图", xlab = "期末成绩", col = "blue", border = "black")

# 箱线图
data_without_id <- data[, -1]
numeric_data <- data_without_id[,sapply(data_without_id, is.numeric)]
boxplot(numeric_data, main="期末数据箱线图", ylab="成绩", col="lightblue")

# 密度图
plot(density(final_scores), main = "期末成绩密度图", xlab = "期末成绩", col = "red")

#回归分析

# 建立线性回归模型
model <- lm(期末成绩 ~ 平时成绩, data = data)

# 查看模型摘要
summary(model)

# 模型诊断：残差图
par(mfrow = c(2, 2))  # 设置图形布局为2x2
plot(model)

#修改模型
model2 <- lm(总评成绩 ~ 平时成绩+期末成绩, data = data)
# 查看模型摘要
summary(model2)
# 模型诊断：残差图
par(mfrow = c(2, 2))  # 设置图形布局为2x2
plot(model2)

