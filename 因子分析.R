#蔡煜
#24-12-10

#因子分析

setwd("D:/R/RWorkPlace/应用数理统计")
library(ggplot2)
library(factoextra)
student=read.table("student.txt",header=TRUE)
#第一种写法（一般用于行数大于列数的情况）
#princomp提供的是载荷矩阵（loadings），这更多用于解释每个主成分的方差贡献
student.pca = princomp(student, cor = TRUE) #如果数据变量具有不同的量纲或数量级，选择cor = TRUE进行标准化

s = student.pca$scores
plot(s[,1],s[,2])

plot(student.pca,type="lines")
abline(h=1,lty=2)

M = cor(student)
M
library(corrplot)
corrplot(M)

score_fa = factanal(student,factors = 2,rotation = "varimax") #选择方差最大的方式
score_fa
#输出因子方差,方差贡献率和累积方差贡献率
score_fa$Vars

#输出载荷矩阵
score_fa$loadings



#真题
#极差分析的核心是计算每个因子在不同水平下的响应变量的平均值，然后求取这些平均值的极差。
data <- data.frame(
  A = c(1, 1, 1, 1, 2, 2, 2, 2),
  B = c(1, 1, 2, 2, 1, 1, 2, 2),
  AXB = c(1, 1, 2, 2, 1, 1, 2, 2),  # A × B
  C = c(1, 2, 1, 2, 1, 2, 1, 2),
  AXC = c(1, 2, 1, 2, 1, 2, 1, 2),  # A × C
  D = c(1, 2, 2, 1, 2, 1, 1, 2),
  ExperimentData = c(1.02, 0.78, 1.06, 0.94, 1.12, 1.16, 0.85, 0.97)
)
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
response <- "ExperimentData"

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
#因子A的极差最大，表明因子A对ExperimentData的影响最显著。
#因子D的极差最小，表明因子D对ExperimentData的影响不显著。