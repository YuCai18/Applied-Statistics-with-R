#!/usr/bin/env python
# -*- coding: UTF-8 -*-
'''
@Project ：ARIMA 
@File    ：ARIMA.py
@IDE     ：PyCharm 
@Author  ：CaiYu
@Date    ：2024-12-10 22:20 
@explain : 文件说明
'''
from datetime import datetime
import numpy as np
import pandas as pd
import matplotlib.pylab as plt
from matplotlib.pylab import rcParams

import warnings
warnings.filterwarnings('ignore')

from statsmodels.tsa.stattools import adfuller

import pmdarima as pm
#数据集链接：https://www.kaggle.com/datasets/rakannimer/air-passengers
'''
时间序列数据的组成部分
趋势

数据随时间变化的总体方向。例如，如果我们观察一个新生儿的身高，他们的身高会随着他们成长而呈现上升趋势。另一方面，一个成功减肥的人会看到他们的体重随时间呈现下降趋势。
季节性+周期

任何具有固定频率的季节性或重复模式。可以是小时、月、日、年等。一个例子是冬季夹克在冬季月份销量增加，在夏季月份销量减少。另一个例子可能是你的银行账户余额。在每个月的前10天，你的余额会因为你支付月租、公用事业和其他账单而呈现下降趋势。
不规则性+噪声

数据中的任何大的峰值或低谷。一个例子可能是你跑400米短跑时的心率。当你开始比赛时，你的心率与一天中的其他时间相似，但在比赛期间，它会在短时间内飙升到一个更高的水平，然后恢复正常水平。
在航空公司乘客数据的可视化中，我们可以寻找这些组成部分。乍一看，数据集中似乎有一个正趋势和某种季节性或周期性。数据中似乎没有任何大的不规则性或噪声。

'''
df = pd.read_csv("../input/air-passengers/AirPassengers.csv")
#string to date format
df['Month'] = pd.to_datetime(df['Month'],infer_datetime_format=True)
df = df.set_index(['Month'])
df.head(5)


plt.figure(figsize=(15,7))
plt.title("Number of Airline Passengers by Date")
plt.xlabel('Date')
plt.ylabel('Passengers')
plt.plot(df)
plt.show()

'''
滚动统计
滚动平均是一种很好的方法来可视化数据集的趋势。由于数据集提供了按月计数的数据，窗口大小为12将给我们年度滚动平均值。

我们还将包括滚动标准差，以查看数据与滚动平均值的变异程度。
'''
#Determine rolling statistics
df["rolling_avg"] = df["#Passengers"].rolling(window=12).mean() #window size 12 denotes 12 months, giving rolling mean at yearly level
df["rolling_std"] = df["#Passengers"].rolling(window=12).std()

#Plot rolling statistics
plt.figure(figsize=(15,7))
plt.plot(df["#Passengers"], color='#379BDB', label='Original')
plt.plot(df["rolling_avg"], color='#D22A0D', label='Rolling Mean')
plt.plot(df["rolling_std"], color='#142039', label='Rolling Std')
plt.legend(loc='best')
plt.title('Rolling Mean & Standard Deviation')
plt.show(block=False)
'''
增强型迪基-富勒检验（Augmented Dickey-Fuller Test，简称ADF检验）是一种用于判断时间序列数据是否平稳的统计方法。类似于t检验，我们在测试前设定一个显著性水平，并根据得出的p值对假设进行判断。

零假设：数据不是平稳的。

备择假设：数据是平稳的。

要使数据平稳（即拒绝零假设），ADF检验应该满足以下条件：

p值 <= 显著性水平（0.01、0.05、0.10等）
如果p值大于显著性水平，那么我们可以说数据很可能不是平稳的。

我们可以看到下面提供的ADF检验中，p值为0.991880，这意味着数据非常可能是不平稳的。

'''
# Augmented Dickey–Fuller test:
print('Results of Dickey Fuller Test:')
dftest = adfuller(df['#Passengers'], autolag='AIC')

dfoutput = pd.Series(dftest[0:4], index=['Test Statistic', 'p-value', '#Lags Used', 'Number of Observations Used'])
for key, value in dftest[4].items():
    dfoutput['Critical Value (%s)' % key] = value

print(dfoutput)

'''
ARIMA模型选择与Auto-ARIMA

尽管我们的数据几乎可以肯定不是平稳的（p值=0.991），让我们看看一个标准的ARIMA模型在时间序列上的表现如何。

使用pmdarima包中的auto_arima函数，我们可以对模型的最优参数进行搜索。

重要说明：

当我们训练后打印模型摘要时，标题将始终是SARIMAX，不管我们使用什么模型参数。如果我们将季节性设置为FALSE或者将m参数设置为1，那么我们就有一个标准的ARIMA模型。这将是我们首先训练的模型。有关更多信息，请参阅这个Stack Overflow问题 --> auto_arima(..., seasonal=False) but got SARIMAX?
。

'''
#Standard ARIMA Model
ARIMA_model = pm.auto_arima(df['#Passengers'],
                      start_p=1,
                      start_q=1,
                      test='adf', # use adftest to find optimal 'd'
                      max_p=3, max_q=3, # maximum p and q
                      m=1, # frequency of series (if m==1, seasonal is set to FALSE automatically)
                      d=None,# let model determine 'd'
                      seasonal=False, # No Seasonality for standard ARIMA
                      trace=False, #logs
                      error_action='warn', #shows errors ('ignore' silences these)
                      suppress_warnings=True,
                      stepwise=True)
print(ARIMA_model.summary())


'''
通过 plot_diagnostics 函数生成的四个图表包括：标准化残差图、直方图加核密度估计图、正态Q-Q图和自相关图（ACF图）。

我们可以基于以下条件判断模型拟合良好：

标准化残差图
残差图中没有明显的模式，值的平均数为零，并且具有均匀的方差。
直方图加核密度估计图
核密度估计（KDE）曲线应与正态分布非常相似（在图中标记为N(0,1)）。
正态Q-Q图
大部分数据点应位于直线上。
自相关图（ACF图）
大于零的滞后相关性95%不应显著。灰色区域是置信区间，如果值落在这个区间之外，则它们是统计显著的。在我们的情况下，有一些值落在这个区域之外，因此我们可能需要添加更多的预测变量来提高模型的准确性。
'''

ARIMA_model.plot_diagnostics(figsize=(15,12))
plt.show()

def forecast(ARIMA_model, periods=24):
    # Forecast
    n_periods = periods
    fitted, confint = ARIMA_model.predict(n_periods=n_periods, return_conf_int=True)
    index_of_fc = pd.date_range(df.index[-1] + pd.DateOffset(months=1), periods = n_periods, freq='MS')

    # make series for plotting purpose
    fitted_series = pd.Series(fitted, index=index_of_fc)
    lower_series = pd.Series(confint[:, 0], index=index_of_fc)
    upper_series = pd.Series(confint[:, 1], index=index_of_fc)

    # Plot
    plt.figure(figsize=(15,7))
    plt.plot(df["#Passengers"], color='#1f76b4')
    plt.plot(fitted_series, color='darkgreen')
    plt.fill_between(lower_series.index,
                    lower_series,
                    upper_series,
                    color='k', alpha=.15)

    plt.title("ARIMA/SARIMA - Forecast of Airline Passengers")
    plt.show()

forecast(ARIMA_model)
'''
SARIMA模型选择
现在让我们尝试上面使用过的策略，只不过这次我们使用SARIMA模型，以便我们可以考虑到季节性因素。
'''

# Seasonal - fit stepwise auto-ARIMA
SARIMA_model = pm.auto_arima(df["#Passengers"], start_p=1, start_q=1,
                         test='adf',
                         max_p=3, max_q=3,
                         m=12, #12 is the frequncy of the cycle
                         start_P=0,
                         seasonal=True, #set to seasonal
                         d=None,
                         D=1, #order of the seasonal differencing
                         trace=False,
                         error_action='ignore',
                         suppress_warnings=True,
                         stepwise=True)

'''
查看模型诊断，我们可以看到与标准ARIMA模型相比存在一些显著差异。

标准化残差

标准化残差在图表中更加一致，这意味着数据更接近于平稳状态。
直方图加KDE估计

KDE曲线与正态分布相似（这里没有太大变化）。
正态Q-Q图

数据点比ARIMA诊断图中的点更接近直线。
自相关图（ACF图）

灰色区域是置信区间，如果值落在这个区域之外，则它们是统计显著的。我们希望所有值都在这个区域内。加入季节性成分后实现了这一点！现在所有点都落在95%的置信区间内。

'''
SARIMA_model.plot_diagnostics(figsize=(15,12))
plt.show()
#然后我们可以使用这个模型来预测未来24个月的航空公司乘客数量，就像我们之前做的那样。

#从下面的图表中我们可以看到，这个模型的预测似乎比标准ARIMA模型的预测要准确得多！

forecast(SARIMA_model)

'''
SARIMAX模型选择
现在让我们练习添加一个外生变量。在这个例子中，我简单地将月份编号作为一个外生变量添加进去，但这并不是特别有用，因为这一点已经通过季节性传达了。

请注意，我们在将数据传递给SARIMAX模型时，添加了额外的方括号。
'''

#adding exogenous variable
df['month_index'] = df.index.month

# SARIMAX Model
SARIMAX_model = pm.auto_arima(df[['#Passengers']], exogenous=df[['month_index']],
                           start_p=1, start_q=1,
                           test='adf',
                           max_p=3, max_q=3, m=12,
                           start_P=0, seasonal=True,
                           d=None, D=1,
                           trace=False,
                           error_action='ignore',
                           suppress_warnings=True,
                           stepwise=True)

SARIMAX_model.summary()

def sarimax_forecast(SARIMAX_model, periods=24):
    # Forecast
    n_periods = periods

    forecast_df = pd.DataFrame({"month_index":pd.date_range(df.index[-1], periods = n_periods, freq='MS').month},
                    index = pd.date_range(df.index[-1]+ pd.DateOffset(months=1), periods = n_periods, freq='MS'))

    fitted, confint = SARIMAX_model.predict(n_periods=n_periods,
                                            return_conf_int=True,
                                            exogenous=forecast_df[['month_index']])
    index_of_fc = pd.date_range(df.index[-1] + pd.DateOffset(months=1), periods = n_periods, freq='MS')

    # make series for plotting purpose
    fitted_series = pd.Series(fitted, index=index_of_fc)
    lower_series = pd.Series(confint[:, 0], index=index_of_fc)
    upper_series = pd.Series(confint[:, 1], index=index_of_fc)

    # Plot
    plt.figure(figsize=(15,7))
    plt.plot(df["#Passengers"], color='#1f76b4')
    plt.plot(fitted_series, color='darkgreen')
    plt.fill_between(lower_series.index,
                    lower_series,
                    upper_series,
                    color='k', alpha=.15)

    plt.title("SARIMAX - Forecast of Airline Passengers")
    plt.show()

sarimax_forecast(SARIMAX_model, periods=24)

