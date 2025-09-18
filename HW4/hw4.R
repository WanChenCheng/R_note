#財務指標資料分析 Analysis of financial indexes
#分析財務報表，了解公司企業特性，以降維的方式，找出有意義的指標，衡量績優公司。
#資料financialdata_20231113.csv 有382間公司的財務指標。此數據集包含了重點財務指標，如 ROE、周轉率和研發投資、 SeasonReturn(%)（季度回報率）、TwoMonthReturn(%)（兩月回報率）、MonthReturn(%)（月回報率）和 WeekReturn(%)（周回報）等。

#1.以PCA或SPCA分析，找出前二個主成份共能解釋多少變異？

#2.找出前三個主成份分別重點變數為何，需說明你的理由。

#3.依第一、二主成份結果說明，找出適合投資的公司。(例如：適合投資資產報酬率高的XX公司)

#Part0. Preparation
library(readr)
fin <- read_csv("C:/Users/Ava/Desktop/R/HW4/financialdata_20231113.csv")
View(fin)
fin <- fin[,3:18]
library(summarytools)
dfSummary(fin) # provides a detailed summary of your data frame
view(dfSummary(fin)) # see the results in RStudio’s Viewer; stview(dfSummary(murders))
library(tidyverse)
fin <- fin |> 
  filter(
    !(InventoryTurnover == Inf | 
        AccountsPayableTurnoverRatio == Inf |
        AccountsReceivableTurnoverRatio == Inf)
  ) #!用來移除東東

#Part1. Dimension reduction (Correlation)

## understand ur data
M = cor(fin)

## create heatmap for correlation 
library(reshape2)
melted_cormat <- melt(M)
head(melted_cormat)

library(ggplot2)

ggplot(data = melted_cormat,
       aes(Var1, Var2)) +
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient2(low = "firebrick4", high = "steelblue",
                       mid = "white", midpoint = 0) +
  guides(fill=guide_legend(title="Correlation")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title = element_blank())

library(corrplot)
corrplot(M, method="circle")

#Part2.PCA
#install.packages("stats")
library(stats)
pca<- prcomp(fin, center = TRUE, scale = TRUE)

names(pca) 
summary(pca)

plot(pca) 

## how to get the results of summary(pca)
var = pca$sdev^2 #該主成份解釋變異數的數值
prop = (pca$sdev)^2 / sum((pca$sdev)^2) #該主成份解釋變異數的比率
cum_prop = cumsum((pca$sdev)^2 / sum((pca$sdev)^2)) #該主成份解釋變異數的累積比率

## scree plot: variance
screeplot(pca) #same as plot(pca)
plot(pca, type="line")
abline(h=1, col="blue") #Kaiser eigenvalue-greater-than-one rule, choose pc1~pc5 by Kaiser

summary(pca)



## Rotation matrix: Loadings are the percent of variance explained by the variable
pca$rotation

#visualize
ggplot(melt(pca$rotation[,1:5]), aes(Var2, Var1)) +
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient2(low = "firebrick4", high = "steelblue",
                       mid = "white", midpoint = 0) +
  guides(fill=guide_legend(title="Coefficient")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title = element_blank())



## The actual principal components
head(pca$x)

## Standard deviation of components is represents the percent of variation each component explains
pca$sdev

## Compute variance explained
pve=(pca$sdev)^2 / (sum(pca$sdev^2))
plot(pve, xlab="Principal Component", ylab="Proportion of Variance Explained ", ylim=c(0,1),type='b')#scree plot
plot(cumsum(pve), xlab="Principal Component", ylab="Cumulative Proportion of Variance Explained ", ylim=c(0,1),type='b')
abline(h=0.8)

## pc1 vs pc2 plot
biplot(pca,scale=T) #first two components
biplot(pca,scale=T,choices=2:3)



