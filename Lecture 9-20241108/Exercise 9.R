library(readr)
df <- read_csv("Lecture 9-20241108/Mall_Customers.csv")
View(df)

library(tidyverse)

#1.對變數做以下轉換

#當尺度單位不一致時，有數值變數也有類別變數，可對所有變數做標準化：
#類別變數：轉換為1和0
#數值變數：轉換至「以最大最小值為1和0的尺度中」，(x-min)/range

df |> mutate(
  gender = ifelse(Gender == "Male", 1 , 0),
  age = (Age - min(Age)) / (max(Age) - min(Age)),
  income = (AnnualIncome - min(AnnualIncome)) / (max(AnnualIncome) - min(AnnualIncome)),
  spending = (SpendingScore - min(SpendingScore)) /(max(SpendingScore) - min(SpendingScore))
) -> data

View(data)
newdata<-data[,-c(2,3,4,5)]
View(newdata)

#2.決定分群方法和群數
library(factoextra)

fviz_nbclust(newdata[,4:5], 
             FUNcluster = kmeans,# K-Means
             method = "wss",     # total within sum of square
             k.max = 20          # max number of clusters to consider
) 

k = kmeans(newdata[,4:5], centers=5, nstart=20) 
str(k)

fviz_cluster(k,           # 分群結果
             data = newdata[,4:5],              # 資料
             geom = c("point","text"), # 點和標籤(point & label)
             ellipse.type = "norm")      # 框架型態

#3.對群做EDA描述
newdata$cluster <- k$cluster
newdata |> group_by(cluster) -> newdata1
boxplot(newdata1$income ~ newdata1$cluster,
        col = c("#FFA042","#FF5151","#2828FF","#28FF28","#8600FF"))

