setwd("C:/Users/Ava/Desktop/R/Lecture 2-20240927") #放你的路徑
print(load("carsale.Rda")) #load() 函數是用來載入 .Rda 或 .Rdata 文件中的數據對象

# 1.
str(mydata) #顯示對象的結構和更多細節（如行數、列數、變量類型及部分數據）
class(mydata) #只返回對象的類型名稱
class(mydata[,13])
sapply(mydata, class) #sapply(mydata, class) 在 R 中是用來檢查 mydata 中每一列的資料類型（class）。sapply() 函數是 apply() 系列的一部分，用來對數據集的每個元素應用指定的函數，並簡化輸出。

# 2.
summary(mydata)

# 3.
names(mydata)
colnames(mydata) #和上面的一樣，看變數類型

# 4.
noNAtype  <-mydata[which(mydata$type != "NA"),] # 去掉type有NA的

# 5.
noNAdata <- na.omit(mydata) # 去掉所有NA  
summary(noNAdata$city)
table(noNAdata$city) # same as the former one
levels(noNAdata$city)

# 6. Which one is the most expensive?
max(mydata$price, na.rm=T)
which.max(mydata$price)
mydata[ which.max(mydata$price), ]$maker
mydata[ which.max(mydata$price), ]$header

# 7. maybe that price is a typo, use something to replace it
median(c(6000,30000))
mydata[which(mydata$price==max(mydata$price,na.rm=T)),]$price = median(c(6000,30000)) #用中位數取代
mydata[which.max(mydata$price),]$header #the new most expensive one
mydata[which.max(mydata$price),]$price 
head(sort(mydata$price,decreasing=T))


# Others
# 1.
table(mydata$maker)

# 2.
plot(mydata$long,mydata$lat) #通常用在連續型變數

library(lattice)
xyplot(lat ~ long, mydata)
xyplot(lat ~ long | type, mydata) #使用 xyplot() 函數創建一個散點圖，lat 作為 y 軸，long 作為 x 軸，並使用 mydata 數據框。


install.packages("ggplot2")
library(ggplot2)
ggplot(mydata, aes(x = long, y = lat)) +
  geom_point()+
  theme_classic()


ggplot(mydata, aes(x = long, y = lat)) +
  geom_point() +
  facet_wrap(~ type, ncol = 3) #facet_wrap() 將圖表按 type 變量分成多個子圖，每個子圖展示不同類別的車輛地理分佈情況。

# 3. Subset

bmw_df <- subset(mydata, maker=="bmw") #subset() 函數用於篩選數據。它接受一個數據集和篩選條件，返回符合條件的子集。
head(bmw_df)
subset(bmw_df, price > 40000, select = c(price, maker,type))
subset(bmw_df, price > 40000 & type=='SUV', select = c(price, maker, type))

