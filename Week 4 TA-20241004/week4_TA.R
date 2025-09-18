##1：factor order 因素排序
?factor
grade <- factor(c('丙','丁','戊','甲','乙'),levels=c('戊','丁','丙','乙','甲'),order=T)
str(grade)
#structure（结构）的缩写。它用来显示对象的内部结构和基本信息，帮助你快速了解一个对象的类型、各列/元素的类型及其大小、内容的预览等。

grade[3] > grade[1] #戊>丙?
grade[3] < grade[1] #戊<丙?


##2：dplyr(轉換資料、對資料做摘要)
library(ggplot2)
library(tidyverse)
data(diamonds)
?diamonds

head(diamonds)
str(diamonds)
summary(diamonds)
str(diamonds$cut)
str(diamonds$color)
str(diamonds$clarity)

head(diamonds$cut)
head(diamonds$color)
head(diamonds$clarity)
diamonds$cut[1] > diamonds$cut[6] #Ideal>Very Good嗎?
diamonds$color[1] > diamonds$color[6] #E>J嗎?

#select()：選取變數
#使用「變數名稱」選取變數
select2_1 <- select(diamonds,x:z)
head(select2_1)
select2_2 <- select(diamonds,x,y,z)
head(select2_2)

#使用「索引值」選取變數
select2_3 <- diamonds[,8:10] #選取第8個~第10個變數
head(select2_3)
select2_4 <- diamonds[,-10] #移除第10個變數
head(select2_4)
select2_5 <- diamonds[,c(-1,-10)] #移除第1個和第10個變數
head(select2_5)

##補充：starts_with()、ends_with()、contains()
names(diamonds)
select1_1 <- select(diamonds,starts_with("c"))
head(select1_1)
select1_2 <- select(diamonds,ends_with("e"))
head(select1_2)
select1_3 <- select(diamonds,contains("ri"))
head(select1_3)

#filter()：選取特定條件的觀察值#
filter1 <- filter(diamonds,price > 10000)
head(filter1)

# %in%
filter2 <- filter(diamonds, cut %in% c("Ideal","Premium"))
head(filter2)
# or
filter3 <- filter(diamonds, cut=="Ideal" | cut=="Premium")
head(filter3)

filter4 <- filter(diamonds, x*y*z>100)
head(filter4)

filter5 <- filter(diamonds,price>2000 & cut>="Very Good")
head(filter5)
#and:&
#or:|

#group_by()：群組之間比較(計算該種類下的統計量)
#summarise()：算資料的摘要統計量
group1 <- group_by(diamonds,cut) %>% 
  summarise(nDiamond=n(),MeanPrice=mean(price)) #n()計算個數
head(group1)

#arrange()：重新排列#
#數字類型：依照大小排序
#文字類型：依照字母排序
arrange1 <- arrange(diamonds,carat) #遞增
head(arrange1)

arrange2 <- arrange(diamonds,desc(carat)) #遞減
head(arrange2)

arrange3 <- arrange(diamonds,desc(carat),desc(price))
head(arrange3)

head(diamonds$cut)
arrange4 <- arrange(diamonds,cut) 
head(arrange4)

arrange5 <- arrange(diamonds,desc(cut)) 
head(arrange5)

#mutate()：以現有變數創造新的變數
mutate1 <- mutate(diamonds, gram = carat*0.2)
head(mutate1)

##補充：save() & load()
getwd() #查看目前工作路徑
setwd("D:/TA/113-1 TA/Week4") #設定工作路徑
save(arrange3,file = "arrange")
load("arrange")

