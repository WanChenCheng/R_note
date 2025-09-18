library(ggplot2)
data(iris)
summary(iris)

##qplot：提供快速方便的畫圖功能
#geom參數：設定圖形類型("point","line","smooth","boxplot","histogram","bar")
#color、fill：將資料點以顏色區分
qplot(Sepal.Length,Petal.Length,data=iris) #散佈圖
qplot(Sepal.Length,Petal.Length,data=iris,geom=c("point","line")) 
qplot(Sepal.Length,Petal.Length,data=iris,color=Species,geom=c("point"))
qplot(Sepal.Length,Petal.Length,data=iris,color=Species,geom=c("point","smooth"))
qplot(Species,Petal.Length,data=iris,geom="boxplot") #箱型圖
qplot(Petal.Length,data=iris) #直方圖(Histigram)
qplot(Petal.Length,data=iris,fill = Species)
qplot(Petal.Length,data=iris,fill = Species,binwidth=0.5) #bin:組距
qplot(Species,data=iris) #長條圖(Bar chart)

#facets只可用在類別型變數
qplot(Sepal.Length,Petal.Length, 
      data = iris,
      facets =.~ Species) #左邊是row，右邊是column
qplot(Sepal.Length,Petal.Length, 
      data = iris,
      facets = Species~. )

##ggplot：可調整更多參數（標籤、顏色、形狀、主題...）

ggplot(iris, aes(Sepal.Length,Petal.Length)) +geom_point()
ggplot(iris, aes(Sepal.Length,Petal.Length)) +geom_line() 
ggplot(iris, aes(Sepal.Length,Petal.Length)) +geom_point()+geom_line()

##對某變數不同類別分別作圖+多圖合併
library(patchwork)
a <- ggplot(iris, aes(Sepal.Length,Petal.Length)) +
       geom_point()+geom_line() +facet_grid(Species~.)
b <- ggplot(iris, aes(Sepal.Length,Petal.Length)) +
       geom_point()+geom_line() +facet_grid(.~Species)
a+b

##geom_smooth：平滑曲線
?geom_smooth
ggplot(iris, aes(Sepal.Length,Petal.Length)) +geom_point()+
  geom_line() +facet_grid(.~Species)+geom_smooth() #預設為gam法(觀測值<1000)

ggplot(iris, aes(Sepal.Length,Petal.Length)) +geom_point()+
  geom_line() +facet_grid(.~Species)+geom_smooth(method='lm')

ggplot(iris, aes(Sepal.Length,Petal.Length,color=Species)) +
  geom_point()+geom_line() +geom_smooth(method='lm')

##設x,y座標與標題名稱
ggplot(iris, aes(Sepal.Length,Petal.Length,color=Species)) +
  geom_point()+geom_line() +geom_smooth(method='lm')+
  xlab("這裡打你要的x軸名")+ylab("這裡打你要的y軸名")

ggplot(iris, aes(Sepal.Length,Petal.Length,color=Species)) +
  geom_point()+geom_line() +geom_smooth(method='lm')+
  labs(x="這裡打你要的x軸名",y="這裡打你要的y軸名")

ggplot(iris, aes(Sepal.Length,Petal.Length,color=Species)) +
  geom_point()+geom_line() +geom_smooth(method='lm')+
  labs(x="這裡打你要的x軸名",y="這裡打你要的y軸名")+
  ggtitle("這裡打你要的圖表名稱")

##盒狀圖
ggplot(iris, aes(x=Species, y=Sepal.Length, fill=Species)) +
  geom_boxplot()

##圓餅圖
data(diamonds)
c1 = table(diamonds$color)
c1 = as.data.frame(c1)
piepercent <- paste(round(100*c1$Freq/sum(c1$Freq), 2), "%",sep="")

#rainbow():生成彩色色號的向量
pie(c1$Freq,labels = piepercent, main = "切割等級(預設)",col = rainbow(length(c1$Var1)))
legend("topright",as.character(c1$Var1), cex = 0.5,
       fill = rainbow(length(c1$Var1))) #加上圖例框

c2 <- c1[order(c1$Freq,decreasing = T),]
piepercent <- paste(round(100*c2$Freq/sum(c2$Freq), 2), "%")

pie(c2$Freq,labels = piepercent, main = "切割等級(大到小排序)",
    col = rainbow(length(c2$Var1)))
legend("topright",as.character(c2$Var1), cex = 0.5,
       fill = rainbow(length(c2$Var1)))

#自訂顏色向量
colors = c("red","orange","pink","lightyellow","lightblue","lightgreen","purple")
pie(c2$Freq,labels = piepercent, main = "切割等級(大到小排序)",col = colors)
legend("topright",as.character(c2$Var1), cex = 0.5, fill = colors)

##互動視覺化
library("plotly")
pic=ggplot(iris, aes(Sepal.Length,Petal.Length,color=Species)) +geom_point()+geom_line() +geom_smooth(method='lm')
ggplotly(pic)



