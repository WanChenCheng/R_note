setwd("C:/Users/user/Desktop/R 助教資料/112-1 TA課講義1/0320")
load("airbnb.table")
library(ggplot2)
library(dplyr)
head(airbnb.table)
names(airbnb.table)
table(airbnb.table$room_type)

#散佈圖：Manhattan的房價和瀏覽次數
Manhattan.table<-filter(airbnb.table,neighbourhood_group=="Manhattan")
ggplot(Manhattan.table,aes(price,number_of_reviews))+geom_point()

#圓餅圖：不同房屋類型占比
sum(is.na(airbnb.table$room_type))
table(airbnb.table$room_type)

c1 = table(airbnb.table$room_type)
c1 = as.data.frame(c1)
piepercent <- paste(round(100*c1$Freq/sum(c1$Freq), 2), "%")

colors = c("pink","lightyellow","lightblue","purple") 
pie(c1$Freq,labels =piepercent, main = "房屋類別",col = colors,cex=0.6)
legend("topright",as.character(c1$Var1), cex = 0.6,fill = colors)

pie(c1$Freq,labels =piepercent, main = "房屋類別",col = rainbow(length(c1$Var1)),cex=0.5)
legend("topright",as.character(c1$Var1), cex = 0.5,
       fill = rainbow(length(c1$Var1)))

