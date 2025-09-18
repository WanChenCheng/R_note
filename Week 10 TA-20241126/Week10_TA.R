#####作業一檢討
###第一題
library(readr)
library(tidyverse)

stock = read_csv("stock.csv")#建議用這個讀資料

#方法一
stock1 <- gather(stock, key = "time", value, c(3:63))
stock2 <- stock1 %>% separate(time,into=c("year","month","day"),sep="/") %>% separate(id_name,into=c("id","name"),sep=" ")
stock_ans1 <- stock2 %>% spread(key="type",value)


#方法二
stock_ans2 <- stock %>% 
  gather(ymd, value, `2024/3/7`:`2023/12/1`) %>% 
  spread(type, value) %>% 
  separate(id_name, into=c("id","name"), sep=" ", convert=T) %>% 
  separate(ymd, into=c("year","month","date"), sep="/", convert=T)

#方法三
stock = read.csv('stock.csv')#不建議使用
stock1 <- stock %>% separate(id_name, into = c('id', 'name'), sep = ' ')

stock2 <- stock1 %>% gather(date, price, 'X2024.3.7':'X2023.12.1')
stock3 <- stock2 %>% separate(date, into = c('year', 'month', 'date'))
stock_ans3 <- stock3 %>% spread(type, price)

stock_ans3$year <- gsub('X', '', stock_ans3$year)

###第二題
#讀資料
library(readr)
library(tidyverse)

client.df <- read_csv("client_list.csv")
prod.df <- read_csv("product_list.csv")
sales.df <- read_csv("salesdata.csv")

##2-1
prod.df <- prod.df %>%
  separate(Item, into = c("Product", "Item"), sep = "_")
#將Product轉成integer
prod.df$Product <- as.integer(prod.df$Product)

##2-2
#版本一
full.table <- sales.df %>%
  left_join(client.df, by = "Client") %>%
  left_join(prod.df, by = "Product")

#版本二
full.table2 <- sales.df %>%
  full_join(client.df, by = "Client") %>%
  full_join(prod.df, by = "Product")

##2-3
#方法一
full.table$spend <- full.table$UnitPrice * full.table$Quantity

#方法二
full.table <- full.table %>% mutate(spend = UnitPrice*Quantity)

##2-4
#版本一：以個別消費記錄的角度分析國家、性別、年齡、花費
full.table <- full.table %>%
  mutate(MembershipGroup = ifelse(Membership %in% c("gold", "diamond"), "gold_diamond", "others")) 

gold_diamond <- full.table %>% filter(MembershipGroup=="gold_diamond")
others <- full.table %>% filter(MembershipGroup=="others")

#性別
table(gold_diamond$Gender)
table(others$Gender)
#國家
sort(table(gold_diamond$Region),T)
sort(table(others$Region),T)
#消費狀況
summary(gold_diamond$spend)
summary(others$spend)
#年紀
summary(gold_diamond$Age)
summary(others$Age)


#版本二：以個別顧客的角度分析花費、性別、年齡
#取出Client,Region,spend,MembershipGroup
full.imp <- full.table[,c(4,7,13,14)]
full.imp2 <- full.imp %>% group_by(Client) %>% summarise(
  TotalSpend=sum(spend),
  MembershipGroup=unique(MembershipGroup)
)
full.imp2

client.imp <- client.df %>% left_join(full.imp2,by='Client') %>% na.omit()
#消費狀況、年紀
client.imp %>% group_by(MembershipGroup) %>% summarise(平均一個人消費=mean(TotalSpend,na.rm=T),平均年紀=mean(Age,na.rm=T))
#性別
client.imp %>% group_by(MembershipGroup,Gender) %>%summarise(人數=n())

##2-5
#版本一：以個別消費記錄的角度分析消費、國家、年齡
female <- full.table %>%
  filter(Gender == "female") 

summary(female$Age)
summary(female$spend)
sort(table(female$Region),T)

#畫不同產品的「總消費」圖(長條圖)
p <- female%>%
  group_by(Item) %>%
  summarise(Total_Spend = sum(spend, na.rm = TRUE)) %>% ggplot(aes(x=Item,y=Total_Spend))+geom_bar(stat = "identity")
p

#版本二：以個別顧客的角度分析年齡、消費
#Client,Region,Gender,Spend,Membership
full.imp <- full.table[,c(4,7,11,13,14)]
full.imp <- full.imp %>% filter(Gender=='female')
full.imp2 <- full.imp %>% group_by(Client) %>% summarise(
  TotalSpend=sum(spend),
)
full.imp2

client.df_female <- client.df %>% filter(Gender=='female')

client.imp <- client.df_female %>% left_join(full.imp2,by='Client') %>% na.omit()
client.imp %>% group_by(Membership) %>% summarise(平均一個人消費=mean(TotalSpend ,na.rm=T),平均年紀=mean(Age,na.rm=T))
client.imp %>% group_by(Membership,Gender) %>%summarise(人數=n())

summary(client.imp$Age)
summary(client.imp$TotalSpend)

###額外補充
ls() #列出工作目錄中所有的變數
rm(list=ls())#移除所有現存變數，跟按刷子一樣
gc() #垃圾回收，釋放空間
