saledata <- read.csv("C:\\Users\\Ava\\Desktop\\R\\Lecture 3-20241004\\salesdata.csv", sep=",")
head(saledata)
str(saledata)

saledata$Product <-as.factor(saledata$Product)
saledata$Client <-as.factor(saledata$Client)

library(tidyverse)
saledata %>%
  ggplot(aes(x=UnitPrice, y=Quantity, color=Client))+
  geom_point()


saledata = saledata %>%
  mutate(Spend = UnitPrice*Quantity)

saledata %>%
  ggplot(aes(x=Client, y=Spend, fill=Store)) +
  geom_boxplot() +
  labs(title="Plot of Client's expenditure",x="Cliend ID", y = "Total Amount (in thousand)")+
  theme_classic()

saledata %>%
  ggplot(aes(x=Client, y=Spend)) +
  geom_boxplot() +
  labs(title="Sale Amount Distribution by Client",x="Cliend ID", y = "Total Amount (in thousand)")+
  theme_minimal()


ggplot(saledata, aes(x=Store)) +
  geom_bar() +
  geom_text(stat="count",aes(label=..count..),vjust=6, color=I("white"),size=7)


TotalSales <- saledata %>%
  group_by( Client) %>%  #根據顧客
  summarise( TotalSpend = sum(Spend)) %>% #把Spend加總
  arrange(desc(TotalSpend)) #排大到小

TotalSales %>%
  ggplot(aes(x=Client, y=TotalSpend, fill=Client)) +
  geom_bar( stat = 'identity') +
  scale_x_discrete(limits = TotalSales$Client) 
#使用 geom_bar() 并将 stat = 'identity'，表示直接用 TotalSpend 作为每个柱子的高度（因为你已经有了 TotalSpend 数据）。

ProductSale <- saledata %>%
  group_by( Product) %>%
  summarise( TotalPSale = sum(Spend))

ProductSale %>% 
  ggplot(aes(x= Product, y=TotalPSale)) +
  geom_bar(stat="identity") +
  labs(y="Total Product Sales Amount") 



ProductSale <- saledata %>%
  group_by( Product) %>%
  summarise( TotalQuan = sum(Quantity))

ProductSale %>% 
  ggplot(aes(x= Product, y=TotalQuan)) +
  geom_bar(stat="identity") +
  labs(y="Total Product Sales Quanties") +
  geom_text(stat="identity",aes(label=TotalQuan),vjust=1, color=I("yellow"),size=5)


ggplot( data = saledata) +
  geom_bar( aes( x = Product,
                 y = Spend),
            stat = 'identity') +
  facet_wrap( ~ Client)



Product <- saledata %>%
  group_by(Client, Product) %>%
  summarise(AmountSpend = sum(Spend)) %>%
  mutate( Proportion = round(AmountSpend / sum(AmountSpend),1)*100)


ggplot( data = Product, aes( x = Client, y = AmountSpend, fill = Product)) +  geom_bar(stat="identity") +
  geom_text(aes( x = Client, y = AmountSpend,  label= paste(Proportion, '%', sep='')), position = position_stack(vjust = 0.5), color = I("white"), size = 3) 
