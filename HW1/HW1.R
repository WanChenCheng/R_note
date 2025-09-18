#1.

#By myself (我寫好爛好長)
install.packages("readr")
library(readr)
library(tidyr)
stock_short <- read_csv("C:\\Users\\Ava\\Desktop\\R\\HW1\\stock.csv")
stock_short1 <-separate(stock_short,id_name,into = c("id","name"),sep = " ")
stock_short2 <- gather(stock_short1,key = "time" ,value = "value", -id,-name,-type)
stock_short3 <-separate(stock_short2,time,into = c("year","month","date"),sep = "/")
stock_long <- pivot_wider(stock_short3, names_from = type, values_from = value) 
print(stock_long) #answer

#By Chat GPT 
# 載入必要的函式庫
library(tidyverse)
# 步驟 1：讀取資料
stock <- read_csv("C:\\Users\\Ava\\Desktop\\R\\HW1\\stock.csv")
# 步驟 2：轉換資料
tidy_stock <- stock %>%
  pivot_longer(
    cols = -c(id_name, type), # 保留 id_name 和 type 欄位
    names_to = "date", # 創建一個新的 'date' 欄位來儲存所有日期
    values_to = "price" # 創建一個新的 'price' 欄位來儲存價格
  ) %>%
  # 步驟 3：將 id_name 分割成 id 和 name
  separate(id_name, into = c("id", "name"), sep = " ", convert = TRUE) %>%
  # 步驟 4：將日期分割成年、月和日
  separate(date, into = c("year", "month", "day"), sep = "/", convert = TRUE) %>%
  # 步驟 5：將資料展開回寬格式
  pivot_wider(
    names_from = type, # 使用 type (open/close) 作為新欄位名稱
    values_from = price # 使用 price 作為數值
  )

# 查看整理後的資料
print(tidy_stock)

#2.
#By myself
client_list <- read_csv("C:\\Users\\Ava\\Desktop\\R\\HW1\\client_list.csv")
salesdata <- read_csv("C:\\Users\\Ava\\Desktop\\R\\HW1\\salesdata.csv")
product_list <- read_csv("C:\\Users\\Ava\\Desktop\\R\\HW1\\product_list.csv")
#(1.)
product_list1 <- separate(product_list,Item,into = c("Product","Item"),sep = "_",,convert=TRUE)
#(2.)
library(dplyr)
salesdata_sorted <- salesdata %>% arrange(Client)
salesdata_sorted  %>% left_join(client_list,by="Client") -> merge1
merge1  %>% left_join(product_list1,by="Product") -> full_table
full_table
#(3.)
library(tidyverse)
full_table%>%
  mutate( Spend = UnitPrice*Quantity ) -> full_table1
library(writexl)
write_xlsx(full_table1, path = "C:/Users/Ava/Downloads/full_table1.xlsx") #保存下來方便之後跑EDA!!

#(4.) EDA Start!!!
#分組
full_table2 <- full_table1 %>%
  mutate(Group = ifelse(Membership %in% c("gold", "diamond"), "Gold & Diamond", "Other"))

ggplot(full_table2, aes(x = Age, y = Spend)) +
  geom_col(fill = "steelblue") +
  labs(title = "Expenditure by Age Group", x = "Age", y = "Total Spend")

#年紀
age_comparison <- full_table2 %>%
  group_by(Group) %>%
  summarise(Average_Age = mean(Age, na.rm = TRUE))
#性別
gender_comparison <- full_table2 %>%
  group_by(Group, Gender) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = Count / sum(Count) * 100)
#國家
country_comparison <- full_table2 %>%
  group_by(Group, Region) %>%
  summarise(Count = n()) %>%
  arrange(Group, desc(Count))
#消費差異
spend_comparison <- full_table2 %>%
  group_by(Group) %>%
  summarise(Average_Spend = mean(Spend, na.rm = TRUE),
            Total_Spend = sum(Spend, na.rm = TRUE),
            Std_Dev_Spend = sd(Spend, na.rm = TRUE))
#(5.)
full_table2_female <- full_table2[full_table2$Gender =="female",]
write_xlsx(full_table2_female, path = "C:/Users/Ava/Downloads/full_table2_female.xlsx")

#年紀
age_comparison_female <- full_table2_female %>%
  summarise(Average_Age = mean(Age, na.rm = TRUE))

#國家
country_comparison_female <- full_table2_female %>%
  group_by(Region) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

#消費差異
spend_comparison_female <- full_table2_female %>%
  summarise(Average_Spend = mean(Spend, na.rm = TRUE),
            Total_Spend = sum(Spend, na.rm = TRUE),
            Std_Dev_Spend = sd(Spend, na.rm = TRUE))

#不同產品的「總消費」畫圖分析
library(ggplot2)
ggplot(full_table2_female,aes(x=Membership, y=Spend, fill=Store)) +
  geom_boxplot() +
  labs(title="Plot of Client's expenditure",x="Membership", y = "Total Amount")+
  theme_classic()

#(這裡好像沒有用了)
iPhone <- full_table2_female[full_table2_female$Item =="iPhone",]
iPad <- full_table2_female[full_table2_female$Item =="iPad",]
MacBook <- full_table2_female[full_table2_female$Item =="MacBook",]
iMac <- full_table2_female[full_table2_female$Item =="iMac",]
AirPods <- full_table2_female[full_table2_female$Item =="AirPods",]
AppleWatch <- full_table2_female[full_table2_female$Item =="AppleWatch",]


