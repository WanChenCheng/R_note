install.packages("tidyverse")
library(tidyverse)


## 原本R的寫法：
set.seed(1) # 設定隨機數種子，確保每次生成的隨機數相同
x = rnorm(100) # 生成 100 個標準正態分佈的隨機數，存入變數 x
y = abs(x) # 計算 x 的絕對值，並存入變數 y
hist(y) # 繪製 y 的直方圖，顯示其分佈情況

## with %>%:( can use |> ) 
rnorm(100) %>% # 生成 100 個標準正態分佈的隨機數，並將結果傳遞給下一個函數
  abs() %>% # 計算隨機數的絕對值，並將結果傳遞給下一個函數
  hist() # 繪製絕對值的直方圖

rnorm(100) |> # randomize normal 
  abs() |> 
  hist() #(同上)

# car sale例子
setwd("C:/Users/Ava/Desktop/R/Lecture 2-20240927")
load("carsale.Rda")

mydata %>% # 使用管道操作符，將 mydata 傳遞到下一個函數 (翻譯年糕:and then 然後呢)
  dim()    # 獲取 mydata 的維度（行數和列數）

mydata %>%
  summary()
mydata %>%
  str() 
mydata %>%
  filter(maker=="toyota") %>%
  head()

## 用小一點的資料試試
mydata2 <- filter(mydata, !is.na(price), maker=="volvo"| maker== "tesla"| maker=="lexus" ) %>%
  select( maker,price,year,type )  #直線是or

mydata %>% 
  filter(!is.na(price), maker=="volvo"| maker== "tesla"| maker=="lexus" ) %>%
  select( Pay = price, Car = type ) %>% #can also change var name
  head()

mydata2 %>%
  select( type, price, everything() ) %>% #change the order of variables
  head()
#everything() 是一個 dplyr 函數，這會選擇資料框中剩餘的所有欄位，並保持它們原本的順序。
#總結來說，這行代碼的目的是將 type 和 price 兩個欄位放在前面，其餘欄位則按原本的順序排列。

mydata2 %>%
  filter( price >= 15000 & maker =="tesla" )   #subset
  
mydata2 %>%
  filter( maker =="tesla" ) %>%  
  select( year ) 

mydata2 %>%
  summarise(avg = mean(price, na.rm = TRUE) )

mydata2 %>%
  group_by( maker ) %>%
  summarise( mean(price, na.rm = TRUE) )
#group_by() 函數將資料框按 maker 欄位進行分組。這樣後續的操作（如計算統計量）將針對每個製造商的資料進行。
#將資料框依據一個或多個變數（例如 maker、year 等）進行分組，使得每個組別內的資料可以被獨立處理。

mydata2 %>%
  group_by( maker ) %>%
  summarise( count = n(),
             avg_price = mean(price),
             avg_year = mean(year[price>10000]),
             sd_price = sd(price)
            )

mydata2 %>%
  mutate( new_price = price*0.5 ) %>% 
  head()
# mutate() 函數用來新增或修改資料框中的欄位。

mydata2 %>%
  arrange( price ) %>% #sort
  head()

mydata2 %>%
  arrange( desc(price) ) %>%
  head()
#desc(price) 表示按照 price 欄位的值進行降序排序（即從高到低）。如果想要升序排列（從低到高），可以直接使用 arrange(price)

mydata2 %>%
  arrange( price, year ) %>% #sort
  head()

mydata2 %>%
  filter(price >= 15000 & maker =="lexus") %>%  
  tally()  #how many 

mydata2 %>%
  filter( price == max(price) ) %>% 
  select( maker, year, price )
#把最大那一筆資料抽出來

mydata2 %>%
  group_by( maker,type ) %>%
  tally()

# mydata2 %>%:
#使用管道操作符 %>%，將 mydata2 的結果傳遞給接下來的 group_by() 函數。

# group_by(maker, type):
# group_by() 函數用來將資料框按指定的變數進行分組。在這裡，資料會根據 maker（製造商）和 type（類型）進行分組。
# 每個唯一的 maker 和 type 的組合都會形成一個組別。

# tally():
# tally() 函數計算每個組別的行數，即每個 maker 和 type 組合的計數。
# 返回的資料框將包含每個組別及其對應的計數。

mydata2 %>% 
  count( maker, type ) %>%  
  ggplot( mapping = aes(x = maker, y = type) ) +
  geom_tile( mapping = aes(fill = n) )
# geom_tile(mapping = aes(fill = n)):
# geom_tile() 函數用來畫出熱圖的每一個方塊。
# aes(fill = n) 指定每個方塊的填充顏色根據計數 n 來決定，這樣計數較高的組合會顯示為較深的顏色，計數較低的則為較淺的顏色。

mydata2 %>% 
  qplot( price,year,col=maker, data=. )
# qplot 散點圖
# data = . 指定使用管道傳遞過來的資料框 mydata2

mydata2 %>% 
  qplot( price, geom="histogram", data=., bins=5 ) 

# qplot() 可以根據不同的參數自動識別你想要繪製的圖形類型。例如：
# 散點圖：不需要指定 geom。
# 直方圖：需要指定 geom = "histogram"。
# 折線圖：可以使用 geom = "line"。

mydata2 %>%
  sample_frac(0.02)
# sample_frac() 函數用來從資料框中隨機抽樣。
# 0.02 表示從 mydata2 中隨機抽取 2% 的觀測值（行數）。例如，如果 mydata2 中有 1000 行，那麼將會隨機選擇 20 行。
# 抽樣是隨機的，因此每次執行代碼時選中的觀測值可能會不同。
# set.seed(99)

mydata2 %>%
  sample_n(5)
#sample_n(5)、sample_frac(0.02) 都是隨機抽樣

library(summarytools)
mydata2 %>% 
  glimpse() #像str()
#glimpse() 會顯示資料框中的每個變數的名稱和資料類型，還會顯示每個變數的前幾個觀測值。這有助於你快速理解資料的結構。

mydata2 %>% 
  dfSummary() %>% 
  stview()
#stview()：
# 此函數將 dfSummary() 的結果轉換為一個互動式的 HTML 視圖，方便用戶瀏覽和探索摘要資料。
# 可以在 RStudio 的視圖窗格中查看，或在瀏覽器中打開，提供更友好的視覺效果。

bmw_df %>% 
  select(-c(id,title,body,posted,updated,drive,time)) %>% 
  descr() %>%  #numeric
  stview()
#使用負號 - 表示要排除這些變數。這通常是為了集中在數值變數上，去掉那些可能不影響數據分析的變數。
#descr() 函數來自 summarytools 套件，用於計算資料框中數值變數的描述性統計。
#它會提供每個變數的均值、標準差、最小值、最大值、四分位數等統計指標，幫助用戶了解數據的分佈情況。

bmw_df %>% 
  select(-c(id,title,body,posted,updated,drive,time)) %>% 
  freq() %>%  #category
  stview()

##tidyr
table1

table2
#FYI: table1 -> table2
table1 |> 
  pivot_longer(
    cols = cases:population,
    names_to = "type",
    values_to = "count"
  ) #翻譯年糕:拆掉
#

table2 %>% spread(type, count)
table2 %>%
  spread(key = type,
         value = count,
         sep = '_')

table2 |> 
  pivot_wider(
    names_from = type,
    values_from = count
  ) #翻譯年糕:合起來


table3
table3 %>% separate(rate, into=c("cases", "population"), sep = "/") #should not be character 
#默认情况下 separate() 不会自动转换数据类型，因此这两列的值都会被当作 字符型（character）。
#割后的 cases 和 population 列都将是字符型数据，不能直接进行数学运算。
table3 %>% separate(rate, into=c("cases", "population"), sep = "/", convert = TRUE)
#convert = TRUE 参数的作用是 自动将分割后的列转换为适当的类型，即如果分割后的值可以被解释为数字，它们就会被自动转换为数值型（numeric）。

# gather() 
table4a %>% gather(key = "year", value = "cases", `1999`:`2000`) -> tidy4a
tidy4a <- table4a %>% gather(key = "year", value = "cases", `1999`:`2000`)
tidy4a

table4a |> 
  pivot_longer(
    cols = `1999`:`2000`,
    names_to = "year",
    values_to = "cases"
  )


tidy4b <- table4b %>% gather(year, population, `1999`:`2000`) 
tidy4b

left_join(tidy4a, tidy4b)

table5 %>% unite(Year, century, year, sep="") %>% 
  separate(rate, into=c("cases", "population"), sep = "/", convert=TRUE)


### 合併資料
#library(readr)
student <- read_csv("lec2_data1.csv")
moodle <- read_csv("lec2_data2.csv")
course <- read_csv("lec2_data3.csv")

student %>% inner_join(moodle) #unmatched rows in either input are not included in the result

moodle2 <- moodle[c(1:3),]
student %>% inner_join(moodle2)  #有出現的才合併
student %>% full_join(moodle2) #全都合併，沒有的就填na

student2 <- student[-6,] #student[-6,]: 这表示从 student 数据框中移除第 6 行（-6 表示排除第 6 行），保留所有列（, 后面留空表示选择所有列）。
student2 %>% left_join(moodle2) 
student2 %>% right_join(moodle2)

#A left_join() keeps all observations in x.
#A right_join() keeps all observations in y.
#A full_join() keeps all observations in x and y

student %>% 
  left_join(moodle, by = "moodle_id") %>%  #如果有很多key，可指定 (by)
  left_join(course)

# |> shift+ctrl+m 快捷鍵  


##################
###  Exercise  ###
##################
#1 
install.packages("nycflights13")
library(nycflights13)
flights |>
  filter(dest == "LAX" | dest == "SFO"| dest == "ORD" | dest == "LGB") |> 
  arrange(year, month, day, arr_delay) |> 
  select( year, month, day, arr_delay, origin, dest,  dep_time )  
  
# can also 
flights %>% 
  filter( dest %in% c("LAX","SFO","ORD","LGB")) %>%
  select( year, month, day, arr_delay, origin, dest,  dep_time ) %>% 
  arrange(year, month, day, arr_delay)

#2
flights %>% group_by(dest) %>% tally() 
flights |>
  count(origin, dest, sort = TRUE)

#3
flights2 = flights |> 
  mutate(speed = distance / air_time * 60) |> 
  rename(YEAR = year)

#4
flights2 |> 
  group_by(month) |> 
  summarize(
    avg_speed = mean(speed, na.rm=T),
    n = n()
  )


## Exercise 2 ##
df = tibble(sub = c(1,2,3), treat = c("A","B","C"),
            t_1 = c(0.1,0.2,0.3), t_2 = c(4,5,6),
            t_3 = c(7,8,9), t_4 = c(1,1,1), t_5 = c(1.3,2.3,3.3)
)

# 1
df %>% gather(t,v,t_1:t_5) %>% arrange(sub,treat) -> df2 # or
df |> 
  pivot_longer(
    cols = t_1:t_5,
    names_to = "t",
    values_to = "v"
  )

# 2
df2 %>% spread(t,v) # or
df2 |> 
  pivot_wider(
    names_from = t,
    values_from = v
  )

# 3
df2 %>% separate(t, into=c("tt","number"), sep="_") -> df3

# 4
df3 %>% unite(t,tt,number)

# 5
df3 %>% unite(time, tt,number, sep="" ) 

# df->df3 
df |> 
  pivot_longer(
    cols = !c(sub,treat), 
    names_to = c("tt", "number"), 
    names_sep = "_", 
    values_drop_na = TRUE
  )


