install.packages("dslabs")
#dslabs 是一個適合學習數據科學的數據包，提供了豐富的數據集供初學者練習分析和建模。
library(dslabs)
data(murders) #把資料叫出來
help("murders") #反正會跑出東西讓你理解拉
class(murders) #看整個資料的資料型態
str(murders) # examing objects 看變數的資料型態
head(murders) # first 6 
murders$population # data$variable
names(murders) # variables' name

pop <- murders$population #他太長了改縮寫
length(pop) #how many

class(murders$region)
levels(murders$region) # levels of factor;
nlevels(murders$region)

region <- murders$region
value <- murders$total
region <- reorder(region, value, FUN = sum)
# reorder the levels of the region by the total number of murders

#reorder() 函數用來重新排列一個因子（factor）的層級（levels），以便根據一個數值變量的某種統計量來排序。
#x：要重新排列的因子變量。
#X：用來根據某個數值變量（例如總和、平均值等）進行排序。
#FUN：用來計算排序依據的函數（例如 sum、mean）。

#region：要重新排列的因子變量，這裡指的是 murders$region，表示各個地區（如 "Northeast", "South" 等）。
#value：一個數值變量，這裡是 murders$total，表示每個地區的謀殺總數。
#FUN = sum：表示按照每個地區的謀殺總數的總和來進行排序。

levels(region)
# Northeast has the least murders and the South has the most.


## e.g.
x <- c(29, 33, 10, 82, 46)
rbind(x,
      sort = sort(x), #原來的數據從小排到大
      order = order(x), #排順序，由小到大x 中的第 3 個元素（值為 10）應該放在最前面，接著是第 1 個元素（值為 29），然後是第 2 個元素（33），以此類推。
      rank = rank(x))  #10 的排名是 1，29 的排名是 2，33 的排名是 3，46 的排名是 4，82 的排名是 5。
##

sort(murders$total) #這行代碼將 murders$total（每個州的謀殺總數）從小到大進行排序，返回一個排序好的向量，但不會改變原來的數據集。
which(murders$total == 2) #誰的total是2，which跑出來是資料位置
murders$state[46] 

ord <- order(murders$total) # tell u which one
#order() 函數返回的是 排序後的索引，而不是排序本身。換句話說，它告訴你謀殺總數由小到大排序時，應該怎樣排列 murders$total 中的元素。
#例如，如果 ord 的結果是 c(46, 23, 10, 50, ...)，這意味著第 46 行的州謀殺總數最少，接著是第 23 行的州，依此類推。
murders$state[ord] 
#這行代碼根據謀殺總數從小到大排序，返回對應的州名。
#結果會是一個州名列表，從謀殺總數最少的州到最多的州。

max(murders$total)
i_max <- which.max(murders$total) #which.max() 函數返回的是最大值的索引位置。它告訴你哪個州的謀殺總數是最大的。
murders$abb[i_max] # abb 列存儲的是州的縮寫
## California is dangerous?

#
murder_rate <- murders$total / murders$population * 100000
round(murder_rate,2) #round() 函數將計算出的謀殺率四捨五入到小數點後 2 位。
murders$state[order(murder_rate)] #order(murder_rate) 返回的是按照謀殺率升序排列的索引。

ind <- murder_rate < 0.6
murders$state[ind]
sum(ind)

west <- murders$region == "West"
safe <- murder_rate <= 0.6

ind <- safe & west #同時滿足這兩個條件
murders$state[ind] 

ind <- which(murders$state == "Hawaii")
murder_rate[ind]

# want more state:
ind <- match(c("New York", "California", "Missouri"), murders$state) 
ind
murder_rate[ind]

# %>%
c("Taiwan", "New York") %in% murders$state

