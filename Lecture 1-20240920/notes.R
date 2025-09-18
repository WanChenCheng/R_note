a <- c(1,3,5,7,9,-10)
a
b <- 2:6
b
a[c(2,3,5)] #中括號給位置求數字
b[4]

b <- c("one", "two", "three","two") 
b == "two"
tf <- b == "two"
b[tf==TRUE]

hi <- "Hello World"
hi

str(a) # 屬性
class(a)

print(hi)

is.vector(hi)
is.numeric(hi)
is.character(hi)

length(a)

(tmp1 <- "I'm lovin it")
(tmp2 <- 'I\'m lovin it')
(tmp3 <- "I \"love\" it")
writeLines(tmp3)

x <- seq(1,10,2)
x
length(x)
x[3] #中括號吐出來是值
x[c(2,5)]
x[-c(2,5)]
x[-3]
x[3:length(x)]

x[1] + x[3]*x[4]/x[5]
round(x[4]/x[5])
x[4] %/% x[5] # integer division  整數除法
x[4] %% x[5] # remainder  餘數

sum(x)
mean(x)
sd(x)
var(x)
summary(x)

which( x>4 ) #指定、設條件，which吐出來的是位置
x[which( x>4 )]
which( x == 3)
which( x != 3)  # unequal
which(x>4 & x<=7)  # intersection
x[which(x>4 | x<=7)] # union

c(2, 3) %in% x # 配對，檢查數值2和3是否存在於向量x中
#1:10 %in% c(1,3,5,9)

x1 <- 1:6
x2 <- 2:4
intersect(x1, x2) # 交集
union(x1,x2) # 聯集

setdiff(x1,x2) # 找x1中跟x2不同的
setdiff(x2,x1) # 找x2中跟x1不同的
setequal(x1,x2) # 判斷x1, x2是否相同
setequal(x1,1:6)

a[3] <- 2 * a[3]
a

M <- matrix(c('a','b','c','d','e','f'), nrow = 3, ncol = 2)
M

matrix(c('a','b','c','d','e','f'), nrow = 3, ncol = 2, byrow = T)

list1 <- list(vector = c(2,5,3), avg = 21.3, func = mean)
list1
names(list1) <- c("a","b","c")

## factor
department <- c('IntBus','Banking','Statistics','Statistics','IntBus','BusAdm','Statistics')

# Create a factor object.
factor_dep <- factor(department) factor() #是用來將向量轉換為 因子（factor） 的函數
#是一種用來表示分類變量（categorical variables）的數據結構，特別適合用來表示具有有限取值的變量，例如部門名稱、性別、地區等。它能將這些分類變量轉換成類別資料，使得在進行統計分析時更具意義。

# Print the factor.
levels(factor_dep ) #類別名稱
nlevels(factor_dep ) #類別數量

factor_com <- factor(department, levels= c( "IntBus", "Banking","Account","Statistics","BusAdm","ManInfoSys","Finance", "RiskMan"))
#這行代碼創建了一個新的 factor 對象 factor_com，但這次你使用 levels 參數來明確指定所有可能的類別，即使某些類別（如 "Account", "ManInfoSys" 等）在原始資料中並不存在。R 會將這些額外的類別包含在 factor 的層級（levels）中，但它們的出現次數為零。
factor_com 
nlevels(factor_com )
factor(department, levels= c( "IntBus", "Banking","Account","Statistics","BusAdm","ManInfoSys","Finance", "RiskMan"),labels=c("IB", "Bank", "ACC", "Stat","BA","MIS","Fin","RM"))
#labels 參數將原來的類別名稱替換為更簡短的標籤

name <- c("Alex", "Bill", "Cathy","Dora")
age <- c("20", "23", "22","19")
gender <- c("Male", "Male", "Female", "Female")

# Create by variables
data1 <- data.frame(name, age, gender) #data frame（資料框）是一種常用的數據結構，類似於 Excel 表格或 SQL 表中的數據表。它用來儲存多種數據類型的數據，每列代表一個變量（可以是不同的數據類型），每行代表一個觀測值。
data1
data1$age <- as.numeric(data1$age) #錢錢符號代表"的"

unique(data1$gender)

rm(age) #移除變數


# Matrix
matrix()
cbind()
rbind()
rownames()
colnames()
is.matrix()
as.matrix()
rowSums()
colSums()
rowMeans()
colMeans()