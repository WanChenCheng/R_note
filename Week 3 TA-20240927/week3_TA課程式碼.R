###Week3 code

##補充1：索引值
a <- seq(2,20,by=2)
a

a[4] #取第四個數值(從1開始算喔)
a[6]
a[c(4,6)]


b <- c(T,F,F) #看補充，R會把短的拿去配合長的
a[b] #看圖，把t部分拿出來

##補充2：rep()函數(產生相同元素的向量)
?rep #用法等同搜尋使用方法
rep(5,times=10)

rep(a,times=3) #重複a這個數列3次
rep(a,each=3) #把第一個元素重複三次，第二個元素重複三次，依此類推

rep(a,each=3,times=3)
rep(a,times=3,each=3)
rep(a,3,NA,3)

##補充3：NA(缺失值)
a[3] <- NA #把na放到第三個位置
a[8] <- NA
a

is.na(a) #a哪裡是有缺失值的
complete.cases(a) #這個數列哪裡有值(和上面那個相反)

which(is.na(a)) #哪一個位置是na (就不會一大堆t和f了)
which(complete.cases(a))

sum(is.na(a)) #把背後的數字加總(true=1 false=0)
sum(complete.cases(a))

##Example：iris dataset
data(iris) #它內建的不用install，data是把這筆資料叫出來
head(iris,10) #把前面10筆資料叫出來，預設6
tail(iris,8) #把後面8筆資料叫出來，預設6

length(iris$Sepal.Width) #變數下有幾筆資料 翻譯年糕:資料集的哪一個欄位，錢錢符號是"的"
nrow(iris) #行數
ncol(iris) #列數

table(iris$Species) #統計各物種的個數 (通常用在離散型的資料)
summary(iris) #資料基本資訊
class(iris) #資料結構 (整個的儲存格式)
str(iris) #資料內變數的結構 (變數的儲存格式)

str(iris$Sepal.Length)
str(iris$Sepal.Width)
str(iris$Species)
nlevels(iris$Species) #因子數量 (三種花的品種)
rownames(iris) #沒意義，觀測值數量
colnames(iris) #變數名稱
names(iris)

#修改因子名稱
iris$Species <- factor(iris$Species,labels=c('山鳶尾','變色鳶尾','維吉尼亞鳶尾'))
str(iris$Species)
levels(iris$Species)

#修改column的名稱
colnames(iris) <- c('花萼長度','花萼寬度','花瓣長度','花瓣寬度','物種')
head(iris)
colnames(iris) <- c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width","Species")
head(iris)

which.max(iris$Sepal.Length) #哪一個是最大的
iris[which.max(iris$Sepal.Length),1] #二維資料，手寫筆記裡面有!! 第一個看直的，第二個看橫的
iris[which.max(iris$Sepal.Length),] #把最大那個所有的資料抽出來

which.min(iris$Sepal.Length)
iris[which.min(iris$Sepal.Length),1]
iris[which.min(iris$Sepal.Length),]

which(iris$Species=='山鳶尾') #等於等於(==)用在判斷
iris[which(iris$Species=='山鳶尾'),]
length(which(iris$Species=='山鳶尾'))

which(iris$Species!='山鳶尾') #品種不等於山鳶尾
which(iris$Species=='山鳶尾'|iris$Species=='變色鳶尾') #直線槓槓是"或"

