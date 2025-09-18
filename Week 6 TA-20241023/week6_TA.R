library(ggplot2)
#Taking Input from User in R Programming
var = readline()
var

# convert the inputted value to integer
var = as.integer(var)

# print the value
print(var)

#add prompt string
name <- readline(prompt = "請輸入你的姓名：")
print(paste("你好，", name, "！歡迎使用R"))
var1 = readline(prompt = 'Enter any number : ')
print(var1)

#multiple inputs 
var1 = readline('Enter 1st number : ')
var2 = readline('Enter 2nd number : ') 
var3 = readline('Enter 3rd number : ') 
var4 = readline('Enter 4th number : ')

{ 
  var1 = readline('Enter 1st number : ')
  var2 = readline('Enter 2nd number : ') 
  var3 = readline('Enter 3rd number : ') 
  var4 = readline('Enter 4th number : ')
}


#互動式BMI計算器
bmi_cal = function(){
  {
    height = as.numeric(readline('請輸入身高(公分): '))/100
    weight = as.numeric(readline('請輸入體重(公斤): '))
  }
  bmi = weight / height^2
  
  paste('BMI：',bmi,sep='')
}
bmi_cal()

#Fibonacci數列
#1,1,2,3,5,8,13,...
#n是「數列長度」
fibo <- function(n){
  if (n>1) {
    a = c(1,1)
    i=1
    while (length(a)<n){
      a = c(a, a[i]+a[i+1])
      i = i+1
    }
    print(a)
  }else{1}
}

fibo(1)
fibo(4)
fibo(10)


#自訂函數：將資料中所有factor型態資料畫圓餅圖
#sapply(x,FUN)
#apply(x,margin,FUN) (margin:row=1,column=2) 
data(diamonds)
str(diamonds)
is.factor(diamonds$cut)

sapply(diamonds[,c(1,5:10)],sum) #正確
apply(diamonds[,c(1,5:10)],2,sum) #正確

sapply(diamonds,class) #正確
apply(diamonds,2,class) #不正確

sapply(diamonds,is.factor) #正確
apply(diamonds,2,is.factor) #不正確

#利用迴圈多張圖
超pie <- function(data){
  index=sapply(data,is.factor)
  data=data[,index]
  for (i in 1:ncol(data)){
    c1=table(data[,i])
    c1=as.data.frame(c1)
    piepercent<- paste(round(100*c1$Freq/sum(c1$Freq), 2), "%")
    pie(c1$Freq,labels =piepercent, main = colnames(data)[i],col = rainbow(nrow(c1)))
    legend("topright",as.character(c1[,1]), cex = 0.8,
           fill = rainbow(nrow(c1)))
  }
}

index=sapply(diamonds,is.factor)
diamonds[,index]
barplot(1:20,col=rainbow(20))

超pie(diamonds)
超pie(iris)

str(iris[,5])
ncol(iris[,5])

超pie_2 <- function(data){
  index=sapply(data,is.factor)
  if (sum(index)>1){
    data=data[,index]
    for (i in 1:ncol(data)){
      c1=table(data[,i])
      c1=as.data.frame(c1)
      piepercent<- paste(round(100*c1$Freq/sum(c1$Freq), 2), "%")
      pie(c1$Freq,labels =piepercent, main = colnames(data)[i],col = rainbow(nrow(c1)))
      legend("topright",as.character(c1[,1]), cex = 0.8,
             fill = rainbow(nrow(c1)))
    }
  }else if (sum(index)==1){
    name=colnames(data)[index]
    data=data[,index]
    c1=table(data)
    c1=as.data.frame(c1)
    piepercent<- paste(round(100*c1$Freq/sum(c1$Freq), 2), "%")
    pie(c1$Freq,labels =piepercent, main = name,col = rainbow(nrow(c1)))
    legend("topright",as.character(c1[,1]), cex = 0.8,
             fill = rainbow(nrow(c1)))
    }else{print('無類別變數')}
    
  }

超pie_2(diamonds)
超pie_2(iris)
超pie_2(diamonds[,c(1,5:10)]) #選出diamonds中的連續型變數

#存成PDF
pdf('超派.pdf')
超pie_2(diamonds)
dev.off()

getwd()

# while迴圈 vs for迴圈
