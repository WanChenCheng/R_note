df <- data.frame(
  a = rnorm(10), # Generate 10 standard normal random numbers
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

#括號內為要輸入的參數
normalizedFun <- function(x){
  maxmin <- range(x, na.rm = TRUE) #range returns a vector containing the minimum and maximum of all the given arguments.
  ans <- (x - maxmin[1]) / (maxmin[2] - maxmin[1])
  #maxmin第一個為最小值，第二個為最大值
  return(ans)
}

normalizedFun(df$c)

bmi_cal = function(height, weight){
  height = height/100
  bmi = weight / height^2
  return(bmi)
}

bmi_cal(155,52)

if (sample(1:10,1) > 5) message("Bingo!")

if (sample(1:10,1) > 5) {
  message("Bingo!")
} else{
  message("Fail!")
}

ifelse(sample(1:10,1) >= 5, 'bingo', 'fail')

switch(2, #指定執行第二行程式碼，故回傳4
       2+3,   
       2^2,    
       3*6)  

my.lunch <- function(y){
  switch(y,
         poor="Noodle",
         ok="McD",
         rich="Starbucks")
}

my.lunch("poor")

for(bloodtype in c("A","B","O","AB")){
  cat(bloodtype, "\t")
} 
#"concatenate and print"（連接和打印），主要用來格式化輸出。這個函數常用於顯示訊息或變數的值。

for(bloodtype in c("A","B","O","AB")){
  cat("my blood type is", bloodtype, "\n")
}

# for loop
result <- 0

for(i in 1:100){ #for-loop裡，i會依序帶入1~100的值，重複進行括號內的程式碼
  
  # 迴圈內重複進行的動作
  result <- result + i
}

result


#apply and for loop

A<-c()
for( i in 1:4){
  A[i] <- mean(df[,i])
}


apply(df,2,mean) 
#apply(df, 2, mean)：apply() 函數是用來對資料框或矩陣的行或列進行操作的。
#df 是要操作的資料框。
#2 代表按列（columns）進行操作（如果是 1 則代表按行）。
#mean 是要應用的函數，這裡是計算每一列的平均值。




stFun <- function(x,y){
  x+y
}

do.call(stFun,list(1,2))
# same as stFun(1,2)

do.call(function(x,y) x+y, list(1,2))


bmi_cal = function(height, weight){
  height = height/100
  bmi = weight / height^2
  bmi_status = ifelse(bmi < 18.5,'過輕',
                      ifelse(bmi>=24,'過重','正常') )
  
  return(c(bmi,bmi_status))
}

