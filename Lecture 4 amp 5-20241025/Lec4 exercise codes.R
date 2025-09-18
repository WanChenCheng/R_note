
##############
# Exercise 1 #
##############
my.stat <- function(x){
  x.number <- length(x)
  x.mean <- mean(x)
  x.sd <- sd(x)
  return(list(number = x.number, mean = x.mean, std = x.sd))
}
my.stat(df$a)

#can use c() insted of list()
my.stat2 <- function(x){
  x.number <- length(x)
  x.mean <- mean(x)
  x.sd <- sd(x)
  return(c(number = x.number, mean = x.mean, std = x.sd))
}

my.stat2(df$a)

##############
# Exercise 2 #
##############

two.sample.t <- function(x1, x2){
  n1 <- length(x1)
  n2 <- length(x2)
  m1 <- mean(x1)
  m2 <- mean(x2)
  v1 <- var(x1)
  v2 <- var(x2)
  sp <- ((n1-1)*v1 + (n2-1)*v2) / (n1+n2-2)
  t<- (m1-m2) / sqrt(sp*(1/n1+1/n2))
  return(c(mean = c(m1,m2), pooled.var = sp, t = t, pvalue = 2*pt(-abs(t),df=n1+n2-2)))
  #can use list(mean=c(m1,m2), pooled.var=sp, t=t, pvalue = 2*pt(-abs(t),df=n1+n2-2))
}

var1 <- rnorm(10,3,1)
var2 <- rnorm(10,0,1)
fake <- data.frame(var1,var2)

two.sample.t(fake$var1,fake$var2)
t.test(fake$var1,fake$var2,var.equal=T)


##############
# Exercise 3 #
##############
#one time
one = function() {
  deck <- c(rep("h", 13), rep("d", 13), rep("c", 13), rep("s", 13))
  hand <- sample(x = deck, size = 5)
  names(hand) <-paste("h", 1:length(hand), sep = "")
  diamonds <- sum(hand == "d")
  hearts <- sum(hand == "h")
  success <- diamonds == hearts
  data.frame(t(hand), diamonds, hearts, success)
}

#many times
many <- function(NNN) {
  replicate(NNN, one()$success)
}

#check results(show the details of 10 experiments to make sure one is behaving correctly.)
do.call(rbind, lapply(1:10, function(nnn) {
  one()
})) ##do.call(fun,data)

#calculate the probability
table(many(1000))

#theoretical calculation 
num <- choose(26, 5) + choose(13, 1)*choose(13, 1)*choose(26, 3) + choose(13, 2)*choose(13, 2)*choose(26, 1)
denom <- choose(52, 5)
num/denom

##############
# Exercise 4 #
##############
age <- sample(1:100, 20)
age_grp <- function(x){
  group.id <- ifelse(x <=20, "A",
                     ifelse(x>=21 & x<=40 , "B",
                            ifelse(x>=41 & x<=60, "C","D")))
  data.frame(age=x, group.id , row.names = NULL)
}
age_grp(age)

##############
# Exercise 5 #
##############
#(3種不同寫法: for, repeat, while )
factorial.for <- function(n){
  f <- 1
  for(i in 1:n){
    f <- f*i
  }
  f
}
factorial.for(5)


factroial.repeat <- function(n){
  f <- 1
  t <- n
  repeat{
    if(t < 2) break
    f <- f*t
    t <- t-1
  }
  return(f)
}
factroial.repeat(5)

factorial.while <- function(n){
  f <- 1
  t <- n
  while(t > 1){
    f <- f*t
    t <- t-1
  }
  return(f)
}
factorial.while(5)


##############
# Exercise 6 #
##############
result <- c() # 建立一個空的向量來放置每一次投擲的結果
coin <- c(1, 0) # 1 head，0 tail
i <- 1 # 從第一次投擲開始記錄

while (sum(result) < 3){
  result[i] <- sample(coin, size = 1) # 將每次投擲結果記錄起來
  i <- i + 1 # 準備記錄下一次的投擲結果
}
result # 印出每次投擲的紀錄
length(result) # 總共投擲了幾次

##############
# Exercise 7 #
##############

data(iris) 
n <- nrow(iris)
D <- matrix(0,n,n) #create a matrix to save the results
data <-iris[,-5]

for( i in 1:n){
  for(j in 1:n){
  D[i,j]<- (sum((data[i,]-data[j,])^2))^0.5
  }
}
D

dist(data, diag = T, upper=T)

  
  
  