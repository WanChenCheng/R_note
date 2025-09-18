y <- rnorm(5)
x <- rnorm(5)
library(broom)
fit <- lm(y~x)
tidy(fit)
glance(fit)
augment(fit) 

str(tidy(fit)) 
str(glance(fit))
#===================

library(foreign)
kidiq <- read.dta("C:\\Users\\Ava\\Downloads\\kidiq.dta")

### fitting and summarizing regressions in R
attach(kidiq) #just work on kidiq
plot(mom_iq,kid_score,xlab="Mother IQ score", ylab="Child test score") #先確認有沒有線性關係

fit.1 <- lm (kid_score ~ mom_iq)
summary(fit.1)
print(fit.1)

#library(broom)
tidy(fit.1)
glance(fit.1)
### 模型指標解釋

1. **r.squared (R²)**  
  - 解釋變異的比例。  
- 此處為 0.201，表示模型可以解釋 20.1% 的目標變數變異，說明模型的解釋力有限。

2. **adj.r.squared (調整後的 R²)**  
  - 考慮了模型中的變數數量，避免過度擬合。  
- 此處為 0.199，與 R² 非常接近，表示變數數量適當。

3. **sigma**  
  - 殘差標準誤（Residual Standard Error, RSE）。  
- 此處為 18.3，表示模型殘差的標準差，越小越好。

4. **statistic**  
  - F 統計量，用於檢驗模型整體是否顯著。  
- 此處為 109，通常越大越顯示模型顯著性越高。

5. **p.value**  
  - F 檢定的 p 值，用於檢驗模型顯著性。  
- 此處為 \(7.66 \times 10^{-23}\)，非常小（接近 0），表明模型整體顯著。

6. **df (自由度)**  
  - 與變數數量和數據點數量相關。

7. **logLik**  
  - 模型的對數似然值，表示模型對數據的擬合程度。  
- 值越大越好。

8. **AIC (Akaike 信息準則)**  
  - 用於模型選擇，值越小越好，表明模型更優。

9. **BIC (貝葉斯信息準則)**  
  - 與 AIC 類似，但對模型的複雜度有更嚴格的懲罰。  
- 值越小越好。
augment(fit.1) 

fit.2 <- lm (kid_score ~ mom_hs + mom_iq)
summary(fit.2)


### graphical displays of data and fitted models

plot(mom_iq,kid_score,xlab="Mother IQ score", ylab="Child test score")
curve (coef(fit.1)[1] + coef(fit.1)[2]*x, add=TRUE,col="red") #or abline
abline(fit.1)
#alternately (in matrix form)
curve (cbind(1,x) %*% coef(fit.1), add=TRUE,col="blue") 
#or use ggplot
library(tidyverse)
ggplot(kidiq, aes( mom_iq,kid_score)) + geom_point() + geom_smooth(method="lm") 



## model with no interaction
fit.2 <- lm (kid_score ~ mom_hs + mom_iq)
colors <- ifelse (mom_hs==1, "black", "gray") #0 no hs
plot (mom_iq, kid_score, xlab="Mother IQ score", ylab="Child test score",
      col=colors, pch=20) 
curve (cbind (1, 1, x) %*% coef(fit.2), add=TRUE, col="black") #?
curve (cbind (1, 0, x) %*% coef(fit.2), add=TRUE, col="gray") #?

##fit a regression line for each group
library(tidyverse)
kidiq %>% group_by(mom_hs) %>% do(tidy(lm(kid_score ~ mom_iq, .))) 

# alternative sequence of commands
plot(mom_iq,kid_score,xlab="Mother IQ score",ylab="Child test score",type="n")
points (mom_iq[mom_hs==1], kid_score[mom_hs==1], pch=20, col="black")
points (mom_iq[mom_hs==0], kid_score[mom_hs==0], pch=20, col="gray")
curve (cbind (1, 1, x) %*% coef(fit.2), add=TRUE, col="black")
curve (cbind (1, 0, x) %*% coef(fit.2), add=TRUE, col="gray")


## model with interaction
fit.3 <- lm (kid_score ~ mom_hs + mom_iq + mom_hs:mom_iq)
tidy(fit.3)
fit.4 <- lm (kid_score ~ mom_hs*mom_iq) #the same as fit.3
tidy(fit.4)
colors <- ifelse (mom_hs==1, "black", "gray")
plot (mom_iq, kid_score, xlab="Mother IQ score", ylab="Child test score",
      col=colors, pch=20)
curve (cbind (1, 1, x, 1*x) %*% coef(fit.3), add=TRUE, col="black")
curve (cbind (1, 0, x, 0*x) %*% coef(fit.3), add=TRUE, col="gray")
#by ggplot
ggplot(kidiq, aes(x = mom_iq, y = kid_score, color = as.factor(mom_hs))) +
  geom_point() +
  geom_smooth(method = "lm", aes(group = as.factor(mom_hs)))

#interaction plot
kidiq %>%
  group_by(mom_hs, mom_work) %>%
  summarize(mean= mean(kid_score)) -> temp

ggplot(temp, aes(x=as.factor(mom_hs),
                      y=mean,
                      group=as.factor(mom_work),
                      color=as.factor(mom_work))) + geom_point() + geom_line()


# model compared
f = lm(kid_score ~ mom_hs+mom_iq)
f2 <- lm (kid_score ~ mom_hs*mom_iq)
tidy(f)
tidy(f2)
anova(f,f2) #with interaction term is better, H0:no difference, Ha: complicated one is better

### other useful function
coef(fit.2)
confint(fit.2, level=0.95)
fitted(fit.2)
resid(fit.2)

### Prediction
x.new <- data.frame(mom_hs=1, mom_iq=100)
predict(fit.2, x.new, interval="prediction", level=0.95)


### using discrete rather than continuous predictors
lm(formula = kid_score ~ as.factor(mom_work))
lm(formula = kid_score ~ mom_work) #R will consider mom_work as numerical var
contrasts(as.factor(mom_work)) #the coding that R uses for the dummy variables.



### error assumption (residual analysis)

res = resid(fit.1) #residuals. plot(fitted y,res)
par(mfrow = c(2, 2))
plot(fit.1)
#1. This plot shows if residuals have non-linear patterns.
#2. This plot shows if residuals are normally distributed. 
#3. This is how you can check the assumption of equal variance (homoscedasticity)
#4. This plot helps us to find influential cases (i.e., subjects) if any. 

plot(hatvalues(fit.1)) #Leverage statistics槓桿值
which.max(hatvalues(fit.1))

library(car)
durbinWatsonTest(fit.1) #check for independence library(car)

e_z=rstandard(fit.1) #standardized residual or rstudent(fit.1)
yhat=fit.1$fitted.values
plot(yhat,e_z)
abline(h=0)
abline(h=1,lty=3)
abline(h=-1,lty=2)

# relationship between each paired vars
pairs(~ mom_hs + mom_iq + mom_age, data=kidiq)

c= cooks.distance(fit.1) #>=1, might be outlier, can remove
plot(c)
which(c>0.04)
points(7,c[7],col='red')






### Model select
anova(fit.1,fit.2) #fit2

### LRT
library(broom)
glance(fit.1) %>% select(adj.r.squared, df, logLik)
glance(fit.2) %>% select(adj.r.squared, df, logLik)
D <- 2 *( (-1872)- (-1876))
df <- 2-1
1 - pchisq(D, df) #sig. means Evidence	for	model	improvement	
#P(X<x)


### AIC, BIC
library(tidyverse)
glance(fit.1) %>% select(AIC,BIC)
glance(fit.2) %>% select(AIC,BIC)
glance(fit.3) %>% select(AIC,BIC)
glance(fit.4) %>% select(AIC,BIC)


### stepwise
full <- lm(kid_score~.,data=kidiq)
glance(full) %>% select(AIC,BIC)
null <-lm(kid_score~1,data=kidiq)
#AIC
step(full, direction="backward")
step(null, scope=list(lower=null, upper=full), direction="forward")

#BIC
step(full, direction="backward", criterion = "BIC")

library(MASS)
step <- stepAIC(fit.3, direction="both")
step$anova

#By plot
library(leaps)
leaps=regsubsets(kid_score~.,data=kidiq,nbest=13)
plot(leaps, scale="adjr2")
plot(leaps, scale="bic")


# est. by matrix form: b=(X'X)^(-1)X'Y
n=length(mom_iq)
x=as.matrix(cbind(rep(1,n),mom_iq),2,n)
y=as.matrix(kid_score,1,n)

solve(t(x) %*% x) %*% t(x) %*% y
solve(crossprod(x), crossprod(x,y))

# Transformation
#log y= b0+b1X1+....
#y=exp(bo+b1X1+...)

f=lm(formula = I(log(kid_score)) ~ mom_iq)
exp(f$coef[2]) #expected positive difference of about 0.7% in y

x = seq(1,7)
y = c(27,36,45,181,306,1093,2459)
df = data.frame(x,y)
plot(x,y) #exponential growth in y, then take log10
f = lm(I(log10(y)) ~ x)
summary(f)
plot(x,I(log10(y))) #looks like linear relationship
abline(f)
plot(x,f$res)

##I()
#lm(Y~x1+x2)
#lm(Y~I(x1+x2)) #create a variable by x1+x2

logy = transform(df$y, method ="log")
plot(df$x,logy$X_data) #looks like linear relationship
#library(dlookr)
#find_skewness(data) #Find skewed variable


coef(f)
log.yhat = coef(f)[1] + coef(f)[2]*8 #if x=8
log.yhat
x.new=data.frame(x=8)
predict(f,x.new)

yhat = 10^log.yhat
yhat

#I() "as.is", return a vector of values raised to the second power.


