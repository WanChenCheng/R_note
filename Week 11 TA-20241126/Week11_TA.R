library(AER)
library(tidyverse)
library(pROC)
library(caret)

data("CreditCard")
str(CreditCard)
?CreditCard
table(CreditCard$card)

CreditCard$index=1:nrow(CreditCard)
set.seed(123)
train <- CreditCard %>% sample_frac(0.8) #80%當訓練集
test <- CreditCard %>% anti_join(train,by='index') #20%當測試集

###建模1：預測yes的機率
model <- glm(card ~ reports+age+income+owner+active,data = train, 
             family = binomial)

###建模2：預測no的機率
train2 <- train
train2$card <- factor(train$card,levels = c('yes','no'))
str(train$card)
str(train2$card)

model2 <- glm(card ~ reports+age+income+owner+active,data = train2, 
             family = binomial)

###比較兩者預測出來的機率
pred=predict(model,train %>% select(reports,age,income,owner,active),type = 'response')
pred2=predict(model2,train2 %>% select(reports,age,income,owner,active),type = 'response')
head(pred)
head(pred2)
head(pred)+head(pred2)

###以下以model當例子
###在訓練集畫ROC曲線尋找閾(ㄩˋ)值
summary(model)

pred=predict(model,train %>% select(reports,age,income,owner,active),type = 'response')

roc=roc(train$card,pred) #real,predict
plot(roc,
     main="ROC曲線最佳閾值",
     thresholds="best", 
     print.thres="best")

p=as.numeric(coords(roc, "best")[1])
p
#AUC
roc$auc

###在測試集看成效

#以yes當作positive
pred_test <- predict(model,test %>% select(reports,age,income,owner,active),type = 'response')
pred_test_result = factor(ifelse(pred_test>p,'yes','no'))
con <- confusionMatrix(pred_test_result,test$card,positive = 'yes') #predict,real
con
con$byClass
con$positive

#以no當作positive
pred_test <- predict(model,test %>% select(reports,age,income,owner,active),type = 'response')
pred_test_result = factor(ifelse(pred_test>p,'yes','no'))
con2 <- confusionMatrix(pred_test_result,test$card,positive = 'no')
con2
con2$byClass
con2$positive

