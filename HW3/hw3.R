# Preparation

# 載入資料
library(readr)
data <- read_csv("C:/Users/Ava/Desktop/R/HW3/airline_survey.csv")
View(data)

# summary
#install.packages("summarytools")
library(summarytools)
dfSummary(data)
view(dfSummary(data)) # 看資料的各項分布
freq(data)
view(freq(data))

# 資料清洗
missing_arrival_delay <- data[is.na(data$`Arrival Delay in Minutes`), ]
sum(is.na(data$`Arrival Delay in Minutes`)) # 確認有幾筆缺失值
View(missing_arrival_delay) # 看有缺失值的筆數細項
data_cleaned <- na.omit(data) # 直接刪除有缺失值的資料
View(data_cleaned)

# Q2.Customer segmentation

#install.packages("factoextra")
library(factoextra)

# 決定最佳群數(使用肘部法則)
# fviz_nbclust(data_cleaned[,9:22], FUNcluster = kmeans, method = "wss", k.max = 20) +
#   labs(title="Elbow Method for K-Means") +
#   geom_vline(xintercept = 3, linetype = 2)
# (我電腦沒有40GB可以跑這行程式碼，所以後面最佳群數用假設的)

# K-means 分群
k = kmeans(data_cleaned[,9:22], centers=3) # 選取服務滿意度的欄位分群
str(k)
k$centers
k$withinss
k$tot.withinss
k$size
data_cleaned$Cluster <- k$cluster

# 視覺化分群結果
#install.packages("useful")
library(useful)

fviz_cluster(k, 
             data = data_cleaned[,9:22], 
             geom = c("point"),
             ellipse.type = "norm") +
  labs(title = "K-means Clustering",
       x = "Principal Component 1 (PC1)",
       y = "Principal Component 2 (PC2)")

# EDA
#install.packages("tidyverse")
library(tidyverse)

plot_satisfaction_proportion <- function(df, group) {
  service_cols <- names(df)[9:22]
  service_cols <- service_cols[sapply(df[service_cols], is.numeric)] 
  # 上面那行是用來確定每一個都是數值型態，理論上應該都是啦，但以防萬一檢查一下
  
  lowPer <- c()
  neuPer <- c()
  highPer <- c()
  
  for (col in service_cols) {
    rCount_low <- sum(df[[col]] %in% c(0, 1, 2), na.rm = TRUE)
    rPer_low <- round(rCount_low / nrow(df), 4)
    lowPer <- c(lowPer, rPer_low)
    
    rCount_neu <- sum(df[[col]] == 3, na.rm = TRUE)
    rPer_neu <- round(rCount_neu / nrow(df), 4)
    neuPer <- c(neuPer, rPer_neu)
    
    rCount_high <- sum(df[[col]] %in% c(4, 5), na.rm = TRUE)
    rPer_high <- round(rCount_high / nrow(df), 4)
    highPer <- c(highPer, rPer_high)
  }
  
  df_rate <- data.frame(
    Service = service_cols,
    Low = lowPer,
    Neutral = neuPer,
    High = highPer
  )
  
  df_rate_long <- df_rate %>%
    pivot_longer(cols = c("Low", "Neutral", "High"), names_to = "Level", values_to = "Proportion")
  
  plot <- ggplot(df_rate_long, aes(x = reorder(Service, Proportion, sum), y = Proportion, fill = Level)) +
    geom_bar(stat = "identity", position = "stack") +
    coord_flip() +
    labs(x = "Services", y = "Rate Proportion", title = paste(group, "Service Rate Proportion")) +
    scale_fill_manual(values = c("Low" = "lightgrey", "Neutral" = "skyblue", "High" = "pink")) +
    geom_text(aes(label = sprintf("%.2f", Proportion * 100)), position = position_stack(vjust = 0.5), size = 3) +
    theme_minimal() +
    theme(legend.title = element_blank())
  
  return(plot)
}

# 生成所有群體的圖形
df_split <- split(data_cleaned, data_cleaned$Cluster)
#上面那行是將資料分為三個子資料框，方便之後分開畫三個圖
plots <- lapply(names(df_split), function(Cluster) {
  plot_satisfaction_proportion(df_split[[Cluster]], Cluster)
})

# 顯示圖形（逐一顯示）
lapply(plots, print)

# Q1.Predict passenger satisfaction
#任選1種監督式學習方法配適模型，預測滿意度satisfaction (2類：滿意、中立 或 不滿意)。
#找出重要變數：哪些因素影響客戶滿意度。

# By隨機森林
# 載入套件
#install.packages("MASS")
library(MASS)
#install.packages("randomForest")
library(randomForest)

# 隨機分割訓練集和測試集
# 清理變數名稱
colnames(data_cleaned) <- make.names(colnames(data_cleaned), unique = TRUE)
print(colnames(data_cleaned))
trainI <- sample(1:nrow(data_cleaned), 51797)
traind <- data_cleaned[trainI,]
testd <- data_cleaned[-trainI,]

# 選擇需要的特徵欄位，並將滿意度變數轉為因子類型
traind_selected <- traind[, c(9:22, 25)]  # 滿意度在第25欄
testd_selected <- testd[, c(9:22, 25)]

traind_selected$satisfaction <- as.factor(traind_selected$satisfaction)
testd_selected$satisfaction <- as.factor(testd_selected$satisfaction)

# 確認資料結構
str(traind_selected)
str(testd_selected)

# 建立隨機森林模型
rf <- randomForest(satisfaction ~ ., data = traind_selected, importance = TRUE)
print(rf)

# 視覺化錯誤率隨著樹數的變化
plot(rf)
legend("topright", colnames(rf$err.rate), col = 1:4, cex = 0.8, fill = 1:4)

# 評估變數重要性
importance(rf)
varImpPlot(rf)
#Mean Decrease Accuracy - How much the model accuracy decreases if we drop that variable.
#Mean Decrease Gini - Measure of variable importance based on the Gini impurity index used for the calculation of splits in trees. 

# 預測測試集的滿意度
pred <- predict(rf, newdata = testd_selected)

# 混淆矩陣：實際值與預測值的對比
conf_matrix <- table(Real = testd_selected$satisfaction, Predict = pred)

#計算分數
#準確率（Accuracy）
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix) # diag(conf_matrix)是左上那一格
print(paste("Accuracy:", round(accuracy, 4)))

#精確率 (Precision) 和 召回率 (Recall)
precision <- diag(conf_matrix) / colSums(conf_matrix)
recall <- diag(conf_matrix) / rowSums(conf_matrix)
print(data.frame(Class = rownames(conf_matrix), Precision = precision, Recall = recall))

#F1 分數
f1_score <- 2 * (precision * recall) / (precision + recall)
print(data.frame(Class = rownames(conf_matrix), F1_Score = f1_score))

#混淆矩陣可視化
#install.packages("caret")
library(caret)
confusionMatrix(pred, testd_selected$satisfaction)

library(reshape2)
library(ggplot2)
conf_matrix_melt <- melt(conf_matrix)
ggplot(data = conf_matrix_melt, aes(x = Real, y = Predict, fill = value)) +
  geom_tile() +
  geom_text(aes(label = value), color = "white") +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Confusion Matrix", x = "Actual", y = "Predicted") +
  theme_minimal()


