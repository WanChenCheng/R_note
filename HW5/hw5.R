# HW5
# Import the data
library(readr)
ytvideo <- read_csv("C:/Users/Ava/Desktop/R/HW5/ytvideo.csv")
View(ytvideo)

# import the packages
library(dplyr)
library(ggplot2)
library(tm)
library(wordcloud)
library(RColorBrewer)

# classification
ytvideo_filtered <- ytvideo %>%
  filter(yt == "蔡阿嘎") %>%
  select(title)

# data cleaning
text_data <- paste(ytvideo_filtered$title, collapse = " ")

clean_text <- text_data %>%
  tolower() %>%                                     
  gsub("[[:punct:]]", " ", .) %>%                   
  gsub("[^\\p{Han}]", " ", ., perl = TRUE) %>%                     
  gsub("[0-9]", " ", .) %>%                         
  gsub("[\\r\\n]", " ", .) %>%                      
  gsub("\\s+", " ", .) %>%                          
  trimws() 

library(jiebaR)
cutter <- worker()  # 建立 jieba 分詞器
word_tokens <- cutter[clean_text]  # 分詞

# remove stop words
stop_words <- c("的", "是", "了", "在", "我", "也", "和", "有", "這", "他", "她", "就", "不")
filtered_words <- word_tokens[!word_tokens %in% stop_words]
filtered_words

# wordcloud
word_freq <- table(filtered_words)
wordcloud(names(word_freq), 
          freq = word_freq, 
          min.freq = 2, 
          random.order = FALSE, 
          colors = brewer.pal(8, "Dark2"))

# visualization
word_freq_df <- as.data.frame(word_freq, stringsAsFactors = FALSE)
colnames(word_freq_df) <- c("word", "freq")

top_words <- word_freq_df %>%
  arrange(desc(freq)) %>%
  head(20)

ggplot(top_words, aes(x = reorder(word, freq), y = freq)) +
  geom_bar(stat = "identity", fill = "blue") +               
  geom_text(aes(label = freq), hjust = -0.2, size = 4) +     
  coord_flip() +                                             
  labs(title = "詞頻分析", x = "詞語", y = "頻率") +         
  theme_minimal()   
