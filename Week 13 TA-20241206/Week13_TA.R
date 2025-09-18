#install.packages("ggwordcloud")
library(ggwordcloud)
library(tidyverse)
data("love_words_small") #要使用的資料集
data("love_words") #完整的資料集

##原始文字雲
set.seed(123)
ggplot(love_words_small, aes(label = word)) +
  geom_text_wordcloud() +
  theme_minimal()

set.seed(321)
ggplot(love_words_small, aes(label = word)) +
  geom_text_wordcloud() +
  theme_minimal()

##調文字大小:size
set.seed(123)
ggplot(love_words_small, aes(label = word, size = speakers)) +  #size = speakers設定相對大小
  geom_text_wordcloud() +
  theme_minimal()

set.seed(123)
ggplot(love_words_small, aes(label = word, size = speakers)) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 25) +  #max_size設定整體大小
  theme_minimal()
#scale_radius(range = c(0, 30), limits = c(0, NA))：設定大小範圍

##調整角度:angle
love_words_small <- love_words_small %>%
  mutate(angle = sample(0:360,34,T))

set.seed(123)
ggplot(love_words_small, aes(
  label = word, size = speakers,
  angle = angle)) +
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 40) +
  theme_minimal()

##調整雲的垂直與水平軸長度比(預設：0.65:1)
set.seed(123)
ggplot(love_words_small, aes(label = word, size = speakers)) +
  geom_text_wordcloud_area(eccentricity = 0.65) +
  scale_size_area(max_size = 40) +
  theme_minimal()

set.seed(123)
ggplot(love_words_small, aes(label = word, size = speakers)) +
  geom_text_wordcloud_area(eccentricity = 1) +
  scale_size_area(max_size = 40) +
  theme_minimal()

set.seed(123)
ggplot(love_words_small, aes(label = word, size = speakers)) +
  geom_text_wordcloud_area(eccentricity = 0.3) +
  scale_size_area(max_size = 40) +
  theme_minimal()

##調整顏色
library(RColorBrewer)
display.brewer.all()
my_palette <- brewer.pal(12,name="Set3") 
#補充:看色票顏色
barplot(1:12, col = my_palette)

#隨機指派顏色
set.seed(123)
ggplot(
  love_words_small,
  aes(
    label = word, size = speakers,
    color = sample(my_palette,34,replace=T),
  )
) +
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 40) +
  theme_minimal()

#根據數值大小指派顏色深淺
set.seed(123)
ggplot(
  love_words_small,
  aes(
    label = word, size = speakers,
    color = speakers
  )
) +
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 40) +
  theme_minimal() +
  scale_color_gradient(low = "blue", high = "red")

####調整形狀
###內建形狀
for (shape in c(
  "circle", "cardioid", "diamond",
  "square", "triangle-forward", "triangle-upright",
  "pentagon", "star"
)) {
  set.seed(123)
  print(ggplot(love_words_small, aes(label = word, size = speakers)) +
          geom_text_wordcloud_area(shape = shape) +
          scale_size_area(max_size = 40) +
          theme_minimal() + ggtitle(shape))
}

###自訂形狀
library(png)
library(grid)

#範例一：TA家的貓
shape=readPNG("咪卡.png")

set.seed(123)
ggplot(love_words, aes(label = word, size = speakers)) +
  geom_text_wordcloud_area(
    mask =shape,
    rm_outside = TRUE
  ) +
  scale_size_area(max_size = 42) +
  theme_minimal()

#範例二
shape2=readPNG("小八貓.png")

set.seed(123)
ggplot(love_words, aes(label = word, size = speakers)) +
  geom_text_wordcloud_area(
    mask =shape2,
    rm_outside = TRUE
  ) +
  scale_size_area(max_size = 42) +
  theme_minimal()
