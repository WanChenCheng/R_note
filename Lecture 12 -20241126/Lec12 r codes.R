##############
# Lecture 12 #
##############
#----------Text mining--------------------
#Choose one
install.packages("devtools")
library(devtools)
install_github("qinwf/jiebaRD")
install_github("qinwf/jiebaR")
library("jiebaR")
# or
install.packages("jiebaR")
library("jiebaR")
#
install.packages("tm")
library(tm)
install.packages("tmcn")
library(tmcn)
#--------------------------------------
#斷詞

cc = worker() #worker()函式是jiebaR用來新建一個分詞引擎
cc['今年母親節剛好是禮拜天啊!'] #way1
cc<='今年母親節剛好是禮拜天啊!' #way2

segment('今年母親節剛好是禮拜天啊!',cc) #way3

# add new word 要這個詞
new_user_word(cc,"今年母親節","n") #n是詞性（名詞）
segment('今年母親節剛好是禮拜天啊!',cc) 
cc['今年母親節剛好是禮拜天啊!']

add<-worker(user="add.txt") #note: encoding by UTF-8, save in ur working directory
segment('今年母親節剛好是禮拜天啊!',add)

# stop word 過濾這個字/詞
readLines("stop.txt") #note: encoding by UTF-8, 
cc3 <-worker(stop_word = "stop.txt")
segment('今年母親節剛好是禮拜天啊!',cc3) 
cc3['今年母親節剛好是禮拜天啊!']

filter<-c("小熊維尼")
filter_segment(cc["小熊維尼是卡通"] , filter)


cc <- worker(user = "add.txt", stop_word = "stop.txt", bylines = T)
cc['今年母親節剛好是禮拜天啊!']
# 要標點符號
cc3 <-worker(symbol =T)
segment('今年母親節剛好是禮拜天啊!',cc3) 


cc2<- worker("tag")#詞性
segment('今年母親節剛好是禮拜天啊',cc2)
cc2["我聽見 有 個 聲音"]

#ref:詞性介紹 http://blog.fens.me/r-word-jiebar/

# keyword
提取器 = worker("keywords", topn = 2)
keywords("我讀台灣政治大學", 提取器)

# frequency table
freq(c("台大","政大","政大"))

#-------
#桃花源記  
text<-c("初極狹，纔通人，復行數十步，豁然開朗，土地平曠，屋舍儼然，
        有良田、美池、桑竹之屬，阡陌交通，雞犬相聞，
        其中往來種作，男女衣著，悉如外人，黃髮垂髫，
        並怡然自樂，見漁人，乃大驚，問所從來，具答之，
        便要還家，設酒、殺雞、做食。村中聞有此人，咸來問訊，
        自云：先世避秦時亂，率妻子邑人來此絕境，不復出焉，
        遂與外人間隔。問今是何世，乃不知有漢，無論魏晉。
        此人一一為具言所聞，皆歎惋，餘人各復沿至其家，
        皆出酒食，停數日辭去，此中人語云：不足為外人道也")


cc<-worker()
cc[text]

keep <- c("大驚", "做食","外人") #不想拆分開的字
new_user_word(cc, keep) #增加字定義的辭彙
cc[text]

count <-freq(cc[text])  #can also use table(cc[text])
count


str(count)
count2<-count[nchar(count$char)>1,]#table #2個字以上的

#newd = data.frame(count2)
head(count2[order(count2$freq,decreasing = TRUE),],20)
newdd = count2[order(count2$freq,decreasing = TRUE),]
#------------------------------
#Visualized
#wordcloud

install.packages('wordcloud2') 
library('wordcloud2')
wordcloud2(newdd)
wordcloud2(newdd,shape='cardioid')
wordcloud2(newdd,shape='pentagon')
wordcloud2(newdd,shape='star',size=0.5)
wordcloud2(newdd,shape = 'triangle',backgroundColor='pink',size=0.3)
library(tidyverse)
wordcloud2(filter(count2, freq > 1), 
          fontFamily = "BiauKai", size = 0.4)


#bar chart
count2%>%
  filter(freq > 1) %>%
  mutate(word = reorder(char, freq)) %>% #reorder char by freq
  ggplot(aes(word,freq))+
  theme(text=element_text(family="wqy-microhei", size=14))+
  geom_col() +
  xlab(NULL) +
  ylab("Frequency")+
  coord_flip()
#繪圖亂碼 https://blog.gtwang.org/r/how-to-use-your-favorite-fonts-in-r-charts/
#Windows使用者:  Sys.setlocale(category = "LC_ALL", locale = "cht")
#Mac使用者: Sys.setlocale(category = "LC_ALL", locale = "zh_TW.UTF-8")

## chinese character
install.packages("showtext")
library(showtext)
# 自動使用 showtext 顯示文字
showtext_auto()
font_families()
## https://officeguide.cc/r-showtext-package-use-various-types-of-fonts-tutorial-examples/


#----------------------------------------
#
library(tm)
load("chou2022.dat") #8 songs from Jay Chou
cc<-worker()
choudf<-list()

for (i in 1:8){
  choudf[[i]]=cc[chou[[i]]]
}

#check
writeLines(as.character(chou[[3]])) #prints the entire content of 3rd document
choudf[[1]]
keep <- c("孟克","馬諦斯","才是") #不想拆分開的字
new_user_word(cc, keep) #增加字定義的辭彙
choudf<-list()

for (i in 1:8){
  choudf[[i]]=cc[chou[[i]]]
}

#------e.g. another way to import the file-- for lots of .txt files-------
doc1<-Corpus(DirSource("songdoc",pattern = ".txt",encoding="UTF-8",),readerControl = list(language ="UTF-8"))
doc1
writeLines(as.character(doc1[[1]]))
#---------------------------------------------

x <- VectorSource(choudf)
x <- Corpus(x) #collection of all text files 
tdm <- TermDocumentMatrix(x, control =list(wordLengths = c(2, Inf))) #挑兩個詞以上的
tdm #Term Document Matrix
inspect(tdm)

#clean text
x<-tm_map(x,stripWhitespace) #space
x<-tm_map(x,removeNumbers) #numbers
x<-tm_map(x,removePunctuation) #punctuation
x <- tm_map(x, function(word) {
  gsub("[A-Za-z0-9]", "", word)
}) #english and number



myStopWords <- c(stopwordsCN(), "的","就","了","卻","著" )#remove some words
x <- tm_map(x, removeWords, myStopWords)
head(myStopWords)

dtm<-DocumentTermMatrix(x)
inspect(dtm)
tdm <- TermDocumentMatrix(x, control =list(wordLengths = c(2, Inf))) #挑兩個詞以上的
inspect(tdm) 

m1 <- as.matrix(tdm)
v <- sort(rowSums(m1), decreasing = TRUE)
d <- data.frame(word = names(v), freq = v) #count freq
head(d)
wordcloud2(d,size=0.5)

apply(tdm, 2, sum) # term frequency

findFreqTerms(tdm, 11) #freq>11
findAssocs(tdm,"幸福",0.7) #association >0.7
inspect(removeSparseTerms(tdm, 0.6)) #remove sparse >0.6 
dtms <- t(as.matrix(tdm))
dtms[,which(colnames(dtms) %in% c("粉色","情書"))]

d %>%
  filter(freq > 6) %>%
  mutate(word = reorder(word, freq)) %>%
  ggplot(aes(word,freq))+
  theme(text=element_text(family="BiauKai", size=14))+
  geom_col() +
  xlab(NULL) +
  coord_flip()

#compare 周＆方
c_write<-m1[,3:4]
c_count<- sort(rowSums(c_write), decreasing = TRUE)
c_df <- data.frame(word = names(c_count), freq = c_count) #count freq


f_write<-m1[,5:8]
f_count<- sort(rowSums(f_write), decreasing = TRUE)
f_df <- data.frame(word = names(f_count), freq = f_count) #count freq

library(gridExtra)

c_df %>%
  filter(freq > 2) %>%
  mutate(word = reorder(word, freq)) %>%
  ggplot(aes(word,freq))+
  theme(text=element_text(family="BiauKai", size=14))+
  geom_col() +
  xlab(NULL) +
  ylab("周杰倫")+
  coord_flip()->plot1


f_df %>%
  filter(freq > 2) %>%
  mutate(word = reorder(word, freq)) %>%
  ggplot(aes(word,freq))+
  theme(text=element_text(family="BiauKai", size=14))+
  geom_col() +
  xlab(NULL) +
  ylab("方文山")+
  coord_flip()->plot2

grid.arrange(plot1, plot2, ncol=2)

#--------Clustering------------------------
install.packages("proxy")
library("proxy")
mydata <- as.data.frame(m1) #轉換分析數據為數據框結構
mydata <- as.data.frame(inspect(tdm)) #

mydata.scale<-scale(mydata) 

dd<-dist(mydata.scale,method="euclidean") 
h <- hclust(dd, method="ward.D2") 
plot(h,family="BiauKai",h=-1)  

# cosine similarity
cdist <- dist(mydata, method = 'cosine')
h <- hclust(cdist, method="ward.D2") 
plot(h,family="BiauKai")

# 對8個歌詞分群
cdist <- dist(t(m1), method = 'cosine')
h <- hclust(cdist, method="ward.D2") 
plot(h,family="BiauKai")

library(gplots)
par(family="BiauKai")
heatmap.2(m1,trace="none", dendrogram ="row",col=rev(heat.colors(10)))



#  web crawler  #
#--------------------------
library(rvest)
library(magrittr)
library(httr)


#---------------
###PTT
PTTNBA <- "https://www.ptt.cc/bbs/NBA/index.html"
pttContent <- read_html(PTTNBA)
post_title <- pttContent %>% html_nodes(".title") %>% html_text()
post_title
d=gsub(pattern = "\n\t+\n\t+", post_title, replacement = "")

cc[d]
content <- str_remove_all(d, "[0-9a-zA-Z]+?")
cc[content]

filter<-c("公告")
f=filter_segment(cc[content] , filter)
sort(table(f),decreasing = T)

newd=data.frame(table(f))
head(newd[order(newd$Freq,decreasing = TRUE),],20)

newdd=newd[order(newd$Freq,decreasing = TRUE),]
wordcloud2(newdd)





