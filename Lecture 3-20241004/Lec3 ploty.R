install.packages("plotly")

# Libraries
library(tidyverse)
library(plotly)

# Scatterplot
p = ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, color=Species, shape=Species)) + 
  geom_point(size=6, alpha=0.6)
p


ggplotly(p) #可互動式圖表

##--------------------------------------------------------------------
  
plot_ly(iris, x = ~Sepal.Length, y = ~Sepal.Width,  type="scatter", mode = "markers" , color = ~Species ,
       colors="Set1",
         marker=list( size=20 , opacity=0.5)  )

plot_ly(iris, x = ~Sepal.Length, y = ~Sepal.Width , type="scatter", mode = "markers",
        marker=list( size=20 , opacity=0.5), color = ~Sepal.Length ,
        colors=c("navyblue","royalblue","skyblue","yellow","pink" ))

# Histogram 
plot_ly(iris, x = ~Sepal.Length, type = "histogram")

# Box plot
plot_ly(iris, x = ~Sepal.Length, color = ~Species, type = "box")

# Bar plot
plot_ly(iris, x = ~Species, y = ~Sepal.Length, type = "bar", color = ~Species)

# separate by species
plot_ly(iris,x = ~Sepal.Length, y = ~Sepal.Width, type = "scatter", mode = "markers", sizes = c(10, 1000), frame = ~Species) 
 #mode = "markers" https://plot.ly/r/reference/#scatter-mode

#frame = ~Species：這是為動畫效果指定的參數。這裡的 frame 根據 Species（物種）創建不同的圖表幀。當你運行圖表時，可以看到每一個物種都有一個對應的幀，這使得圖表能夠進行動畫播放，展示數據的變化。

#3D
plot_ly(iris, x = ~Sepal.Length, y = ~Sepal.Width, z = ~Petal.Length, color = ~Species, colors = "Set2", size=9) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Sepal.Length'),
                      yaxis = list(title = 'Sepal.Width'),
                      zaxis = list(title = 'Petal.Length')))


# example in R package  
p <- plot_ly(iris, x = ~Sepal.Width, y = ~Sepal.Length) 
add_markers(p, color = ~Petal.Length, size = ~Petal.Length)
add_markers(p, color = ~Species)
add_markers(p, color = ~Species, colors = "Set1")
add_markers(p, symbol = ~Species)
add_paths(p, linetype = ~Species)

