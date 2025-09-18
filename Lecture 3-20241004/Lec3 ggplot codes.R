# categorical  
pie(table(iris$Species))
barplot(table(iris$Species))

# two variables: continuous n continuous
library(lattice)
xyplot(Sepal.Width ~ Sepal.Length, iris, groups = Species, pch= 20)

# two variables: continuous n discrete
boxplot(iris$Sepal.Length~iris$Species)
length.means = tapply(
  iris$Sepal.Length, 
  iris$Species, 
  mean)
barplot(length.means, 
         xlab = "Species", 
         ylab = "Mean of Sepal.Length")

# two variables: discrete n discrete
mean.index <- ifelse(iris$Sepal.Length>mean(iris$Sepal.Length),1,0)
mosaicplot(~mean.index+iris$Species,color=T)

# multivariables
plot(iris)


## 3D
#1 for each species#
library(lattice)
cloud(iris$Sepal.Length ~ iris$Sepal.Width*iris$Petal.Length|iris$Species, main="3D Scatterplot by Species")

#1 for all species, can rotate#
colors <- c("darkorange", "hotpink", "limegreen")
colors <- colors[as.numeric(iris$Species)]
library(rgl)
plot3d(iris[,1:3], col=colors)

----------------------------------------------------------------------
#####################################################################
# In order to get a gif, u need to download and install ImageMagick #
# http://www.imagemagick.org/script/download.php                    # 
#                                                                   #
# MAC: http://saosaomang.com/2016/11/imagemagick-for-mac/           #
# Windows: https://www.csie.ntu.edu.tw/~win/InstallImageMagick.htm  #
#                                                                   #
#####################################################################
install.packages("magick")
library(magick)
#or #windows
require(installr)
install.ImageMagick()

# or #mac
devtools::install_github("talgalili/installr")
install.packages("magick")
######

install.packages("animation")
library(animation)
install.packages("scatterplot3d")
library("scatterplot3d")
saveGIF({
  for(i in 1:30){
    scatterplot3d(iris[,1:3], color=colors, pch=20,
                  xlab="Sepal.Length", ylab="Sepal.width", zlab="Petal.Length",
                  angle=90 + 3.6 * 3 * i
    )
  }
}, movie.name="iris.gif")




###########
# ggplot2 #
###########

library(tidyverse)
iris %>%  
  ggplot(aes(x=Sepal.Length, y=Sepal.Width ,color=Species,shape=Species)) +
  geom_point() #scatterplot

iris %>%  
  ggplot(aes(x=Sepal.Length)) +
  geom_histogram() #histogram

ggplot(iris, aes(x=Sepal.Length, color=Species)) +
  geom_histogram(fill="white",bins=10)

ggplot(iris, aes(x=Sepal.Length, color=Species,fill=Species)) +
  geom_histogram(bins=10)

##-----------------------------------------------------------
library(cowplot)  
ii <- iris %>%  
  ggplot(aes(x=Sepal.Length, y=Sepal.Width ,color=Species,shape=Species)) +
  geom_point(size=2) +
  theme_cowplot()


#add background
png(file = "scatter.png", width = 3840, height = 2160, units = "px", res = 72*4) #export the plot
ggdraw() +
  draw_image("flower.jpeg", scale = 0.5) + # the background for the plot
  draw_plot(ii)
dev.off() #done, close the "png" devise

ggdraw(ii) + 
  draw_image("flower.jpeg", x = 1, y = 1, width = 0.2, height = 0.2,
             hjust = 1, vjust = 1)


#arrange plots
p <- ggplot(iris, aes(x = Sepal.Length, fill = Species)) + geom_density(alpha = 0.7)
p2 <- ggdraw() + draw_image("flower.jpeg", scale = 0.5)
plot_grid(p, p2, labels = "AUTO")
plot_grid(p, ii, labels = c("1","2"), ncol = 1, align = 'v')


##-----------------------------------------------------------

# correlation plot
install.packages("corrgram")
library(corrgram)
corr = cor(iris[,1:4])
corrgram(iris)

install.packages("corrplot")
library(corrplot)
corrplot(corr, method = "circle")

# heatmap
library(gplots)
heatmap.2(as.matrix(t(iris[,1:4])),dendrogram ="none",trace="none",Rowv = F,Colv = F)

heatmap.2(as.matrix(t(iris[,1:4])),dendrogram ="column",trace="none")

symnum(corr)

heatmap.2(corr, Rowv=FALSE, symm=TRUE, trace="none",
          cexRow=0.8, cexCol=0.8,srtCol=45,srtRow =0 )

library(RColorBrewer)
cols = colorRampPalette(rev(brewer.pal(9, "Reds")))(1000)
cols = colorRampPalette(brewer.pal(9, "Reds"))(1000)

heatmap.2(as.matrix(t(iris[,1:4])),dendrogram ="column",trace="none",
          cexRow=0.8, cexCol=0.8,srtCol=45,srtRow =0,
          col=cols,key=TRUE)

## FYI-------------------------------------------------------------
## 圖像 ##
# https://www.bioconductor.org/packages/3.7/bioc/vignettes/EBImage/inst/doc/EBImage-introduction.html

install.packages(c("tiff", "jpeg", "png", "fftwtools"))
library(jpeg)
flowerP <- readJPEG("flower.jpeg")
#print(flowerP) #array of pixel values.
(dims <- dim(flowerP))

#
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("EBImage")
browseVignettes("EBImage")
library(EBImage)
flowerP=readImage("flower.jpeg")
display(flowerP) 

display(flowerP, method="raster") #displays images as R raster graphics
text(x = 20, y = 20, label = "I prefer roses", adj = c(0,1), col = "red", cex = 2)

plot(c(0, dims[1]), c(0, dims[2]), type='n',xlab="", ylab="")
rasterImage(flowerP, 0, 0, dims[1], dims[2])


# Adjust Brightness (亮度)
Image1 <- flowerP + 0.5
Image2 <- flowerP - 0.5

display(Image1)
display(Image2)


# Adjust Contrast (對比)
Image3 <- flowerP * 0.5
Image4 <- flowerP * 2
display(Image3) 
display(Image4)

# Gamma Correction (光線的輝度（luminance）或是三色刺激值（tristimulus values))

Image5 <- flowerP ^ 2
Image6 <- flowerP ^ 0.7
display(Image5)
display(Image6)

# Cropping 修剪
display(flowerP[450:600,0:300, ]) #674*1000*3

# convert RGB to grayscale
d= flowerP[,,1:3]
colorMode(d) <- Grayscale
display(d)

colorMode(d) <- Color # change it back to color mode
display(d)


# Filtering 平滑、模糊

fLow <- makeBrush(21, shape= 'disc', step=FALSE)^2
fLow <- fLow/sum(fLow)
Image.fLow <- filter2(flowerP, fLow)
display(Image.fLow)

fHigh <- matrix(1, nc = 3, nr = 3)
fHigh[2, 2] <- -8
Image.fHigh <- filter2(flowerP, fHigh)
display(Image.fHigh)


# median filter (去雜訊)
# create a pic with noisy 
l = length(flowerP)
n = l/10
pixels = sample(l, n)
img_noisy = flowerP
img_noisy[pixels] = runif(n, min=0, max=1)
display(img_noisy)

medFltr <- medianFilter(img_noisy, 1.1) #filter out the noise
display(medFltr)


#########
# Shiny #
#########
install.packages("shiny")
install.packages("plotly")
install.pacakges("tidyverser")
library(shiny)
library(plotly)
library(tidyverse)

runExample()
runExample("01_hello")


