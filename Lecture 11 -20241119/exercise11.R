#1.How many variation can be explained by the first two PC?
#2.Interpret the results of scree plot.
#3.Interpret the results of PCA/SPCA.

#install.packages("HDclassif")
library(HDclassif)
data(wine)
View(wine)

## understand ur data
M = cor(wine)

## create heatmap for correlation 
library(reshape2)
melted_cormat <- melt(M)
head(melted_cormat)

###picture1
ggplot(data = melted_cormat,
       aes(Var1, Var2)) +
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient2(low = "firebrick4", high = "steelblue",
                       mid = "white", midpoint = 0) +
  guides(fill=guide_legend(title="Correlation")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title = element_blank())

###picture2
library(corrplot)
corrplot(M, method="circle")

## PCA ##-------------------------------------------------------------------
#install.packages("stats")
library(stats)
pca<- prcomp(wine, center = TRUE, scale = TRUE)

names(pca)
summary(pca)

plot(pca)

## how to get the results of summary(pca)
var = pca$sdev^2 
prop = (pca$sdev)^2 / sum((pca$sdev)^2) 
cum_prop = cumsum((pca$sdev)^2 / sum((pca$sdev)^2))

## scree plot: variance
screeplot(pca) 
plot(pca, type="line")
abline(h=1, col="blue") #Kaiser eigenvalue-greater-than-one rule, choose pc1~pc5 by Kaiser

summary(pca)

## Rotation matrix: Loadings are the percent of variance explained by the variable
pca$rotation

#visualize
ggplot(melt(pca$rotation[,1:5]), aes(Var2, Var1)) +
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient2(low = "firebrick4", high = "steelblue",
                       mid = "white", midpoint = 0) +
  guides(fill=guide_legend(title="Coefficient")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title = element_blank())

## The actual principal components
head(pca$x)

## Standard deviation of components is represents the percent of variation each component explains
pca$sdev

## Compute variance explained
pve=(pca$sdev)^2 / (sum(pca$sdev^2))
plot(pve, xlab="Principal Component", ylab="Proportion of Variance Explained ", ylim=c(0,1),type='b')#scree plot
plot(cumsum(pve), xlab="Principal Component", ylab="Cumulative Proportion of Variance Explained ", ylim=c(0,1),type='b')
abline(h=0.8)

## pc1 vs pc2 plot
biplot(pca,scale=T) #first two components
biplot(pca,scale=T,choices=2:3)
# another way 
#install.packages("ggfortify")
library(ggfortify)
autoplot(prcomp(data, center = TRUE, scale = TRUE), 
         loadings = TRUE, loadings.colour = 'black',
         loadings.label = TRUE, loadings.label.size = 5)

# by cluster 
group = as.factor(cutree(hclust(dist(data)),k=3))
data2 = cbind(data, group)
autoplot(prcomp(data, center = TRUE, scale = TRUE), data2, colour = 'group', 
         loadings = TRUE, loadings.colour = 'black',
         loadings.label = TRUE, loadings.label.size = 3)

## SPCA ##-------------------------------------------------------------------
#install.packages("nsprcomp")
library(nsprcomp)
spca <- nscumcomp(wine, k=40, nneg=T, scale=T)
summary(spca)
screeplot(spca)

ggplot(melt(spca$rotation), aes(Var2, Var1)) +
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient2(low = "white", high = "steelblue") +
  guides(fill=guide_legend(title="Coefficient")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title = element_blank())
