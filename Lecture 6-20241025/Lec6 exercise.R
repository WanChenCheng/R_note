#Exercise sale
saledf <- read.csv("sale.csv",sep=",") 

#
ggplot(data = saledf) + geom_boxplot(aes( x= Store, y= Sale, colour = Store)) + 
  labs( x = 'Store',
        y = 'Sales',
        title = 'Sales Distribution by Store')

#
ggplot(data = saledf) + geom_boxplot(aes( x= Type, y= Sale, colour = Type)) + 
  labs( x = 'Type',
        y = 'Sales',
        title = 'Sales Distribution by Type')

#
saledf$Month <- factor(saledf$Month, levels = c('January','February','March','April','May','June','July','August','September','October','November','December'))

monthavg <- saledf %>% 
  group_by( Store,Month) %>%
  summarise( SaleAvg= mean(Sale))


ggplot(data=monthavg, aes( x = Month, y = SaleAvg, group = Store)) + 
  geom_line()+
  geom_point()+
  facet_wrap(~Store) +
  labs( x = 'Month',
        y = 'Sales',
        title = 'Sales Distribution by Month') 

#
saledf %>%
  ggplot(aes(x=ADprice,y=Sale))+
  geom_point()



### fitting and summarizing regressions in R
DataA <- saledf %>%
  filter( Store=='A')

fit.1 <- lm(Sale ~ ADprice, data=DataA)
summary(fit.1)
print(fit.1)

tidy(fit.1)
glance(fit.1)
augment(fit.1) 

fit.2 <- lm(Sale ~ Type , data=DataA)
summary(fit.2)


fit.3 <- lm(Sale ~ Type+ADprice+Month , data=DataA)
summary(fit.3)


# Exercise Wine

wine <- read.csv("wine.csv")
wine$Cultivar <- factor(wine$Cultivar)
ggplot(wine, aes(x = TotalPhenol, y = Flavanoids, color = Cultivar)) + geom_point() + geom_smooth(method = "lm", aes(group = Cultivar))
fit <- lm(Flavanoids ~ TotalPhenol * Cultivar, data = wine)
tidy(fit)
glance(fit)

fit <- lm(Color ~ ., data = wine)
tidy(fit)
glance(fit)
step(fit)
leaps=regsubsets(Color~.,data=wine)
plot(leaps, scale="adjr2")

fit <- aov(Color ~ Cultivar, data = wine)
tidy(fit)
TukeyHSD(fit)

