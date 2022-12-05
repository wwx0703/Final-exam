require(readr)
require(ggplot2)
require(dplyr)
require(tidyr)
require(caret)
require(corrplot)
require(Hmisc)
require(parallel)
require(doParallel)
require(ggthemes)


## read data
logistics<- read.csv("C:/Users/HONGYAN/Desktop/logistics.csv")
describe(logistics)

head(logistics)         
tail(logistics)

summary(logistics$sales.RMB.)
summary(logistics$quantity)

#remove rows with missing values 
logistics <- na.omit(logistics)
logistics

#convert all character variables to factor variables
library(dplyr)
logistics %>% mutate_if(is.character, as.factor)

## group_by
install.packages("magrittr")
library(magrittr)
library(dplyr)

## add month list
logistics<- mutate(logistics,month=as.POSIXlt(sales.time)$mon+1)
month<- group_by(logistics,month)
head(logistics)

## month mothod2
library(timechange)
library(lubridate)

month(logistics$sales.time)

month<-strsplit(logistics$sales.time,split = "/") # strsplit()

month1<-do.call(rbind, month)[,2]# list-table

logistics$month1<-month1

## Month &&  mean of sales
newlogistics<- mutate(logistics,month=as.POSIXlt(sales.time)$mon+1)%>%
  group_by(month) %>%
  summarise(sales.amount=mean(sales.RMB.,na.rm=T))

Month<- group_by(newlogistics,month)

head(Month)

## ts method
salesTS <-ts(logistics$sales.RMB.[1:6],frequency=12,start=c(2016,7,1),end=c(2016,12,30))  
print(salesTS)
str(salesTS)
attributes(salesTS)

par(mar = c(1, 1, 1, 1))

plot(salesTS,col="red")

## mean and sd
quantity <- as.numeric(logistics[,8])
sales.amount <- as.numeric(logistics[,9])

print(mean(quantity, na.rm = T))
print(sd(quantity, na.rm = T))

print(mean(sales.amount, na.rm = T))
print(sd(sales.amount, na.rm = T))

## single values
salestime<- c(logistics$sales.time)

delivery<- c(logistics$delivery)
table(delivery)
  
goods<- c(logistics$goods)
table(goods)

feedback<- c(logistics$product.user.feedback)
table(feedback)

area<- c(logistics$sales.area)
table(area)

quantity<- c(logistics$quantity)

sales<- c(logistics$sales.RMB.)

## table()
df <- data.frame(month,delivery,goods,feedback,area,quantity)
df2 <- subset(df,subset = goods==c(goods),select = c(goods,feedback))
df2

table(df2)

##multidimensional contingency table
table(logistics$month,logistics$sales.area,logistics$goods)
xtabs(~month+sales.area+goods,data = logistics)

## % rate
table(logistics$month,logistics$delivery)
prop.table(table(logistics$month,logistics$delivery),1)


## month && delivery status && on time rate
month1 <- table(logistics[,'delivery'])
print(month1)
barplot(month1)

barplot(month1, main="delivery", 
        col=c("brown2", "aquamarine1"))

##combine
month2 <- with(logistics, table(month,delivery))
month2
barplot(month2, legend=TRUE)
barplot(month2, beside=TRUE, legend=TRUE)



## area && delivery status && on time rate
table(logistics$sales.area,logistics$delivery)
prop.table(table(logistics$sales.area,logistics$delivery),1)

##combine
area1 <- with(logistics, table(area,delivery))
area1
barplot(area1, legend=TRUE)
barplot(area1, beside=TRUE, legend=TRUE)

barplot(area1, beside=TRUE, legend=TRUE,
        main='areas in delivery',
        ylim=c(0, 320),
        xlim=c(-1, 6), width=0.2,
        col=c("brown2", "aquamarine1","blue","green","red","white"))


## goods && delivery status && on time rate
table(logistics$goods,logistics$delivery)
prop.table(table(logistics$goods,logistics$delivery),1)

goods1 <- table(logistics[,'delivery'])
print(goods1)
barplot(goods1)

barplot(goods1, main="delivery", 
        col=c("brown2", "aquamarine1"))

##combine
goods2 <- with(logistics, table(goods,delivery))
goods2
barplot(goods2, legend=TRUE)
barplot(goods2, beside=TRUE, legend=TRUE)
barplot(goods2, beside=TRUE, legend=TRUE,
        main='goods and delivery',
        ylim=c(0, 150),
        xlim=c(-1, 6), width=0.15,
        col=c("brown2", "aquamarine1","blue","green","red","white"))

##  setup
install.packages("ggplot2")
install.packages("ggthemes")

set.seed(123)
library(ggplot2)
library(ggthemes)

p <- ggplot(data = logistics,
            mapping = aes(x = goods))
p + geom_bar()

##
df3 <- logistics %>%
  select(goods,month) %>%
  group_by(goods,month) %>%
  summarise(n = n()) %>%
  mutate(ratio = n / sum(n)) %>%
  ungroup()

p <- ggplot(data = df3,
            mapping = aes(
              x = bigregion, 
              y = n,
              fill = religion))

p + geom_col()


##
install.packages("magrittr") # package installations are only needed the first time you use it
install.packages("dplyr")    # alternative installation of the %>%

library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%


## plot
plot(logistics$goods, logistics$quantity)
with(logistics,
     plot(goods, quantity))
with(logistics,
     plot(goods, quantity,
          main='goods and quantity',
          xlab='goods', ylab='quantity'))

with(logistics,
     plot(goods, quantity,
          pch=10, col='blue',
          cex=1))
with(logistics,
     plot(goods, quantity,
          main='goods feedback and quantity ',
          xlab='goods', ylab='quantity',
          pch=16, 
          col=ifelse(feedback=='Rework', 'blue', 'red')))

######## box plot #######
boxplot(goods~month,data = logistics,col="green",main="Goods in Month")
boxplot(goods~sales.area,data = logistics,col="blue",main="Delivery in Sales Area")


## plot_ly
library(plotly)
p1 <- plot_ly(logistics, x = ~goods, y = ~month) %>% 
  add_lines(name = "month")

p2 <- plot_ly(logistics, x = ~goods, y = ~area) %>% 
  add_lines(name = "area")

p3 <- plot_ly(logistics, x = ~goods, y = ~month,z=~area) %>% 
  add_lines(name = "goods")

subplot(p1)
subplot(p2)
subplot(p3)

## dygraphs
install.packages("dygraphs")
library(dygraphs)

dygraph(logistics, main = "month") %>% 
  dyRangeSelector(dateWindow = c("2016/7/1", "2016/12/30"))

gg <- ggplot(logistics, aes(goods,sales.RMB., color ="continent")) +
  geom_point(aes(size = 0.5,id=month)) +
  scale_x_log10()
ggplotly(gg)

## 分组，切片
p <- ggplot(data=logistics,
            mapping = aes(
              x = goods,
              y = month))
p + geom_point(alpha = 0.2)

p + geom_point(alpha = 0.2) +
  facet_grid(sales.area ~ delivery)

## p + geom_point(alpha = 0.2) + geom_smooth() + facet_grid(sales.area ~ delivery)
