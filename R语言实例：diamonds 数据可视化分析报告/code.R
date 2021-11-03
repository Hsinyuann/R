library(readr)
library(dplyr)
library(kknn)
library(TTR)
library(AER)
library(pROC)
library(e1071)
library(nnet)
library(rpart)
library(tidyverse)
library(memisc)
data <- diamonds
head(data)
str(data)
names(data)
data %>% summarise(
  across(everything(), ~ sum(is.na(.)))
)
data <- data %>%
  distinct()
data %>% 
  summarise(depth_median=median(depth),
            table_median=median(table))
par(mfrow=c(1,2))
hist(data$depth,breaks = 40)
hist(data$table,breaks = 20)
ggplot(data)+
  geom_histogram(aes(x=carat),binwidth=0.1)
data %>%
  count(cut, sort = T)
pie(table(data$cut),labels=names(table(data$cut)))
data %>%
  count(color, sort = T)
pie(table(data$color),labels=names(table(data$color)))
data %>%
  count(clarity, sort = T)

pie(table(data$clarity),labels=names(table(data$clarity)))
data %>% 
  arrange(desc(price)) %>% 
  slice_max(price, n = 10)
data %>% 
  filter(cut=='Ideal',color=='D',clarity=='IF') %>% 
  summarise(average_price=mean(price),
            median_price=median(price),
            max_price=max(price),
            min_price=min(price)
            )

ggplot(data %>% 
         filter(cut=='Ideal',color=='D',clarity=='IF'))+
  geom_histogram(aes(x=price),bins=28)
ggplot(data)+
  geom_violin(aes(x=cut,y=sqrt(price),color=cut))
ggplot(data)+
  geom_boxplot(aes(x=color,y=sqrt(price),color=color))
ggplot(data)+
  geom_boxplot(aes(x=clarity,y=sqrt(price),color=clarity))
#### carat
ggplot(data)+
  geom_point(aes(x=carat,y=price,color=cut),alpha=0.5)+
  geom_smooth(aes(x=carat,y=price))
ggplot(data)+
  geom_point(aes(x=carat,y=price,color=color),alpha=0.5)+
  geom_smooth(aes(x=carat,y=price))
ggplot(data)+
  geom_point(aes(x=carat,y=price,color=clarity),alpha=0.5)+
  geom_smooth(aes(x=carat,y=price))

data %>% 
  group_by(clarity, cut) %>% 
  summarize(m = mean(price)) %>% 
  ggplot(aes(x = clarity, y = m, group = cut, color = cut)) +
  geom_point() +
  geom_line(linetype = 2)

data %>% 
  group_by(clarity, color) %>% 
  summarize(m = mean(price)) %>% 
  ggplot(aes(x = clarity, y = m, group = color, color = color)) +
  geom_point() +
  geom_line(linetype = 2)

data %>% 
  group_by(cut, color) %>% 
  summarize(m = mean(price)) %>% 
  ggplot(aes(x = cut, y = m, group = color, color = color)) +
  geom_point() +
  geom_line(linetype = 2)
ggplot(data)+
  geom_histogram(aes(x=price,y=..density..),fill="#247BA0",binwidth=300)+
  geom_density(aes(x=price),size=1,alpha=.5,ajust=4,col='grey',fill='grey')

ggplot(data)+
  geom_histogram(aes(x=price,fill=cut,color=cut))+
  facet_wrap(~cut)
ggplot(data)+
  geom_histogram(aes(x=price,fill=color,color=color))+
  facet_wrap(~color)
ggplot(data)+
  geom_histogram(aes(x=price,fill=clarity,color=clarity))+
  facet_wrap(~clarity)
ggplot(data)+
  geom_point(aes(x=x,y=y,col=price))
ggplot(data)+
  geom_point(aes(x=x,y=z,col=price))
ggplot(data)+
  geom_point(aes(x=y,y=z,col=price))

data %>%
  group_by(cut) %>%
  summarise(
    count = n(),
    price_average = mean(price),
    price_sd = sd(price))
ggplot(data)+
  geom_histogram(aes(price))+
  facet_wrap(~cut)

kruskal.test(price ~ cut, data = data)
data %>%
  ggstatsplot::ggbetweenstats(
    x = cut,
    y = price,
    type = "nonparametric",
    mean.ci = TRUE,
    pairwise.comparisons = TRUE, 
    pairwise.display = "all",    
    p.adjust.method = "fdr",     
    messages = FALSE
  )
kruskal.test(price ~ color, data = data)
kruskal.test(price ~ clarity, data = data)
data$price <- log(data$price)
set.seed(1)
pre <- sort(sample(53794,40000))
train <- data[pre,]
test <- data[-pre,]
m1 <- lm(price ~., data = train)
m1_pre <- predict(m1,test)
plot(m1)
RMSE=function(t,p){
  return(sqrt(mean((t-p)^2)))
}
RMSE(test$price,m1_pre)
