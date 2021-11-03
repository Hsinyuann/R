#### 导入宏包 ####
library(readr)# 读取数据
library(dplyr)# 清洗数据
library(kknn)
library(TTR)
library(AER)
library(pROC)
library(e1071)
library(nnet)
library(rpart)
setwd('C:/Users/86152/Desktop/0609')# 设置工作空间

mt <- read_csv("maotai.csv")# 读取数据
mt <- mt[,-1]
head(mt)
mt_ascend <- mt[order(mt$trade_date),] # 对数据按照日期升序整理
mt_ascend <- mt_ascend %>% 
  select(trade_date,open,high,low,close,pct_chg,change,vol)
#### 计算收益率 ####
mt_ascend$r <- NA
mt_ascend$r[2:length(mt_ascend$trade_date)]<-
mt_ascend$close[2:length(mt_ascend$trade_date)] /
mt_ascend$close[1:length(mt_ascend$trade_date)-1] - 1
mt_ascend <- na.omit(mt_ascend)
#### 计算75%置信度的日度VaR ####
var <- -qnorm(0.25,mean(mt_ascend$r),sd(mt_ascend$r))
var
print(paste('75%置信度的日度VaR为',
            round(var,4),
            '。',
            '说明在2020年，茅台股份在一天内损失超过',
            round(var,4),
            '的概率为25%。',
            sep=' '))
#### 是否违约 ####
mt_ascend$de <- NA
mt_ascend$de_1 <- NA
for (i in 1:length(mt_ascend$close)){
  if ((mt_ascend$pct_chg[i] - var) > 0){
    mt_ascend$de[i] <- 1
    mt_ascend$de_1[i] <- "违约"
  }
  else{
    mt_ascend$de[i] <- 0
    mt_ascend$de_1[i] <- "不违约"
  }
}

#### 计算MACD ####
mt_ascend$MACD <- NA
dt_macd <- data.frame(MACD(mt_ascend$close))
DIF <- dt_macd$macd
DEA <- dt_macd$signal
MACD <- 2*(DIF-DEA)
mt_ascend$MACD <- MACD
mt_ascend <- na.omit(mt_ascend)
#### 计算OBV ####
mt_ascend$OBV <- NA
OBV <- function(v,c,l,h){
  res <- v*((c-l)-(h-c))/(h-c)
  return(res)
}
mt_ascend$OBV <- OBV(v=mt_ascend$vol,
                     c=mt_ascend$close,
                     l=mt_ascend$low,
                     h=mt_ascend$high)
#### 计算CCI ####
mt_ascend$CCI <- NA
CCI <- function(h,l,c,m){
  res <- (sum(h+l+c)/3-m)/((m-c)/5)/0.015
  return(res)
}
mt_ascend$CCI <- CCI(h=mt_ascend$high,
                     l=mt_ascend$low,
                     c=mt_ascend$close,
                     m=mt_ascend$MACD)
mt_ascend[mapply(is.infinite,mt_ascend)] <- NA
mt_ascend <- na.omit(mt_ascend)
#### 计算RSV指标 ####
mt_ascend$RSV <- NA
for (i in 1:length(mt_ascend$close)){
  mt_ascend$RSV[i] = 
    (mt_ascend$close[i]-min(mt_ascend$close[1:i]))/
    (max(mt_ascend$close[1:i])-min(mt_ascend$close[1:i]))
}
mt_ascend <- na.omit(mt_ascend)
#### 计算KDJ指标 ####
# mt_ascend$K[1] = 1/3 * mt_ascend$RSV[1]
mt_ascend$K <- NA
mt_ascend$K[1] <- 50
for (i in 2:length(mt_ascend$close)){
  mt_ascend$K[i] = 2/3 * mt_ascend$K[i-1] + 1/3 * mt_ascend$RSV[i]
}
# mt_ascend$D[1] = 1/3 * mt_ascend$K[1]
mt_ascend$D <- NA
mt_ascend$D[1] = 50
for (i in 2:length(mt_ascend$close)){
  mt_ascend$D[i] = 2/3 * mt_ascend$D[i-1] + 1/3 * mt_ascend$K[i]
}
mt_ascend$J <- NA
for (i in 1:length(mt_ascend$close)){
  mt_ascend$J[i] = 3 * mt_ascend$K[i] - 2 * mt_ascend$D[i]
}
mt_ascend <- na.omit(mt_ascend)
#### logit ####
nrow(mt_ascend)
set.seed(1)
pre <- sort(sample(200,140))
train <- mt_ascend[pre,]
test <- mt_ascend[-pre,]
head(train)

logit <- glm(de~vol+change+r+MACD+OBV+CCI+K+D,
             family=binomial(link='logit'),
             train) 
summary(logit)
log <- predict(logit,test,type="response")
#predict()函数默认输出违约的对数概率
#指定参数type="response"即可得到预测"违约"的概率
obs_p_rf <- factor(log > .5, levels=c(TRUE, FALSE),
                   labels=c("违约", "不违约"))
obs_p_rf
# obs_p_rf <- data.frame(prob=log,
#                        obs=test$de)

# table(test$de,log,dnn=c('actuality','prediction'))
table(test$de_1,obs_p_rf,dnn=c("Actual", "Predicted"))

#### 绘制ROC曲线 ####
log <- roc(test$de,as.numeric(log))
plot(log,
     print.auc=TRUE,
     auc.polygon=TRUE,
     grid=c(0.1,0.2),
     grid.col=c('green','red'),
     max.auc.polygon=TRUE,
     auc.polygon.col='chocolate',
     print.thres=TRUE,
     main='logit模型ROC曲线')
#### SVM ####
SVM <- svm(de~vol+change+r+MACD+OBV+CCI+K+D+J,
           train,
           type='C',
           kernel='radial')
svm <- predict(SVM,test)
obs_p_rf <-data.frame(prob=svm,
                      obs=test$de) 
table(test$de,svm,dnn=c('actuality','prediction'))
#### 绘制ROC曲线 ####
svm <- roc(test$de,as.numeric(svm))
plot(svm,
     print.auc=TRUE,
     auc.polygon=TRUE,
     grid=c(0.1,0.2),
     grid.col=c('green','red'),
     max.auc.polygon=TRUE,
     auc.polygon.col='chocolate',
     print.thres=TRUE,
     main='SVM模型ROC曲线')
#### NNET ####
NNET <- nnet(de~vol+change+r+MACD+OBV+CCI+K+D+J,
             train,
             size=6,
             decay=0.01,
             maxit=1000,
             linout=T)
nn <- predict(NNET,test)
obs_p_rf <-data.frame(prob=nn,
                      obs=test$de) 
table(test$de,nn,dnn=c('actuality','prediction'))
nn <- roc(test$de,as.numeric(nn))
plot(nn,
     print.auc=TRUE,
     auc.polygon=TRUE,
     grid=c(0.1,0.2),
     grid.col=c('green','red'),
     max.auc.polygon=TRUE,
     auc.polygon.col='chocolate',
     print.thres=TRUE,
     main='神经网络模型ROC曲线')
#### KNN ####
maotai.kknn <- kknn(de~vol+change+r+MACD+OBV+CCI+K+D+J,
                    train,
                    test)
pre_knn <- fitted(maotai.kknn)
#### 绘制 ROC 曲线 ####
knn_roc <- roc(test$de,as.numeric(pre_knn))
plot(knn_roc, 
     print.auc=TRUE, 
     auc.polygon=TRUE, 
     grid=c(0.1, 0.2),
     grid.col=c("green", "red"), 
     max.auc.polygon=TRUE,
     auc.polygon.col="pink", 
     print.thres=TRUE,
     main='KNN模型ROC曲线')
#### 决策树 ####
tree <- rpart(de~vol+change+r+MACD+OBV+CCI+K+D+J,
              train)
summary(tree)
pre_rf <- predict(tree,test)
obs_p_rf <- data.frame(prob=pre_rf,obs=test$de)
table(test$de,pre_rf,dnn=c('actuality','prediction'))
rf_roc <- roc(test$de,as.numeric(pre_rf))
plot(rf_roc,
     print.auc=TRUE,
     auc.polygon=TRUE,
     grid=c(0.1,0.2),
     grid.col=c('green','red'),
     max.auc.polygon=TRUE,
     auc.polygon.col='chocolate',
     print.thres=TRUE,
     main='决策树模型ROC曲线')
