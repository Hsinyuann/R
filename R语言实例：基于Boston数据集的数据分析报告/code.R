rm(list=ls())# 把当前环境中的对象全部删除
library(ggplot2)
library(dplyr)
library(MASS)
head(Boston)# 查看数据前6行
#### 箱线图 ####
boxplot <- boxplot(Boston$crim,outline = T,log= "y")
boxplot$stats
abline(h=boxplot$stats[1,],lwd=1,col=2,asp = 2,lty = 2)
text(1.25,boxplot$stats[1,], "minimum=0.00632", col = 2,adj=c(0,-0.4))

abline(h=boxplot$stats[2,],lwd=1,col=2,asp = 2,lty = 2)
text(1.25,boxplot$stats[2,], "Q1=0.08199", col = 2,adj=c(0,-0.4))

abline(h=boxplot$stats[3,],lwd=1,col=2,asp = 2,lty = 2)
text(1.25,boxplot$stats[3,], "median=0.25651", col = 2,adj=c(0,-0.4))

abline(h=boxplot$stats[4,],lwd=1,col=2,asp = 2,lty = 2)
text(1.25,boxplot$stats[4,], "Q3=3.67822", col = 2,adj=c(0,-0.4))

abline(h=boxplot$stats[5,],lwd=1,col=2,asp = 2,lty = 2)
text(1.25,boxplot$stats[5,], "maximum=8.98296", col = 2,adj=c(0,-0.4))

#### logistic 回归模型 ####
dt <- Boston# 将 Boston 赋值给 dt
# 构建新变量 crim_bi
# crim_bi：高于 crim 中位数的项记为“1”, 否则为“0”
dt$crim_bi <- ifelse(dt$crim > median(dt$crim), 1, 0)

#### 构建3个模型 ####
log.fit <- glm(crim_bi ~ zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+black+lstat+medv,
               data = dt , family = "binomial")
summary(log.fit)
log.fit2 <- glm(crim_bi ~ zn+indus+nox+age+dis+rad+tax+ptratio+black+medv,
               data = dt , family = "binomial")
summary(log.fit2)
log.fit3 <- glm(crim_bi ~ zn+nox+age+dis+rad+tax+ptratio+black+medv,
                data = dt , family = "binomial")
summary(log.fit3)


fold_log <- function(log.fit,dt){
  library(caret)
  set.seed(3)
  folds <- createFolds(y=dt[,10],k=10)
  accuracy <- as.numeric()
  for (i in 1:10){
    fold_test <- dt[folds[[i]],] #取folds[[i]]作为测试集
    fold_train <- dt[-folds[[i]],] # 剩下的数据作为训练集
    fold_pre<- predict(log.fit,fold_test,type = "response")
    log.class <- ifelse(fold_pre > 0.5, 1, 0)
    a <- table(log.class, fold_test$crim_bi)
    accuracy <- append(accuracy,(a[1]+a[4])/sum(a))
  }
  return(mean(accuracy))
}

fold_log(log.fit,dt)
fold_log(log.fit2,dt)
fold_log(log.fit3,dt)

#### 划分测试和学习数据集 ####
dim(dt)
length <- dim(dt)[1]
set.seed(5)
pre <- sample(length,length*0.8)#随机抽取 80%的观测放入学习数据集
pre <- sort(pre)# 排序
train <- dt[pre,]# 随机抽取 80%的观测放入学习数据集train
test <-  dt[-pre,]# 测试数据集test

log.pred <- predict(log.fit2, test, type = "response")
log.class <- ifelse(log.pred > 0.5, 1, 0)
# 混淆矩阵
table(log.class, test$crim_bi)

#### LDA 回归模型 ####
lda <- lda(crim_bi ~ zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+black+lstat+medv,
           data = dt) 
lda2 <- lda(crim_bi ~ zn+indus+nox+age+dis+rad+tax+ptratio+black+medv,
           data = dt) 
lda3 <- lda(crim_bi ~ zn+nox+age+dis+rad+tax+ptratio+black+medv,
           data = dt) 
fold_lda <- function(lda,dt){
  library(caret)
  set.seed(3)
  folds <- createFolds(y=dt[,10],k=10)
  accuracy <- as.numeric()
  for (i in 1:10){
    fold_test <- dt[folds[[i]],] #取folds[[i]]作为测试集
    fold_train <- dt[-folds[[i]],] # 剩下的数据作为训练集
    fold_pre<- predict(lda,fold_test)
    a <- table(predict(lda,fold_test)$class, fold_test$crim_bi)
    accuracy <- append(accuracy,(a[1]+a[4])/sum(a))
  }
  return(mean(accuracy))
}
fold_lda(lda,dt)
fold_lda(lda2,dt)
fold_lda(lda3,dt)
#### K 临近模型 ####
#### 模型1 ####
# k=1
library(kknn)
library(caret)
set.seed(3)
folds <- createFolds(y=dt[,10],k=10)
accuracy <- as.numeric()
for (i in 1:10){
  fold_test <- dt[folds[[i]],] #取folds[[i]]作为测试集
  fold_train <- dt[-folds[[i]],] # 剩下的数据作为训练集
  knn <- kknn(crim_bi ~ zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+black+lstat+medv,
              fold_train,fold_test,k=1)
  pre_knn <- fitted(knn)
  pre_knn <- ifelse(pre_knn > 0.5, 1, 0)
  a <- table(pre_knn, fold_test$crim_bi)
  accuracy <- append(accuracy,(a[1]+a[4])/sum(a))
}
mean(accuracy)
# k=5
set.seed(3)
folds <- createFolds(y=dt[,10],k=10)
accuracy <- as.numeric()
for (i in 1:10){
  fold_test <- dt[folds[[i]],] #取folds[[i]]作为测试集
  fold_train <- dt[-folds[[i]],] # 剩下的数据作为训练集
  knn <- kknn(crim_bi ~ zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+black+lstat+medv,
              fold_train,fold_test,k=5)
  pre_knn <- fitted(knn)
  pre_knn <- ifelse(pre_knn > 0.5, 1, 0)
  a <- table(pre_knn, fold_test$crim_bi)
  accuracy <- append(accuracy,(a[1]+a[4])/sum(a))
}
mean(accuracy)

#### 模型2 ####
# k=1
library(kknn)
library(caret)
set.seed(3)
folds <- createFolds(y=dt[,10],k=10)
accuracy <- as.numeric()
for (i in 1:10){
  fold_test <- dt[folds[[i]],] #取folds[[i]]作为测试集
  fold_train <- dt[-folds[[i]],] # 剩下的数据作为训练集
  knn <- kknn(crim_bi ~ zn+indus+nox+age+dis+rad+tax+ptratio+black+medv,
              fold_train,fold_test,k=1)
  pre_knn <- fitted(knn)
  pre_knn <- ifelse(pre_knn > 0.5, 1, 0)
  a <- table(pre_knn, fold_test$crim_bi)
  accuracy <- append(accuracy,(a[1]+a[4])/sum(a))
}
mean(accuracy)
# k=5
set.seed(3)
folds <- createFolds(y=dt[,10],k=10)
accuracy <- as.numeric()
for (i in 1:10){
  fold_test <- dt[folds[[i]],] #取folds[[i]]作为测试集
  fold_train <- dt[-folds[[i]],] # 剩下的数据作为训练集
  knn <- kknn(crim_bi ~ zn+indus+nox+age+dis+rad+tax+ptratio+black+medv,
              fold_train,fold_test,k=5)
  pre_knn <- fitted(knn)
  pre_knn <- ifelse(pre_knn > 0.5, 1, 0)
  a <- table(pre_knn, fold_test$crim_bi)
  accuracy <- append(accuracy,(a[1]+a[4])/sum(a))
}
mean(accuracy)

#### 模型3 ####
# k=1
library(kknn)
library(caret)
set.seed(3)
folds <- createFolds(y=dt[,10],k=10)
accuracy <- as.numeric()
for (i in 1:10){
  fold_test <- dt[folds[[i]],] #取folds[[i]]作为测试集
  fold_train <- dt[-folds[[i]],] # 剩下的数据作为训练集
  knn <- kknn(crim_bi ~ zn+nox+age+dis+rad+tax+ptratio+black+medv,
              fold_train,fold_test,k=1)
  pre_knn <- fitted(knn)
  pre_knn <- ifelse(pre_knn > 0.5, 1, 0)
  a <- table(pre_knn, fold_test$crim_bi)
  accuracy <- append(accuracy,(a[1]+a[4])/sum(a))
}
mean(accuracy)
# k=5
set.seed(3)
folds <- createFolds(y=dt[,10],k=10)
accuracy <- as.numeric()
for (i in 1:10){
  fold_test <- dt[folds[[i]],] #取folds[[i]]作为测试集
  fold_train <- dt[-folds[[i]],] # 剩下的数据作为训练集
  knn <- kknn(crim_bi ~ zn+nox+age+dis+rad+tax+ptratio+black+medv,
              fold_train,fold_test,k=5)
  pre_knn <- fitted(knn)
  pre_knn <- ifelse(pre_knn > 0.5, 1, 0)
  a <- table(pre_knn, fold_test$crim_bi)
  accuracy <- append(accuracy,(a[1]+a[4])/sum(a))
}
mean(accuracy)

library(leaps)
leaps<- regsubsets(crim ~ zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+black+lstat+medv,
           data=dt)
plot(leaps,scale = "adjr2")
lmfit<- lm(crim ~ zn+nox+dis+rad+ptratio+black+lstat+medv,
                   data=dt)

dim(dt)
length <- dim(dt)[1]
set.seed(1)
pre <- sample(length,length*0.7)# 随机抽取70 %的观测放入学习数据集
pre <- sort(pre)
train <- dt[pre,]# 学习数据集train
test <-  dt[-pre,]# 剩余30 %放入测试数据集test

lm_pre<- predict(lmfit, test) 
RMSE=function(t,p){
  return(sqrt(mean((t-p)^2)))
}
RMSE(test$crim,lm_pre)
