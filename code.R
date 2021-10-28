#### 第一问 ####
library(readxl)
library(dplyr)
library(xlsx)
setwd("^^^^^^^^")
# 读取数据
licence <- read_excel("licence.xlsx")
# 观察数据知，字符串的第一个字代表省份
# 提取字符串的第一个字，放入`省份`一列中
licence$`省份` <- substring(licence$`生产许可证号`,1,1)
# 对每省生产许可证数量进行统计
count <- licence %>% 
  group_by(`省份`) %>% 
  summarise(`数量`=n()) %>% 
  arrange(desc(`数量`))
# 输出统计结果
count
write.xlsx(count,"count.xlsx")
#### 第二问 ####
library(leafletCN)
# 写一个将省份缩写转换为全称的函数 trans_name
trans_name <- function(name){
  name <- gsub("晋", "山西省", name)
  name <- gsub("京", "北京市", name)
  name <- gsub("津", "天津市", name)
  name <- gsub("冀", "河北省", name)
  name <- gsub("内", "内蒙古自治区", name)
  name <- gsub("宁", "宁夏回族自治区", name)
  name <- gsub("辽", "辽宁省", name)
  name <- gsub("吉", "吉林省", name)
  name <- gsub("黑", "黑龙江省", name)
  name <- gsub("沪", "上海市", name)
  name <- gsub("苏", "江苏省", name)
  name <- gsub("浙", "浙江省", name)
  name <- gsub("皖", "安徽省", name)
  name <- gsub("闽", "福建省", name)
  name <- gsub("赣", "江西省", name)
  name <- gsub("鲁", "山东省", name)
  name <- gsub("豫", "河南省", name)
  name <- gsub("鄂", "湖北省", name)
  name <- gsub("湘", "湖南省", name)
  name <- gsub("粤", "广东省", name)
  name <- gsub("桂", "广西壮族自治区", name)
  name <- gsub("琼", "海南省", name)
  name <- gsub("川", "四川省", name)
  name <- gsub("黔", "贵州省", name)
  name <- gsub("滇", "云南省", name)
  name <- gsub("渝", "重庆市", name)
  name <- gsub("藏", "西藏自治区", name)
  name <- gsub("陕", "陕西省", name)
  name <- gsub("甘", "甘肃省", name)
  name <- gsub("青", "青海省", name)
  name <- gsub("新", "新疆维吾尔自治区", name)
  name <- gsub("港", "香港特别行政区", name)
  name <- gsub("澳", "澳门特别行政区", name)
  name <- gsub("台", "台湾省", name)
  return(name)
}
# 用函数 trans_name() 对简称进行替换
count$`省份全称` <- trans_name(count$`省份`)
# 绘图
regionNames('china')
dat = data.frame(name = count$`省份全称`, 
                 value =count$`数量`)
geojsonMap(dat,
           "china",
           popup=paste0(count$`省份全称`,"：",count$`数量`),
           colorMethod="factor",
           palette="Reds")
geojsonMap(dat,
           "china",
           colorMethod="factor",
           palette="Reds")
