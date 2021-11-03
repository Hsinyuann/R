# 实例
|      | 省份 | 数量 |
| ---- | ---- | ---- |
| 1    | 粤   | 990  |
| 2    | 鲁   | 764  |
| 3    | 浙   | 567  |
| 4    | 苏   | 391  |
| 5    | 皖   | 290  |
| 6    | 辽   | 284  |
| 7    | 湘   | 232  |
| 8    | 赣   | 205  |
| 9    | 豫   | 177  |
| 10   | 鄂   | 172  |
| 11   | 冀   | 142  |
| 12   | 闽   | 127  |
| 13   | 桂   | 99   |
| 14   | 陕   | 99   |
| 15   | 沪   | 83   |
| 16   | 京   | 71   |
| 17   | 津   | 64   |
| 18   | 吉   | 62   |
| 19   | 新   | 56   |
| 20   | 渝   | 33   |
| 21   | 黑   | 27   |
| 22   | 晋   | 17   |
| 23   | 甘   | 12   |
| 24   | 黔   | 11   |
| 25   | 内   | 8    |
| 26   | 琼   | 6    |
| 27   | 青   | 5    |
| 28   | 川   | 3    |
| 29   | 滇   | 3    |

如图所示，命名为 `count` 的表格为29个以简称代替的省份统计数量表。根据每个省份的统计的数量不同，绘制着色图。

使用 `leafletCN` 包绘制地图。

使用 `leafletCN` 包中的 `regionNames()` 传入需要查看的城市名, 显示这个城市支持的区域信息, 比如查看成都:

```r
regionNames("成都")
[1] "成华区"   "崇州市"   "大邑县"   "都江堰市" "金牛区"  
[6] "金堂县"   "锦江区"   "龙泉驿区" "彭州市"   "蒲江县"  
[11] "青白江区" "青羊区"   "双流县"   "温江区"   "武侯区"  
[16] "新都区"   "新津县"   "邛崃市"   "郫县"    
```
如果不传入对象, 会自动返回300多个支持的名字列表,包括各个城市,省,以及三个特殊的名字:

- `world` 世界地图；
- `china` 中国分省份地图；
- `city` 中国分城市地图；


查看中国分省份地图：
```r
> regionNames('china')
 [1] "新疆维吾尔自治区" "西藏自治区"       "内蒙古自治区"     "青海省"           "四川省"          
 [6] "黑龙江省"         "甘肃省"           "云南省"           "广西壮族自治区"   "湖南省"          
[11] "陕西省"           "广东省"           "吉林省"           "河北省"           "湖北省"          
[16] "贵州省"           "山东省"           "江西省"           "河南省"           "辽宁省"          
[21] "山西省"           "安徽省"           "福建省"           "浙江省"           "江苏省"          
[26] "重庆市"           "宁夏回族自治区"   "海南省"           "台湾省"           "北京市"          
[31] "天津市"           "上海市"           "香港特别行政区"   "澳门特别行政区" 
```
可以发现这些省份都是全称。

在我们的数据中，省份使用简称。无法直接用简称的省份绘制着色图。所以写一个把简称替换为全称的函数。
```r
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
           colorMethod="factor",
           palette="Reds")
```
![在这里插入图片描述](https://img-blog.csdnimg.cn/e572c942f8b740859566cc6ea2d3ae1a.png?x-oss-process=image/watermark,type_ZHJvaWRzYW5zZmFsbGJhY2s,shadow_50,text_Q1NETiBAdXJsbGliMl8=,size_20,color_FFFFFF,t_70,g_se,x_16)
如图所示，绘制为交互图。可以增加一行代码修改显示框。比如：
```r
geojsonMap(dat,
           "china",
           popup=paste0(count$`省份全称`,"：",count$`数量`),
           colorMethod="factor",
           palette="Reds")
```
![在这里插入图片描述](https://img-blog.csdnimg.cn/e4d18a20fca84df39b2dee2328bed549.png?x-oss-process=image/watermark,type_ZHJvaWRzYW5zZmFsbGJhY2s,shadow_50,text_Q1NETiBAdXJsbGliMl8=,size_20,color_FFFFFF,t_70,g_se,x_16)
