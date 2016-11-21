library(plotrix)
x <- c(30, 40, 70)
label <- c("Mining", "Manufacturing", "Services")
pie3D(x, labels = label, explode = 0.1, main = "Investment Allocation")

library(googleVis)
x <- c("CN-11","CN-12","CN-13","CN-14","CN-15",
         "CN-21","CN-22","CN-23","CN-31","CN-32")
y <- c(1,1,1,1,1,1,2,3,4,1)

xy <- data.frame(x,y)
G2 <- gvisGeoChart(xy, locationvar = 'x', colorvar = 'y',
                   options = list(region = 'CN',displayMode = "regions",
                                  resolution = "provinces",colorAxis = "{colors: ['yellow','red']}" ))
cat(createGoogleGadget(G2), file="motionchart.xml")

library(devtools)
install_github("lchiffon/REmap")

library(maps)
library(mapdata)

library(ggplot2)
x <- c("广东省","福建省","湖南省","黑龙江省","江苏省","浙江省","上海市","其他地区")
y <- c(33.32,22.1,15.1,10.02,9.9,3.1,1.36,4.1)
xy <- data.frame(x,y)
xy$x <- factor(xy$x, levels = c("广东省","福建省","湖南省","黑龙江省","江苏省","浙江省","上海市","其他地区"))
xy <- xy[order(xy$y,decreasing = T),]
ggplot(data = xy,aes(x,y)) + geom_bar(stat="identity", fill="lightblue", colour="black") +
    geom_text(aes(label = paste0(y,"%"), vjust = -0.5)) + labs(x = "区域", y = "行业内占比", title = "区域分布(企业数量)")

##
x <- c(15.4,9.6,10.1,12.6,10.1,10.8,8.66,11.4,11.9,10.4,11.14,12.7)
y <- c("行业","地区","地区内行业","行业","地区","地区内行业","行业","地区","地区内行业","行业","地区","地区内行业")
z <- c(2012,2012,2012,2013,2013,2013,2014,2014,2014,2015,2015,2015)
xyz <- data.frame(x,y,z)
ggplot(xyz, aes(x = factor(z), y = x, colour= y ,group = y )) + geom_line(size = 2) + ylim(5,20) +
    geom_point(size=4) + labs(x = "年度", y = "收入增长率", title = "全行业与区域发展对比")

##
a <- read.table("clipboard", header = F)

x <- data.frame(x = rnorm(500,10,100),y = rnorm(500,300,70), z = rep("行业",500))
x1 <- data.frame(x = c(-10,0,-210,10,20,-180,-60,-150,200),y = c(-30,0,0,13,16,20,-3,200,-150),z = rep("行业",9))
y <- data.frame(x = rnorm(500,100,80),y = rnorm(500,20,100), z = rep("地区",500))
xy <- rbind(x,y,x1)

ggplot(xy, aes(x = x, y = y)) + geom_point() + geom_point(aes(colour = factor(z))) + 
    labs(x = "利润(万)", y = "收入(万)",title = "收入与利润关系图")

##
x1 <- c(5.0,1.5,1.9,2.9,3.9,3.8,3.5,2.0,-6.5,
       -1.3,-0.9,-0.4,0,1.3,1.6,2,4,6.2,
       -0.1,-0.9,-0.9,0.6,0.8,1.1,1.2,4.3,10.1)
y1 <- c("存货","存货","存货","存货","存货","存货","存货","存货","存货",
       "应收帐款","应收帐款","应收帐款","应收帐款","应收帐款","应收帐款","应收帐款","应收帐款","应收帐款",
       "负债","负债","负债","负债","负债","负债","负债","负债","负债")
z1 <- c("2013Q4","2014Q1","2014Q2","2014Q3","2014Q4","2015Q1","2015Q2","2015Q3","2015Q4",
       "2013Q4","2014Q1","2014Q2","2014Q3","2014Q4","2015Q1","2015Q2","2015Q3","2015Q4",
       "2013Q4","2014Q1","2014Q2","2014Q3","2014Q4","2015Q1","2015Q2","2015Q3","2015Q4")
xyz <- data.frame(x1, y1, z1)

ggplot(xyz, aes(x = factor(z1), y = as.numeric(x1), color = y1, group = y1)) + geom_line(size = 2) + 
    geom_point() + labs(x = "季度", y = "增长率",title = "行业内企业运营变化") + scale_colour_hue(name = "标签")

##
rm(list = ls())
x <- c(203,236,252,261,436,631,864,1993,2001,2395,2415,3643,4801,6154,1030,1620,1860,1930,2600,3200,3645)
y <- c(2009,2010,2011,2012,2013,2014,2015,2009,2010,2011,2012,2013,2014,2015,2009,2010,2011,2012,2013,2014,2015)
z <- factor(c("下游产业","下游产业","下游产业","下游产业","下游产业","下游产业","下游产业",
       "本行业","本行业","本行业","本行业","本行业","本行业","本行业",
       "上游产业","上游产业","上游产业","上游产业","上游产业","上游产业","上游产业"),
       levels = c("上游产业","本行业","下游产业"))

xyz <- data.frame(floor(x/10), y, z)
ggplot(xyz, aes(x = factor(y), y = x, fill = as.character(z))) + 
    geom_bar(stat="identity",position = "dodge",width = 1) +labs(x = "年度", y = "在营企业数量", title = "新增企业数量") 
    

x <- c("广东省","福建省","湖南省","黑龙江省","江苏省","浙江省","上海市","其他地区")
y <- c(43.42,12.1,11.1,10.02,9.9,6.1,4.2,3.16)
xy <- data.frame(x,y)
xy$x <- factor(xy$x, levels = c("广东省","福建省","湖南省","黑龙江省","江苏省","浙江省","上海市","其他地区"))
xy <- xy[order(xy$y,decreasing = T),]
ggplot(data = xy,aes(x,y)) + geom_bar(stat = "identity", fill = "lightblue", colour = "black") +
    geom_text(aes(label = paste0(y,"%"), vjust = -0.5)) + labs(x = "区域", y = "行业内占比", title = "区域分布(收入)")

##

options(scipen=200)
x <- c(203000,336000,252000,361000,150100,341000,86400,201000,-100000,139500)
y <- c(2011,2011,2012,2012,2013,2013,2014,2014,2015,2015)
z <- factor(c("下游产业","上游产业","下游产业","上游产业",
              "下游产业","上游产业","下游产业","上游产业","下游产业","上游产业"),
            levels = c("上游产业","本行业","下游产业"))
xyz <- data.frame(x,y,z)
df <- transform(xyz, judge = ifelse(y > 0, 'Yes', 'No'))


ggplot(data = df, mapping = aes(x = y, y = x, fill = z, group = z)) + 
    geom_bar(stat = 'identity', position = 'dodge') + labs(x = "日期", y = "收入", title = "相关行业收入均值") 

x <- c(75.24, 75.11, 73.13, 4.67, 3.96, 3.83, 7.92, 1.78, 0.75, 6.81,0.46, 3.74, 5.37, 18.69, 18.54)
y <- c("营业成本","营业成本","营业成本","营业费用","营业费用","营业费用",
       "财务费用","财务费用","财务费用","管理费用","管理费用","管理费用","营业利润","营业利润","营业利润")
z <- c(2013,2014,2015,2013,2014,2015,2013,2014,2015,2013,2014,2015,2013,2014,2015)
xyz <- data.frame(x,y,z)
ggplot(xyz,aes(x = factor(z), y = x, fill = y)) + geom_bar(stat = "identity") + 
    labs(x = "年度", y = "占收入百分比", title = "损益表结构性分析图")

##
x <- c(2.6,2.1,1.13,1.1,0.9,0.16,-2.006,0.14, 0.15, 0.15, 0.17, 0.12, 0.11,0.1033)
y <- c(2009,2010,2011,2012,2013,2014,2015,2009,2010,2011,2012,2013,2014,2015)
z <- factor(c("下游产业","下游产业","下游产业","下游产业","下游产业","下游产业","下游产业",
              "上游产业","上游产业","上游产业","上游产业","上游产业","上游产业","上游产业"),
            levels = c("上游产业","下游产业"))

xyz <- data.frame(x, y, z)
ggplot(xyz, aes(x = factor(y), y = x, fill = as.character(z))) + 
    geom_bar(stat="identity",position = "dodge",width = 1) +labs(x = "年度", y = "收入增长率", title = "相关行业分析") 
ggplot(xyz, aes(x = factor(y), y = x, colors = z, group = z)) + 
    geom_line(size = 2) +labs(x = "年度", y = "收入增长率", title = "相关行业分析") 
