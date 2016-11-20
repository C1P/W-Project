library (ggplot2)
library (RMySQL)
rm (list = ls())
## 由于数据是从总体库中随机抽取的，故我认为数据中的行业分布基本模拟了真实的企业行业分布
con <- dbConnect(MySQL(), host = "127.0.0.1", dbname = "cci",user = "root", password = "root")
tcon <- dbConnect(MySQL(), host = "127.0.0.1", dbname = "cci_etl", user = "root", password = "root")
exp1 <- dbGetQuery(tcon,
                   "SELECT * FROM company_fin")
data.frame(name = names(exp1), 1:length(names(exp1)))
exp1[ ,c(2:17, 23, 25:28)] <- NULL
sum(na.omit(exp1$cor_reg != exp1$reg))
exp2 <- na.omit(exp1)
exp_reg <- data.frame(cor_reg = as.numeric(exp2$cor_reg), reg = as.numeric(exp2$reg))
exp_reg <- na.omit(exp_reg)

## 创业者出走率
exp_reg$df <- as.numeric(exp_reg$cor_reg != exp_reg$reg)
ca <- as.data.frame(tapply(exp_reg$df, exp_reg$cor_reg, sum))
ca$reg <- row.names(ca)
names(ca)[1] <- "df"
ca1 <- as.data.frame(table(exp_reg$cor_reg))
names(ca1)[1] <- "reg"
ca2 <- merge(ca1, ca, all.x = T, by = "reg")
ca2$rr <- as.numeric(ca2$df) / ca2$Freq
ca2 <- ca2[order(ca2$Freq, decreasing = T), ]
ind <- rbind(read.table("clipboard",header=T,sep=','), 11)  ##地址对应表
ca3 <- ca2[ - which(as.numeric(as.character(ca2$reg)) %in% 
                        setdiff(as.numeric(as.character(ca2$reg)), as.numeric(ind[ ,1]))),]
ca3[order(ca3$rr,decreasing = T), ]
