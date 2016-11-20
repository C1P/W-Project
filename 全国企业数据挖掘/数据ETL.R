rm(list = ls())
setwd("D:/data")
library(RMySQL)
con <- dbConnect(MySQL(), host = "127.0.0.1", dbname = "cci",user = "root", password = "root")
tcon <- dbConnect(MySQL(), host = "127.0.0.1", dbname = "cci_etl", user = "root", password = "root")
dbListTables(con)
com_b <- dbGetQuery(con, 
                    "SELECT entid,c_class,ind,ind_cord,permission,en_s,r_cap,rr_cap,em,reg1,reg2 FROM company_basic") 
com_b$permission[which(com_b$permission != "\\N")] <- 1
com_b$permission[which(com_b$permission == "\\N")] <- 0

com_b1 <- com_b
com_b1[com_b1  == "\\N"] <- "NA"
com_b1$rr <- as.numeric(com_b1$rr_cap) / as.numeric(com_b1$r_cap)
com_b1$rr_cap <- NULL

##根据登记机关标号（reg1）补充原始reg2 地区编号
## table(nchar(com_b1$reg2))
## table(nchar(com_b1$reg1))
ind_reg <- intersect(which(nchar(com_b1$reg1) == 6), which(nchar(com_b1$reg2) < 6))
com_b1$reg2[ind_reg] <- com_b1$reg1[ind_reg]
com_b1$reg <- substr(com_b1$reg2,1,2)
com_b1$reg2 <- NULL
com_b1$reg1 <- NULL
dbWriteTable(tcon, "company_basic", com_b1, row.names = FALSE, overwrite = TRUE)
dbDisconnect(con) 
##股东数据清洗，转换
##对股东性质，根据名称确定；对个人股东从身份证号中提取计算平均年龄

rm(list = ls())
library(RMySQL)
con <- dbConnect(MySQL(), host = "127.0.0.1", dbname = "cci", user = "root", password = "root")
tcon <- dbConnect(MySQL(), host = "127.0.0.1", dbname = "cci_etl", user = "root", password = "root")
dbSendQuery(con,'SET NAMES gbk') ##设置数据格式
comsh <- dbGetQuery(con, "SELECT entid,sh_nm,sh_cl,sh_cd FROM company_inv")

sh_nm <- comsh$sh_nm
ind_corp <-Reduce(union, list(v1 = grep("企业",sh_nm),
                             v2 = grep("厂",sh_nm),
                             v3 = grep("公司",sh_nm)))
ind_det <- which(nchar(comsh$sh_nm) < 2)

ind_sh_cl <- rep("other", nrow(comsh))
ind_sh_cl[which(comsh$sh_cl == 30)] <- "foreigner"  ##外籍人员

length (intersect(which(ind_sh_cl == "other"), which(comsh$sh_cl == 20)))

per_wr <- Reduce(union, list(v1 = which(nchar(comsh$sh_cd) == 18),
                            which(nchar(comsh$sh_cd) == 16),
                            which(nchar(comsh$sh_cd) == 15)))

ind_sh_cl[union(per_wr, which(comsh$sh_cl == 20))] <- "person"

ind_ch <- intersect(grep("公司", comsh$sh_nm), which(comsh$sh_cl == 20)) ##统计表中标错的有多少

ind_sh_cl[ind_corp] <- "corporate" ##放在个人下面可以解决表中标错了的问题

sh_cd <- comsh$sh_cd

comsh1 <- data.frame(entid = comsh$entid, sh_cl = ind_sh_cl,
                     coun = rep(1, nrow(comsh)), age = rep(0, nrow(comsh)))
##本想重新计算投资人年龄，但因为个人身份证数据缺失过于严重，有身份证数据的个人,只占全部个人的1/10左右
comsh1 <- comsh1[-ind_det,] ##删除有问题数据
sh_count_1 <- as.data.frame(tapply(comsh1$coun, comsh1$entid, sum))
sh_count <- data.frame(row.names = NULL, entid = row.names(sh_count_1),
                       count = sh_count_1$`tapply(comsh1$coun, comsh1$entid, sum)`)
sh_fine <- merge(comsh1, sh_count, all.x = T)
sh_fine[ ,c(3,4)] <- NULL

dbWriteTable(tcon, "company_shareholder", sh_fine, row.names = FALSE, overwrite = TRUE)
dbDisconnect(con) 

## 法人清洗，对身份证内容计算年龄、地区等
rm(list = ls())
options(scipen=200)
library(RMySQL)
con <- dbConnect(MySQL(), host = "127.0.0.1", dbname = "cci", user = "root", password = "root")
tcon <- dbConnect(MySQL(), host = "127.0.0.1", dbname = "cci_etl", user = "root", password = "root")

comm <- dbGetQuery(con, "SELECT entid,lerepsign,cerno FROM company_manager")

commcp <- comm[which(comm$lerepsign == 1), ]
commcp <- na.omit(commcp)
age <- commcp$cerno
agen <- which(nchar(age) != 15 & nchar(age) != 18)
age[which(nchar(age) == 15)] <- paste0(19, substr(age[which(nchar(age) == 15)], 7, 8))
age[which(nchar(age) == 18)] <- substr(age[which(nchar(age) == 18)], 7, 10)

reg <- substr(commcp$cerno, 1, 2) ##提取出生地
corage <- data.frame(entid = commcp$entid, age, region = reg)
corage <- corage[-agen, ]

dbWriteTable(tcon, "company_corage", corage, row.names = FALSE, overwrite = TRUE)

##外商投资企业
rm(list = ls())
library(RMySQL)
con <- dbConnect(MySQL(), host = "127.0.0.1", dbname = "cci", user = "root", password = "root")
tcon <- dbConnect(MySQL(), host = "127.0.0.1", dbname = "cci_etl", user = "root", password = "root")
formoney <- dbGetQuery(con, "SELECT entid FROM reg_formoney")
com_b <- dbGetQuery(con, "SELECT entid FROM company_basic") 
ind <- intersect(as.numeric(com_b$entid), as.numeric((formoney$entid)))
dbWriteTable(tcon, "company_foreign", formoney, row.names = FALSE, overwrite = TRUE)


## 企业行政处罚
rm(list = ls())
options(scipen=200)
library(RMySQL)
con <- dbConnect(MySQL(), host = "127.0.0.1", dbname = "cci", user = "root", password = "root")
tcon <- dbConnect(MySQL(), host = "127.0.0.1", dbname = "cci_etl", user = "root", password = "root")
case <- dbGetQuery(con, "SELECT caseid,fine,date FROM case_punish")
case_entid <- dbGetQuery(con, "SELECT entid,caseid FROM case_basic")
##分析认为数据格式中存在错误，大于1000万的都将被人为，单位错误，全部除余10000
##其他案件数据缺失很严重，只好选罚金作为评判标准
fine <- as.numeric(case$fine)
fine[which(fine > 1000)] <- (fine[which(fine > 1000)]) / 10000
case_ad <- transform(case, fine = fine)
case_ad2 <- merge(case_ad, case_entid, by = "caseid", all.x = T)
case_ad2$count <- rep(1, nrow(case_ad2)) ##为未来计算案件次数做准备
sum(is.na(case_ad2$entid))
dbWriteTable(tcon, "company_punish_case", case_ad2, row.names = FALSE, overwrite = TRUE)
##

##分支机构
rm(list = ls())
options(scipen=200)
library(RMySQL)
con <- dbConnect(MySQL(), host = "127.0.0.1", dbname = "cci", user = "root", password = "root")
tcon <- dbConnect(MySQL(), host = "127.0.0.1", dbname = "cci_etl", user = "root", password = "root")
fil <- dbGetQuery(con, "SELECT entid FROM company_filiation")
fil$fil_count <- rep(1, nrow(fil))
dbWriteTable(tcon, "company_filiation", fil, row.names = FALSE, overwrite = TRUE)
##


##财务信息
rm(list = ls())
library(RMySQL)
con <- dbConnect(MySQL(), host = "127.0.0.1", dbname = "cci", user = "root", password = "root")
tcon <- dbConnect(MySQL(), host = "127.0.0.1", dbname = "cci_etl", user = "root", password = "root")
fin <- dbGetQuery(con,
                  "SELECT entid,year,op_in,op_co,op_tax,op_ex,ad_ex,fin_ex,
                  lo_as,ch_as,in_in,op_pr,nop_in,nop_co,net_pr,assets,
                  adjust_tax,adjust_tax2 FROM tax_year")
##问题，##数据内空值和0没有做区分
##不知道NA是“空值还是0”，下面做一个清洗方法
fin <- as.data.frame(apply(fin, 2, as.numeric))
fin$adjust_tax[which(is.na(fin$adjust_tax))] <- fin$adjust_tax2[which(is.na(fin$adjust_tax))]
fin$adjust_tax2 <- NULL
in_sheet <- fin[ ,3:15]
ind_det <- apply(in_sheet, 1, function(x){sum(is.na(x))})
ind_det1 <- which((ind_det - 13) == 0)
fin1 <- fin[-ind_det1, ]
in_sheet <- in_sheet[-ind_det1, ]
in_sheet[is.na(in_sheet)] <- 0
attach (in_sheet)
in_sheet$rw <- (op_in - op_co - op_tax - op_ex - ad_ex - 
                    fin_ex - lo_as + ch_as + in_in - op_pr) 
detach (in_sheet)
##通过对上下相差一万的企业进行提取，发生这种问题可能原因是利润表中有的其他项目
## 税务数据中并没有，也很有可能是由于有空值引起的；现在我们都认为是由空值引起的
er1 <- intersect(which(in_sheet$rw == in_sheet$op_in), which(in_sheet$rw != 0)) ##收入与差值相等的
er2 <- which(abs(in_sheet$rw) > 1000) ##认为相差1万以上的财务数据属于缺失较为严重的
er <- union(er1, er2)
index <- round(abs(in_sheet$rw / in_sheet$op_pr),2) ## 差额占收入比重超过10%，也将被剔除
index1 <- which(index > 0.1)

del <- union(er,index1)
    
fin_fin <- fin1[-del, ]
fin_fin[is.na(fin_fin)] <- 0
dbWriteTable(tcon, "company_fin", fin_fin, row.names = FALSE, overwrite = TRUE)
##
##年份
library(RMySQL)
tcon <- dbConnect(MySQL(), host = "127.0.0.1", dbname = "cci_etl", user = "root", password = "root")
fin <- dbGetQuery(tcon, "SELECT * FROM company_fin")
fin1 <- split(fin, fin$year)
fin2014 <- fin1$`2014`
com <- Reduce(intersect, list(v1 = fin1$`2012`[ ,1], v2 = fin1$`2013`[ ,1], v3 = fin1$`2014`[ ,1]))  ## 3年财务齐全的企业名单
dbWriteTable(tcon, "company_fin2014", fin2014, row.names = FALSE, overwrite = TRUE)
