rm(list = ls())
setwd("D:/data")
library(RMySQL)
con <- dbConnect(MySQL(),host = "127.0.0.1",dbname = "cci",user = "root",password = "root")
tcon <- dbConnect(MySQL(),host = "127.0.0.1",dbname = "cci_etl",user = "root",password = "root")
dbListTables(con)
com_b <- dbGetQuery(con, "SELECT entid,c_class,ind,ind_cord,permission,en_s,r_cap,rr_cap,em FROM company_basic") 
com_b$permission[which(com_b$permission != "\\N")] <- 1
com_b$permission[which(com_b$permission == "\\N")] <- 0

com_b1 <- com_b
com_b1[com_b1  == "\\N"] <- "NA"
com_b1$rr <- as.numeric(com_b1$rr_cap)/as.numeric(com_b1$r_cap)


tcomp <- dbGetQuery(con,"SELECT entid FROM tax_year")
tcomp <- as.numeric(tcomp[,1])
tcomp1 <- unique(tcomp)

comfl <- dbGetQuery(con,"SELECT entid FROM company_manager")
comfl <- as.numeric(comfl[,1])
comfl1 <- unique(comfl)

a <- Reduce(intersect,list(v2 = comiv1,v3 = tcomp1,v4 = comfl1))



