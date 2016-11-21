library (RMySQL)
rm(list = ls())
con <- dbConnect(MySQL(),host = "127.0.0.1",dbname = "cci_etl",user = "root",password = "root")
tcon <- dbConnect(MySQL(),host = "127.0.0.1",dbname = "cci_etl",user = "root",password = "root")
company_basic <- dbGetQuery(con,"SELECT * FROM company_basic")
company_corage <- dbGetQuery(con,"SELECT * FROM company_corage")
names(company_corage)[2] <- "cor_age"
names(company_corage)[3] <- "cor_reg"
company_filiation <- dbGetQuery(con,"SELECT * FROM company_filiation")
company_filiation <- as.data.frame(tapply(company_filiation[ ,2], company_filiation[ ,1], sum))
company_filiation$entid <- row.names(company_filiation)
names(company_filiation)[1] <- "filiation"
company_fin2014 <- dbGetQuery(con,"SELECT * FROM company_fin2014")
company_foreign <- dbGetQuery(con,"SELECT * FROM company_foreign")
company_foreign$foreign <- rep(1,nrow(company_foreign))
company_punish_case <- dbGetQuery(con,"SELECT * FROM company_punish_case")
company_punish_case$caseid <- NULL

## sum(is.na(company_punish_case1$fine))
## company_punish_case$fine[which(is.na(as.numeric(company_punish_case$fine)))]
company_punish_case$fine <- as.numeric(company_punish_case$fine)
company_punish_case$fine[is.na(company_punish_case$fine)] <- 0
company_punish_case1 <- as.data.frame(tapply(company_punish_case$fine, company_punish_case$entid, sum))
names(company_punish_case1) <- "case_fine"
company_punish_case1$entid <- row.names(company_punish_case1)
company_punish_case2 <- as.data.frame(tapply(company_punish_case$case_count, company_punish_case$entid, sum))
names(company_punish_case2) <- "cases_count"
company_punish_case2$entid <- row.names(company_punish_case2)
company_punish_case3 <- merge(company_punish_case2, company_punish_case1, all = T, by = "entid")
company_shareholder <- dbGetQuery(con,"SELECT * FROM company_shareholder")
company_shareholder$sh_cl <- NULL
company_shareholder <- company_shareholder[!duplicated(company_shareholder$entid), ]
names(company_shareholder)[2] <- "shareholder_count"

dupl <- function(x = list()){
    dat <- list()
    for (i in 1:length(x)){
        dat[[i]] <- x[[i]][!duplicated(x[[i]]$entid), ]
    }
    return(dat)
}

fin <- list(v1 = company_fin2014, v2 = company_corage, v3 = company_filiation,
                 v4 = company_basic, v5 = company_shareholder, v6 = company_foreign, v7 = company_punish_case3)
fin1 <- dupl(fin)

merg <- function(x = list()){
    dat <- as.data.frame(x[[1]])
    for  (i in 2:length(x)){
        dat <- merge(dat, x[[i]], all.x = T, by = "entid")
    }
    return(dat)
}

fin2 <- merg(fin1)
fin2$cases_count[is.na(fin2$cases_count)] <- 0
fin2$case_fine[is.na(fin2$case_fine)] <- 0
fin2$foreign[is.na(fin2$foreign)] <- 0
fin2$filiation[is.na(fin2$filiation)] <- 0

dbWriteTable(tcon, "company_fin", fin2, row.names = FALSE, overwrite = TRUE)
## ind_re <- test2$entid[!duplicated(test2$entid)]
## company_basic[company_basic$entid %in% ind_re,]
## length(intersect(company_fin2014[ ,1],company_basic[ ,1]))
## test2 <- merge(company_fin2014,company_basic,all.x = T,by = "entid")

ex <- as.data.frame(table(fin2$cor_reg))
ex[order(ex$Freq,decreasing = T), ]
