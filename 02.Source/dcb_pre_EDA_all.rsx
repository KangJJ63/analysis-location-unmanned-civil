
Sys.setenv(JAVA_HOME  =  'C:/Program Files/Java/jre1.8.0_241')

library(readxl)
library(lubridate)
library(stringr)
library(WriteXLS)
library(hms)
library(data.table)
library(dplyr)
library(ggplot2)
library(openxlsx)
library(reshape2)

filenames <- list.files('C:/19Project/07.UCK/01.Data/01.Raw_data/05.DCB', pattern=glob2rx("*.csv"), full.names=TRUE)
df <- rbindlist(lapply(filenames, fread, encoding = 'unknown'))

df$uni_id <- str_trim(df$uni_id, side = c("both"))
df$center <- str_trim(df$center, side = c("both"))
df$dt_mw <- str_trim(df$dt_mw, side = c("both"))
df$hr_mw <- str_trim(df$hr_mw, side = c("both"))
df$cd_dngcho <- str_trim(df$cd_dngcho, side = c("both"))
df$cd_issue <- str_trim(df$cd_issue, side = c("both"))
df$cd_koreng <- str_trim(df$cd_koreng, side = c("both"))
df$cd_prvpub <- str_trim(df$cd_prvpub, side = c("both"))
df$cd_reciev <- str_trim(df$cd_reciev, side = c("both"))
df$sex_apl <- str_trim(df$sex_apl, side = c("both"))
df$age_apl <- str_trim(df$age_apl, side = c("both"))
df$add_apl <- str_trim(df$add_apl, side = c("both"))
df$nm_svr <- str_trim(df$nm_svr, side = c("both"))

write.csv(df, 'C:/19Project/07.UCK/01.Data/02.Use_Data/DCB_Raw.csv', row.names = FALSE)
rm(df)

df <- read.csv('C:/19Project/07.UCK/01.Data/02.Use_Data/DCB_Raw.csv', stringsAsFactor = FALSE)

df$dt_mw <- ymd(df$dt_mw)
df$hr_mw <- str_pad(df$hr_mw, 6, side = 'left', pad = '0')
df$hr_mw <- sapply(df$hr_mw, function(x)
  paste(str_sub(x, -6, -5),
        str_sub(x, -4, -3),
        str_sub(x, -2),
        sep = ':'),
  USE.NAMES = FALSE
)

df$y_reg_mw <- year(df$dt_mw)
df$m_reg_mw <- month(df$dt_mw)
df$m_reg_mw <- str_pad(df$m_reg_mw, 2, side = 'left', pad = '0')
df$ym_reg_mw <- paste(df$y_reg_mw, df$m_reg_mw, sep = '')
df$h_reg_mw <- str_sub(df$hr_mw, 1,2)

df$d_reg_mw <- lubridate::wday(df$dt_mw, label = TRUE, abbr = FALSE)
df$d_reg_mw <- as.character(df$d_reg_mw)
prepro <- read.csv('C:/19Project/07.UCK/01.Data/04.Setting_data/03.DCB/01.Preprocessing/h_ran_nm.csv', header = FALSE, stringsAsFactors = FALSE)

df$h_ran <- ifelse(df$h_reg_mw >= '09' & df$h_reg_mw < '18', prepro[, 1][1],
                   ifelse(df$h_reg_mw >= '18' & df$h_reg_mw <= '23', prepro[, 1][2],
                          ifelse(df$h_reg_mw >= '00' & df$h_reg_mw < '05', prepro[, 1][3], prepro[, 1][4]
                          )))
prepro_age <- read.csv('C:/19Project/07.UCK/01.Data/04.Setting_data/03.DCB/01.Preprocessing/age_ran_nm.csv', header = FALSE, stringsAsFactors = FALSE)

df$age_apl <- as.integer(df$age_apl)

df$age_ran <- NA

idx <- which(df$age_apl < 20) 
df$age_ran[idx] <- prepro_age[, 1][1]
rm(idx)
idx <- which(df$age_apl < 30 & df$age_apl >= 20)
df$age_ran[idx] <- prepro_age[, 1][2]
rm(idx)
idx <- which(df$age_apl < 40 & df$age_apl >= 30)
df$age_ran[idx] <- prepro_age[, 1][3]
rm(idx)
idx <- which(df$age_apl < 50 & df$age_apl >= 40)
df$age_ran[idx] <- prepro_age[, 1][4]
rm(idx)
idx <- which(df$age_apl < 60 & df$age_apl >= 50)
df$age_ran[idx] <- prepro_age[, 1][5]
rm(idx)
idx <- which(df$age_apl < 70 & df$age_apl >= 60)
df$age_ran[idx] <- prepro_age[, 1][6]
rm(idx)
idx <- which(df$age_apl < 80 & df$age_apl >= 70)
df$age_ran[idx] <- prepro_age[, 1][7]
rm(idx)
idx <- which(df$age_apl < 90 & df$age_apl >= 80)
df$age_ran[idx] <- prepro_age[, 1][8]
rm(idx)
idx <- which(df$age_apl < 100 & df$age_apl >= 90)
df$age_ran[idx] <- prepro_age[, 1][9]
rm(idx)
idx <- which(df$age_apl >= 100)
df$age_ran[idx] <- prepro_age[, 1][10]
rm(idx)

prepro_day <- read.csv('C:/19Project/07.UCK/01.Data/04.Setting_data/03.DCB/01.Preprocessing/jj_day_nm.csv', header = FALSE, stringsAsFactors = FALSE)

df$jj <- ifelse((df$d_reg_mw == prepro_day[, 1][1])|(df$d_reg_mw == prepro_day[, 1][2])|(df$d_reg_mw == prepro_day[, 1][3])|(df$d_reg_mw == prepro_day[, 1][4])|(df$d_reg_mw == prepro_day[, 1][5])
                , prepro_day[, 1][8], prepro_day[, 1][9])  

df$count <- 1

write.csv(df, 'C:/19Project/07.UCK/01.Data/03.Data_Preprocessing/03.DCB/DCB_Data_Preprocessing.csv', row.names = FALSE, fileEncoding = 'CP949')

df <- read.csv('C:/19Project/07.UCK/01.Data/03.Data_Preprocessing/03.DCB/DCB_Data_Preprocessing.csv', stringsAsFactors = FALSE)

prepro_nm <- read.csv('C:/19Project/07.UCK/01.Data/04.Setting_data/03.DCB/02.EDA/dcb_EDA_all_nm.csv', stringsAsFactors = FALSE, header = FALSE)

table_year <- data.table(table(df$y_reg_mw))
table_year <- table_year %>%
  mutate(percent = (N/sum(N)))
table_year$percent <- round(table_year$percent, 2)
names(table_year) <- c(prepro_nm[,1][1], prepro_nm[,1][2], prepro_nm[,1][3])
write.xlsx(table_year, 
           file = "C:/19Project/07.UCK/03.Output/03.DCB/DCB_EDA_All.xlsx",
           sheetName = prepro_nm[,2][1],
           append = TRUE,
           row.names = FALSE)

table_cate <- data.table(table(df$cd_issue))
table_cate <- table_cate %>%
  mutate(percent = (N/sum(N)))
table_cate$percent <- round(table_cate$percent, 2)
names(table_cate) <- c(prepro_nm[,1][4], prepro_nm[,1][2], prepro_nm[,1][3])
write.xlsx(table_cate, 
           file = "C:/19Project/07.UCK/03.Output/03.DCB/DCB_EDA_All.xlsx",
           sheetName = prepro_nm[,2][2],
           append = TRUE,
           row.names = FALSE)

table_min_year <- data.table(table(df$cd_issue, df$y_reg_mw))
table_min_year <- dcast(table_min_year, V1 ~ V2, value.var = 'N')
names(table_min_year)[1] <- c(prepro_nm[,1][4])
write.xlsx(table_min_year, 
           file = "C:/19Project/07.UCK/03.Output/03.DCB/DCB_EDA_All.xlsx",
           sheetName = prepro_nm[,2][3],
           append = TRUE,
           row.names = FALSE)

table_c <- data.table(table(df$m_reg_mw, df$y_reg_mw))
table_c$V1 <- as.numeric(table_c$V1)
table_c1 <- dcast(table_c, V2 ~V1, value.var = "N")
names(table_c1)[1] <- c(prepro_nm[,1][1])
write.xlsx(table_c1, 
           file = "C:/19Project/07.UCK/03.Output/03.DCB/DCB_EDA_All.xlsx",
           sheetName = prepro_nm[,2][4],
           append = TRUE,
           row.names = FALSE)

table_cd_m <- data.table(table(df$m_reg_mw, df$cd_issue))
table_cd_m$V1 <- as.numeric(table_cd_m$V1)
table_cd_m <- dcast(table_cd_m, V2 ~ V1, value.var = "N")
names(table_cd_m)[1] <- c(prepro_nm[,1][4])
write.xlsx(table_cd_m, 
           file = "C:/19Project/07.UCK/03.Output/03.DCB/DCB_EDA_All.xlsx",
           sheetName = prepro_nm[,2][5],
           append = TRUE,
           row.names = FALSE)

table_age_ran <- data.table(table(df$age_ran))
table_age_ran <- table_age_ran[c(2:10, 1),]
table_age_ran <- table_age_ran %>%
  mutate(percent = (N/sum(N)))
table_age_ran$percent <- round(table_age_ran$percent, 2)
colnames(table_age_ran) <- c(prepro_nm[,1][19], prepro_nm[,1][2], prepro_nm[,1][3])
write.xlsx(table_age_ran, 
           file = "C:/19Project/07.UCK/03.Output/03.DCB/DCB_EDA_All.xlsx",
           sheetName = prepro_nm[,2][6],
           append = TRUE,
           row.names = FALSE)

table_age <- aggregate(age_apl ~ cd_issue, data = df, mean)
colnames(table_age) <- c(prepro_nm[,1][4], prepro_nm[,1][20])
write.xlsx(table_age, 
           file = "C:/19Project/07.UCK/03.Output/03.DCB/DCB_EDA_All.xlsx",
           sheetName = prepro_nm[,2][7],
           append = TRUE,
           row.names = FALSE)

table_cd_ar <- data.table(table(df$cd_issue, df$age_ran))
table_cd_ar$V2 <- factor(table_cd_ar$V2, levels = c(prepro_nm[,1][21], prepro_nm[,1][22], prepro_nm[,1][23], prepro_nm[,1][24], prepro_nm[,1][25],
                                                    prepro_nm[,1][26], prepro_nm[,1][27], prepro_nm[,1][28], prepro_nm[,1][29], prepro_nm[,1][30]))
table_cd_ar <- dcast(table_cd_ar, V1 ~ V2, value.var = 'N')
names(table_cd_ar)[1] <- c(prepro_nm[,1][4])
write.xlsx(table_cd_ar, 
           file = "C:/19Project/07.UCK/03.Output/03.DCB/DCB_EDA_All.xlsx",
           sheetName = prepro_nm[,2][8],
           append = TRUE,
           row.names = FALSE)

table_h <- data.frame(table(df$h_reg_mw))
table_h <- table_h %>%
  mutate(percent = (Freq/sum(Freq)))
table_h$percent <- round(table_h$percent, 2)
names(table_h) <- c(prepro_nm[,1][5], prepro_nm[,1][2], prepro_nm[,1][3])
write.xlsx(table_h, 
           file = "C:/19Project/07.UCK/03.Output/03.DCB/DCB_EDA_All.xlsx",
           sheetName = prepro_nm[,2][9],
           append = TRUE,
           row.names = FALSE)

table_h_c <- data.frame(table(df$h_reg_mw, df$cd_issue))
table_h_c <- dcast(table_h_c, Var1 ~ Var2, value.var = 'Freq')
names(table_h_c)[1] <- c(prepro_nm[,1][5])
write.xlsx(table_h_c, 
           file = "C:/19Project/07.UCK/03.Output/03.DCB/DCB_EDA_All.xlsx",
           sheetName = prepro_nm[,2][10],
           append = TRUE,
           row.names = FALSE)

table_hr <- data.frame(table(df$h_ran))
table_hr <- table_hr %>%
  mutate(percent = (Freq/sum(Freq)))
table_hr$percent <- round(table_hr$percent, 2)
setorder(table_hr, -Freq)
colnames(table_hr) <- c(prepro_nm[,1][6], prepro_nm[,1][2], prepro_nm[,1][3])
write.xlsx(table_hr, 
           file = "C:/19Project/07.UCK/03.Output/03.DCB/DCB_EDA_All.xlsx",
           sheetName = prepro_nm[,2][11],
           append = TRUE,
           row.names = FALSE)

table_hr_c <- data.table(table(df$h_ran, df$cd_issue))
table_hr_c <- dcast(table_hr_c, V1 ~ V2, value.var = 'N')
setorder(table_hr_c, -V1)
names(table_hr_c)[1] <- c(prepro_nm[,1][6])
write.xlsx(table_hr_c, 
           file = "C:/19Project/07.UCK/03.Output/03.DCB/DCB_EDA_All.xlsx",
           sheetName = prepro_nm[,2][12],
           append = TRUE,
           row.names = FALSE)
