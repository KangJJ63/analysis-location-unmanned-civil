
library(readxl)
library(lubridate)
library(stringr)
library(WriteXLS)
library(hms)
library(data.table)
library(dplyr)
library(ggplot2)
library(reshape2)
library(openxlsx)
library(WriteXLS)

db <- read.csv('C:\\19project\\07.UCK\\01.Data\\03.Data_Preprocessing\\02.DB_Pub_Counter\\DB_Pub_Counter_Data_Preprocessing.csv', stringsAsFactors = FALSE)

EDA_nm <- read.csv('C:\\19project\\07.UCK\\01.Data\\04.Setting_data\\02.DB_Pub_Counter\\02.EDA\\All\\db_EDA_all_nm.csv', header = FALSE, stringsAsFactors = FALSE)

sd <- aggregate(dt_mw ~ nm_d_c + nm_d_addr + d_gid, data = db, min)
sd <- rename(sd, 'start_date' = 'dt_mw')
ed <- aggregate(dt_mw ~ nm_d_c + nm_d_addr + d_gid, data = db, max)
ed <- rename(ed, 'end_date' = 'dt_mw')

sed <- left_join(sd, ed, by = 'nm_d_c')
sed <- rename(sed, 'nm_d_addr' = 'nm_d_addr.x')
sed <- rename(sed, 'd_gid' = 'd_gid.x')
sed <- sed[, c(1:4, 7)]

db_count <- aggregate(count ~ nm_d_c, data = db, sum)
db_count <- db_count %>%
  mutate(percent = ((count/sum(count)*100)))
db_count$percent <- round(db_count$percent, 2)

db_list <- left_join(sed, db_count, by = 'nm_d_c')
names(db_list)
colnames(db_list) <- c(EDA_nm[, 1][1], EDA_nm[, 1][2], EDA_nm[, 1][3], EDA_nm[, 1][4], EDA_nm[, 1][5], EDA_nm[, 1][6], EDA_nm[, 1][7])
write.csv(db_list, 'C:\\19project\\07.UCK\\03.Output\\02.DB_Pub_Counter\\DB_Pub_Counter_List.csv', row.names = FALSE)

table_db <- data.table(table(db$nm_d_c))
table_db <- table_db %>%
  mutate(percent = ((N/sum(N))*100))
table_db$percent <- round(table_db$percent, 2)
names(table_db) <- c(EDA_nm[, 1][1], EDA_nm[, 1][8], EDA_nm[, 1][9])

table_year <- data.table(table(db$y_reg_mw))
table_year <- table_year %>%
  mutate(percent = ((N/sum(N))*100))
table_year$percent <- round(table_year$percent, 2)
names(table_year) <- c(EDA_nm[, 1][10], EDA_nm[, 1][8], EDA_nm[, 1][9])

table_y_c <- data.table(table(db$y_reg_mw, db$nm_d_c))
table_y_c <- dcast(table_y_c, V2 ~ V1, value.var = 'N')
names(table_y_c)[1] <- c(EDA_nm[, 1][1])

table_y_mon <- data.table(table(db$y_reg_mw, db$m_reg_mw))
table_y_mon <- dcast(table_y_mon, V2 ~ V1, value.var = 'N')
table_y_mon$V2 <- as.numeric(table_y_mon$V2)
setorder(table_y_mon, V2)
names(table_y_mon)[1] <- c(EDA_nm[, 1][11])

table_y_day <- data.table(table(db$y_reg_mw, db$d_reg_mw))
table_y_day$V2 <- factor(table_y_day$V2, levels = c(EDA_nm[, 1][12], EDA_nm[, 1][13], EDA_nm[, 1][14], EDA_nm[, 1][15], EDA_nm[, 1][16], EDA_nm[, 1][17], EDA_nm[, 1][18]))
table_y_day <- dcast(table_y_day, V2 ~ V1, value.var = 'N')
names(table_y_day)[1] <- c(EDA_nm[, 1][19])

table_y_h <- data.table(table(db$y_reg_mw, db$h_reg_mw))
table_y_h <- dcast(table_y_h, V2 ~ V1, value.var = 'N')
table_y_h$V2 <- as.numeric(table_y_h$V2)
h <- seq(0,23)
base_table <- expand.grid(V2 = h)
table_y_h <- left_join(base_table, table_y_h, by = "V2")
table_y_h[is.na(table_y_h)] <- 0
setorder(table_y_h, V2)
names(table_y_h)[1] <- c(EDA_nm[, 1][20])

table_y_h_ran <- data.table(table(db$y_reg_mw, db$h_ran))
table_y_h_ran <- dcast(table_y_h_ran, V2 ~ V1, value.var = 'N')
setorder(table_y_h_ran, -V2)
names(table_y_h_ran)[1] <- c(EDA_nm[, 1][21])

table_y_io <- data.table(table(db$y_reg_mw, db$in_out))
table_y_io <- dcast(table_y_io, V2 ~ V1, value.var = 'N')
names(table_y_io)[1] <- c(EDA_nm[, 1][22])

db$age_ran <- factor(db$age_ran, levels = c(EDA_nm[, 1][23], EDA_nm[, 1][24], EDA_nm[, 1][25], EDA_nm[, 1][26], EDA_nm[, 1][27], EDA_nm[, 1][28], EDA_nm[, 1][29], EDA_nm[, 1][30], EDA_nm[, 1][31], EDA_nm[, 1][32]))
table_y_age_r <- data.table(table(db$y_reg_mw, db$age_ran))
table_y_age_r$V2 <- factor(table_y_age_r$V2, levels = c(EDA_nm[, 1][23], EDA_nm[, 1][24], EDA_nm[, 1][25], EDA_nm[, 1][26], EDA_nm[, 1][27], EDA_nm[, 1][28], EDA_nm[, 1][29], EDA_nm[, 1][30], EDA_nm[, 1][31], EDA_nm[, 1][32]))
table_y_age_r <- dcast(table_y_age_r, V2 ~ V1, value.var = 'N')
names(table_y_age_r)[1] <- c(EDA_nm[, 1][33])

DB_Pub_Counter_EDA_All <- createWorkbook()

addWorksheet(DB_Pub_Counter_EDA_All, EDA_nm[, 2][1])
addWorksheet(DB_Pub_Counter_EDA_All, EDA_nm[, 2][2])
addWorksheet(DB_Pub_Counter_EDA_All, EDA_nm[, 2][3])
addWorksheet(DB_Pub_Counter_EDA_All, EDA_nm[, 2][4])
addWorksheet(DB_Pub_Counter_EDA_All, EDA_nm[, 2][5])
addWorksheet(DB_Pub_Counter_EDA_All, EDA_nm[, 2][6])
addWorksheet(DB_Pub_Counter_EDA_All, EDA_nm[, 2][7])
addWorksheet(DB_Pub_Counter_EDA_All, EDA_nm[, 2][8])
addWorksheet(DB_Pub_Counter_EDA_All, EDA_nm[, 2][9])

writeDataTable(DB_Pub_Counter_EDA_All, EDA_nm[, 2][1], table_db)
writeDataTable(DB_Pub_Counter_EDA_All, EDA_nm[, 2][2], table_year)
writeDataTable(DB_Pub_Counter_EDA_All, EDA_nm[, 2][3], table_y_c)
writeDataTable(DB_Pub_Counter_EDA_All, EDA_nm[, 2][4], table_y_mon)
writeDataTable(DB_Pub_Counter_EDA_All, EDA_nm[, 2][5], table_y_day)
writeDataTable(DB_Pub_Counter_EDA_All, EDA_nm[, 2][6], table_y_h)
writeDataTable(DB_Pub_Counter_EDA_All, EDA_nm[, 2][7], table_y_h_ran)
writeDataTable(DB_Pub_Counter_EDA_All, EDA_nm[, 2][8], table_y_io)
writeDataTable(DB_Pub_Counter_EDA_All, EDA_nm[, 2][9], table_y_age_r)

saveWorkbook(DB_Pub_Counter_EDA_All, 
             file="C:\\19project\\07.UCK\\03.Output\\02.DB_Pub_Counter\\DB_Pub_Counter_EDA_All.xlsx",
             overwrite = TRUE)
