
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

EDA_nm <- read.csv('C:\\19project\\07.UCK\\01.Data\\04.Setting_data\\02.DB_Pub_Counter\\02.EDA\\Each_Pub_Counter\\db_EDA_EP_nm.csv', header = FALSE, stringsAsFactors = FALSE)

db_uni <- unique(db$nm_d_c)

DB_EDA_table_year <- createWorkbook()
DB_EDA_table_y_mon <- createWorkbook()
DB_EDA_table_y_day  <- createWorkbook()
DB_EDA_table_y_h  <- createWorkbook()
DB_EDA_table_y_h_ran  <- createWorkbook()
DB_EDA_table_y_io <- createWorkbook()
DB_EDA_table_y_age_r  <- createWorkbook()


for(i in db_uni){
  db_select_po <- db[(db$nm_d_c == i), ]
  
  setwd('C:\\19project\\07.UCK\\03.Output\\02.DB_Pub_Counter\\01.DB_Pub_Counter_EDA\\01.Each_DB_Pub_Counter_Result')
  
  table_year <- data.table(table(db_select_po$y_reg_mw))
  table_year <- table_year %>%
    mutate(percent = ((N/sum(N))*100))
  table_year$percent <- round(table_year$percent, 2)
  names(table_year) <- c(EDA_nm[, 1][10], EDA_nm[, 1][8], EDA_nm[, 1][9])


  
  table_y_mon <- data.table(table(db_select_po$y_reg_mw, db_select_po$m_reg_mw))
  table_y_mon <- dcast(table_y_mon, V2 ~ V1, value.var = 'N')
  table_y_mon$V2 <- as.numeric(table_y_mon$V2)
  setorder(table_y_mon, V2)
  names(table_y_mon)[1] <- c(EDA_nm[, 1][11])

  
  table_y_day <- data.table(table(db_select_po$y_reg_mw, db_select_po$d_reg_mw))
  table_y_day$V2 <- factor(table_y_day$V2, levels = c(EDA_nm[, 1][12], EDA_nm[, 1][13], EDA_nm[, 1][14], EDA_nm[, 1][15], EDA_nm[, 1][16], EDA_nm[, 1][17], EDA_nm[, 1][18]))
  table_y_day <- dcast(table_y_day, V2 ~ V1, value.var = 'N')
  names(table_y_day)[1] <- c(EDA_nm[, 1][19])

  table_y_h <- data.table(table(db_select_po$y_reg_mw, db_select_po$h_reg_mw))
  table_y_h <- dcast(table_y_h, V2 ~ V1, value.var = 'N')
  table_y_h$V2 <- as.numeric(table_y_h$V2)
  h <- seq(0,23)
  base_table <- expand.grid(V2 = h)
  table_y_h <- left_join(base_table, table_y_h, by = "V2")
  table_y_h[is.na(table_y_h)] <- 0
  setorder(table_y_h, V2)
  names(table_y_h)[1] <- c(EDA_nm[, 1][20])

 
  table_y_h_ran <- data.table(table(db_select_po$y_reg_mw, db_select_po$h_ran))
  table_y_h_ran <- dcast(table_y_h_ran, V2 ~ V1, value.var = 'N')
  setorder(table_y_h_ran, -V2)
  names(table_y_h_ran)[1] <- c(EDA_nm[, 1][21])

  
  table_y_io <- data.table(table(db_select_po$y_reg_mw, db_select_po$in_out))
  table_y_io <- dcast(table_y_io, V2 ~ V1, value.var = 'N')
  names(table_y_io)[1] <- c(EDA_nm[, 1][22])
 
  table_y_age_r <- data.table(table(db_select_po$y_reg_mw, db_select_po$age_ran))
  table_y_age_r$V2 <- factor(table_y_age_r$V2, levels = c(EDA_nm[, 1][23], EDA_nm[, 1][24], EDA_nm[, 1][25], EDA_nm[, 1][26], EDA_nm[, 1][27], EDA_nm[, 1][28], EDA_nm[, 1][29], EDA_nm[, 1][30], EDA_nm[, 1][31], EDA_nm[, 1][32]))
  table_y_age_r <- dcast(table_y_age_r, V2 ~ V1, value.var = 'N')
  names(table_y_age_r)[1] <- c(EDA_nm[, 1][33])

  addWorksheet(DB_EDA_table_year , paste(i))
  writeDataTable(DB_EDA_table_year , paste(i), table_year)

  addWorksheet(DB_EDA_table_y_mon , paste(i))
  writeDataTable(DB_EDA_table_y_mon , paste(i), table_y_mon)

  addWorksheet(DB_EDA_table_y_day , paste(i))
  writeDataTable(DB_EDA_table_y_day , paste(i), table_y_day)

  addWorksheet(DB_EDA_table_y_h , paste(i))
  writeDataTable(DB_EDA_table_y_h , paste(i), table_y_h)

  addWorksheet(DB_EDA_table_y_h_ran , paste(i))
  writeDataTable(DB_EDA_table_y_h_ran , paste(i), table_y_h_ran)

  addWorksheet(DB_EDA_table_y_io , paste(i))
  writeDataTable(DB_EDA_table_y_io , paste(i), table_y_io)

  addWorksheet(DB_EDA_table_y_age_r , paste(i))
  writeDataTable(DB_EDA_table_y_age_r , paste(i), table_y_age_r)
}
saveWorkbook(DB_EDA_table_year , file=EDA_nm[, 2][9],overwrite = TRUE)
saveWorkbook(DB_EDA_table_y_mon , file=EDA_nm[, 2][10],overwrite = TRUE)
saveWorkbook(DB_EDA_table_y_day , file=EDA_nm[, 2][11],overwrite = TRUE)
saveWorkbook(DB_EDA_table_y_h , file=EDA_nm[, 2][12],overwrite = TRUE)
saveWorkbook(DB_EDA_table_y_h_ran , file=EDA_nm[, 2][13],overwrite = TRUE)
saveWorkbook(DB_EDA_table_y_io , file=EDA_nm[, 2][14],overwrite = TRUE)
saveWorkbook(DB_EDA_table_y_age_r , file=EDA_nm[, 2][15],overwrite = TRUE)

for(i in db_uni){
  db_select_po1 <- db[(db$nm_d_c == i), ]
  setwd("C:\\19project\\07.UCK\\03.Output\\02.DB_Pub_Counter\\01.DB_Pub_Counter_EDA\\02.Each_DB_Pub_Counter_1day_Result")
  table_1day <- aggregate(count ~ nm_d_c + dt_mw, data = db_select_po1, length)
  names(table_1day) <- c(EDA_nm[, 1][1], EDA_nm[, 1][34], EDA_nm[, 1][8])
  write.csv(table_1day, 
            file = paste(i, EDA_nm[, 2][8], sep = '_'),
            row.names = FALSE)
}