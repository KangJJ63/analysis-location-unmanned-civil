
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

kiosk <- read.csv('C:\\19project\\07.UCK\\01.Data\\03.Data_Preprocessing\\01.Kiosk\\Kiosk_Data_Preprocessing.csv', stringsAsFactors = FALSE)

EDA_nm <- read.csv('C:\\19project\\07.UCK\\01.Data\\04.Setting_data\\01.Kiosk\\02.EDA\\Each_Kiosk\\kiosk_EDA_EK_nm.csv', header = FALSE, stringsAsFactors = FALSE)

kiosk_uni <- unique(kiosk$nm_k_c)

kiosk_uni_cf <- unique(kiosk$nm_k_cf)

kty <- createWorkbook()
kmt <- createWorkbook()
kdt <- createWorkbook()
kht <- createWorkbook()
khrt <- createWorkbook()
knm <- createWorkbook()
kt3 <- createWorkbook()
kk <- createWorkbook()
kar <- createWorkbook()

for(i in kiosk_uni){
  select_kiosk <- kiosk[(kiosk$nm_k_c == i), ]
  
  setwd('C:\\19project\\07.UCK\\03.Output\\01.Kiosk\\01.Kiosk_EDA\\01.Each_Kiosk_Result')
  
  kiosk_t_y <- data.table(table(select_kiosk$y_reg_mw, select_kiosk$nm_k_c))
  kiosk_t_y <- dcast(kiosk_t_y, V2 ~ V1, value.var = 'N')
  names(kiosk_t_y)[1] <- c(EDA_nm[, 1][1])
  addWorksheet(kty, paste(i))
  writeDataTable(kty, paste(i), kiosk_t_y)
  
  kiosk_m_table <- data.table(table(select_kiosk$y_reg_mw, select_kiosk$m_reg_mw))
  kiosk_m_table <- dcast(kiosk_m_table, V2 ~ V1, value.var = 'N')
  kiosk_m_table$V2 <- as.numeric(kiosk_m_table$V2)
  setorder(kiosk_m_table, V2)
  names(kiosk_m_table)[1] <- c(EDA_nm[, 1][10])
  addWorksheet(kmt, paste(i))
  writeDataTable(kmt, paste(i), kiosk_m_table)
  
  kiosk_d_table <- data.table(table(select_kiosk$y_reg_mw, select_kiosk$d_reg_mw))
  kiosk_d_table$V2 <- factor(kiosk_d_table$V2, levels = c(EDA_nm[, 1][27], EDA_nm[, 1][28], EDA_nm[, 1][29], EDA_nm[, 1][30], EDA_nm[, 1][31], EDA_nm[, 1][32], EDA_nm[, 1][33]))
  kiosk_d_table <- dcast(kiosk_d_table, V2 ~ V1, value.var = 'N')
  names(kiosk_d_table)[1] <- c(EDA_nm[, 1][11])
  addWorksheet(kdt, paste(i))
  writeDataTable(kdt, paste(i), kiosk_d_table)

  kiosk_h_table <- data.table(table(select_kiosk$y_reg_mw, select_kiosk$h_reg_mw))
  kiosk_h_table <- dcast(kiosk_h_table, V2 ~ V1, value.var = 'N')
  kiosk_h_table$V2 <- as.numeric(kiosk_h_table$V2)
  h <- seq(0,23)
  base_table <- expand.grid(V2 = h)
  kiosk_h_table <- left_join(base_table, kiosk_h_table, by = "V2")
  kiosk_h_table[is.na(kiosk_h_table)] <- 0
  setorder(kiosk_h_table, V2)
  names(kiosk_h_table)[1] <- c(EDA_nm[, 1][12])
  addWorksheet(kht, paste(i))
  writeDataTable(kht, paste(i), kiosk_h_table)

  kiosk_h_ran_table <- data.table(table(select_kiosk$y_reg_mw, select_kiosk$h_ran))
  kiosk_h_ran_table <- dcast(kiosk_h_ran_table, V2 ~ V1, value.var = 'N')
  setorder(kiosk_h_ran_table, -V2)
  names(kiosk_h_ran_table)[1] <- c(EDA_nm[, 1][13])
  addWorksheet(khrt, paste(i))
  writeDataTable(khrt, paste(i), kiosk_h_ran_table)
  
  kiosk_nm_min <- data.table(table(select_kiosk$y_reg_mw, select_kiosk$nm_min_mw))
  kiosk_nm_min <- dcast(kiosk_nm_min, V2 ~ V1, value.var = 'N')
  names(kiosk_nm_min)[1] <- c(EDA_nm[, 1][14])
  addWorksheet(knm, paste(i))
  writeDataTable(knm, paste(i), kiosk_nm_min)
 
  kiosk_t3_min <- data.table(table(select_kiosk$y_reg_mw, select_kiosk$nm_certi_mag))
  kiosk_t3_min$V2 <- factor(kiosk_t3_min$V2, levels = c(EDA_nm[, 1][34], EDA_nm[, 1][35], EDA_nm[, 1][36], EDA_nm[, 1][37] ))
  kiosk_t3_min <- dcast(kiosk_t3_min, V2 ~ V1, value.var = 'N')
  names(kiosk_t3_min)[1] <- c(EDA_nm[, 1][24])
  addWorksheet(kt3, paste(i))
  writeDataTable(kt3, paste(i), kiosk_t3_min)
 
  kiosk_io <- data.table(table(select_kiosk$y_reg_mw, select_kiosk$in_out))
  kiosk_io <- dcast(kiosk_io, V2 ~ V1, value.var = 'N')
  names(kiosk_io)[1] <- c(EDA_nm[, 1][25])
  addWorksheet(kk, paste(i))
  writeDataTable(kk, paste(i), kiosk_io)

  kiosk_age_ran <- data.table(table(select_kiosk$y_reg_mw, select_kiosk$age_ran))
  kiosk_age_ran$V2 <- factor(kiosk_age_ran$V2, levels = c(EDA_nm[, 1][38], EDA_nm[, 1][39], EDA_nm[, 1][40], EDA_nm[, 1][41], EDA_nm[, 1][42],
                                                          EDA_nm[, 1][43], EDA_nm[, 1][44], EDA_nm[, 1][45], EDA_nm[, 1][46], EDA_nm[, 1][47]))
  kiosk_age_ran <- dcast(kiosk_age_ran, V2 ~ V1, value.var = 'N')
  names(kiosk_age_ran)[1] <- c(EDA_nm[, 1][26])
  addWorksheet(kar, paste(i))
  writeDataTable(kar, paste(i), kiosk_age_ran)
}

saveWorkbook(kty , file=EDA_nm[, 2][12],overwrite = TRUE)
saveWorkbook(kar , file=EDA_nm[, 2][20],overwrite = TRUE)
saveWorkbook(kk , file=EDA_nm[, 2][19],overwrite = TRUE)
saveWorkbook(kt3 , file=EDA_nm[, 2][18],overwrite = TRUE)
saveWorkbook(knm , file=EDA_nm[, 2][17],overwrite = TRUE)
saveWorkbook(khrt , file=EDA_nm[, 2][16],overwrite = TRUE)
saveWorkbook(kht , file=EDA_nm[, 2][15],overwrite = TRUE)
saveWorkbook(kdt , file=EDA_nm[, 2][14],overwrite = TRUE)
saveWorkbook(kty , file=EDA_nm[, 2][13],overwrite = TRUE)


for(i in kiosk_uni){
  select_kiosk <- kiosk[(kiosk$nm_k_c == i), ]
  setwd("C:\\19project\\07.UCK\\03.Output\\01.Kiosk\\01.Kiosk_EDA\\03.Each_Kiosk_1day_Result")
  table_1day <- aggregate(count ~ nm_k_c + dat_reg_mw, data = select_kiosk, length)
  names(table_1day) <- c(EDA_nm[, 1][1], EDA_nm[, 1][48], EDA_nm[, 1][7])
  write.csv(table_1day, 
            file = paste(i, EDA_nm[, 2][10], sep = '_'),
            row.names = FALSE)
}

kty1 <- createWorkbook()
kmt1 <- createWorkbook()
kdt1 <- createWorkbook()
kht1 <- createWorkbook()
khrt1 <- createWorkbook()
knm1 <- createWorkbook()
kt31 <- createWorkbook()
kio1 <- createWorkbook()
kar1 <- createWorkbook()
  
for(i in kiosk_uni_cf){
  select_kiosk_cf <- kiosk[(kiosk$nm_k_cf == i), ]  
  
  setwd('C:\\19project\\07.UCK\\03.Output\\01.Kiosk\\01.Kiosk_EDA\\02.Each_Same_Spot_Kiosk_Result')
  
  kiosk_t_y <- data.table(table(select_kiosk_cf$y_reg_mw, select_kiosk_cf$nm_k_c))
  kiosk_t_y <- dcast(kiosk_t_y, V2 ~ V1, value.var = 'N')
  names(kiosk_t_y)[1] <- c(EDA_nm[, 1][1])
  addWorksheet(kty1 , paste(i))
  writeDataTable(kty1 , paste(i), kiosk_t_y)
  
  kiosk_m_table <- data.table(table(select_kiosk_cf$y_reg_mw, select_kiosk_cf$m_reg_mw))
  kiosk_m_table <- dcast(kiosk_m_table, V2 ~ V1, value.var = 'N')
  kiosk_m_table$V2 <- as.numeric(kiosk_m_table$V2)
  setorder(kiosk_m_table, V2)
  names(kiosk_m_table)[1] <- c(EDA_nm[, 1][10])
  addWorksheet(kmt1 , paste(i))
  writeDataTable(kmt1 , paste(i), kiosk_m_table)

  kiosk_d_table <- data.table(table(select_kiosk_cf$y_reg_mw, select_kiosk_cf$d_reg_mw))
  kiosk_d_table$V2 <- factor(kiosk_d_table$V2, levels = c(EDA_nm[, 1][27], EDA_nm[, 1][28], EDA_nm[, 1][29], EDA_nm[, 1][30], EDA_nm[, 1][31], EDA_nm[, 1][32], EDA_nm[, 1][33]))
  kiosk_d_table <- dcast(kiosk_d_table, V2 ~ V1, value.var = 'N')
  names(kiosk_d_table)[1] <- c(EDA_nm[, 1][11])
  addWorksheet(kdt1 , paste(i))
  writeDataTable(kdt1 , paste(i), kiosk_d_table)
  
  kiosk_h_table <- data.table(table(select_kiosk_cf$y_reg_mw, select_kiosk_cf$h_reg_mw))
  kiosk_h_table <- dcast(kiosk_h_table, V2 ~ V1, value.var = 'N')
  kiosk_h_table$V2 <- as.numeric(kiosk_h_table$V2)
  h <- seq(0,23)
  base_table <- expand.grid(V2 = h)
  kiosk_h_table <- left_join(base_table, kiosk_h_table, by = "V2")
  kiosk_h_table[is.na(kiosk_h_table)] <- 0
  setorder(kiosk_h_table, V2)
  names(kiosk_h_table)[1] <- c(EDA_nm[, 1][12])
  addWorksheet(kht1 , paste(i))
  writeDataTable(kht1 , paste(i), kiosk_h_table)

  kiosk_h_ran_table <- data.table(table(select_kiosk_cf$y_reg_mw, select_kiosk_cf$h_ran))
  kiosk_h_ran_table <- dcast(kiosk_h_ran_table, V2 ~ V1, value.var = 'N')
  setorder(kiosk_h_ran_table, -V2)
  names(kiosk_h_ran_table)[1] <- c(EDA_nm[, 1][13])
  addWorksheet(khrt1 , paste(i))
  writeDataTable(khrt1 , paste(i), kiosk_h_ran_table)

  kiosk_nm_min <- data.table(table(select_kiosk_cf$y_reg_mw, select_kiosk_cf$nm_min_mw))
  kiosk_nm_min <- dcast(kiosk_nm_min, V2 ~ V1, value.var = 'N')
  names(kiosk_nm_min)[1] <- c(EDA_nm[, 1][14])
  addWorksheet(knm1 , paste(i))
  writeDataTable(knm1 , paste(i), kiosk_nm_min)

  kiosk_t3_min <- data.table(table(select_kiosk_cf$y_reg_mw, select_kiosk_cf$nm_certi_mag))
  kiosk_t3_min$V2 <- factor(kiosk_t3_min$V2, levels = c(EDA_nm[, 1][34], EDA_nm[, 1][35], EDA_nm[, 1][36], EDA_nm[, 1][37] ))
  kiosk_t3_min <- dcast(kiosk_t3_min, V2 ~ V1, value.var = 'N')
  names(kiosk_t3_min)[1] <- c(EDA_nm[, 1][24])
  addWorksheet(kt31 , paste(i))
  writeDataTable(kt31 , paste(i), kiosk_t3_min)

  kiosk_io <- data.table(table(select_kiosk_cf$y_reg_mw, select_kiosk_cf$in_out))
  kiosk_io <- dcast(kiosk_io, V2 ~ V1, value.var = 'N')
  names(kiosk_io)[1] <- c(EDA_nm[, 1][25])
  addWorksheet(kio1 , paste(i))
  writeDataTable(kio1 , paste(i), kiosk_io)

  kiosk_age_ran <- data.table(table(select_kiosk_cf$y_reg_mw, select_kiosk_cf$age_ran))
  kiosk_age_ran$V2 <- factor(kiosk_age_ran$V2, levels = c(EDA_nm[, 1][38], EDA_nm[, 1][39], EDA_nm[, 1][40], EDA_nm[, 1][41], EDA_nm[, 1][42],
                                                          EDA_nm[, 1][43], EDA_nm[, 1][44], EDA_nm[, 1][45], EDA_nm[, 1][46], EDA_nm[, 1][47]))
  kiosk_age_ran <- dcast(kiosk_age_ran, V2 ~ V1, value.var = 'N')
  names(kiosk_age_ran)[1] <- c(EDA_nm[, 1][26])
  addWorksheet(kar1 , paste(i))
  writeDataTable(kar1 , paste(i), kiosk_age_ran)
}

saveWorkbook(kar1 , file=EDA_nm[, 2][20],overwrite = TRUE)
saveWorkbook(kio1 , file=EDA_nm[, 2][19],overwrite = TRUE)
saveWorkbook(kt31 , file=EDA_nm[, 2][18],overwrite = TRUE)
saveWorkbook(knm1 , file=EDA_nm[, 2][17],overwrite = TRUE)
saveWorkbook(khrt1 , file=EDA_nm[, 2][16],overwrite = TRUE)
saveWorkbook(kht1 , file=EDA_nm[, 2][15],overwrite = TRUE)
saveWorkbook(kdt1 , file=EDA_nm[, 2][14],overwrite = TRUE)
saveWorkbook(kmt1 , file=EDA_nm[, 2][13],overwrite = TRUE) 
saveWorkbook(kty1 , file=EDA_nm[, 2][12],overwrite = TRUE)

for(i in kiosk_uni_cf){
  select_kiosk_cf <- kiosk[(kiosk$nm_k_cf == i), ]
  setwd("C:\\19project\\07.UCK\\03.Output\\01.Kiosk\\01.Kiosk_EDA\\04.Each_Same_Spot_Kiosk_1day_Result")
  table_1day <- aggregate(count ~ nm_k_c + dat_reg_mw, data = select_kiosk_cf, length)
  names(table_1day) <- c(EDA_nm[, 1][1], EDA_nm[, 1][48], EDA_nm[, 1][7])
  write.csv(table_1day, 
            file = paste(i, EDA_nm[, 2][11], sep = '_'),
            row.names = FALSE)
}