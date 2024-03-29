
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

kiosk <- read.csv('C:\\19project\\07.UCK\\01.Data\\03.Data_Preprocessing\\01.Kiosk\\Kiosk_Data_Preprocessing.csv', stringsAsFactors = FALSE)

EDA_nm <- read.csv('C:\\19project\\07.UCK\\01.Data\\04.Setting_data\\01.Kiosk\\02.EDA\\All\\kiosk_EDA_all_nm.csv', header = FALSE, stringsAsFactors = FALSE)

sd <- aggregate(dat_reg_mw ~ nm_k_c + k_char + addr_k + k_gid, data = kiosk, min)
sd <- rename(sd, 'start_date' = 'dat_reg_mw')
ed <- aggregate(dat_reg_mw ~ nm_k_c + k_char + addr_k + k_gid, data = kiosk, max)
ed <- rename(ed, 'end_date' = 'dat_reg_mw')

sed <- left_join(sd, ed, by = 'nm_k_c')
sed <- rename(sed, 'k_char' = 'k_char.x')
sed <- rename(sed, 'k_gid' = 'k_gid.x')
sed <- rename(sed, 'addr_k' = 'addr_k.x')
sed <- sed[, c(1:5, 9)]

k_count <- aggregate(count ~ nm_k_c, data = kiosk, sum)
k_count <- k_count %>%
  mutate(percent = ((count/sum(count)*100)))
k_count$percent <- round(k_count$percent, 2)

kiosk_list <- left_join(sed, k_count, by = 'nm_k_c')
colnames(kiosk_list) <- c(EDA_nm[, 1][1], EDA_nm[, 1][2], EDA_nm[, 1][3], EDA_nm[, 1][4], EDA_nm[, 1][5], EDA_nm[, 1][6], EDA_nm[, 1][7], EDA_nm[, 1][8])
write.csv(kiosk_list, 'C:\\19project\\07.UCK\\03.Output\\01.Kiosk\\Kiosk_List.csv', row.names = FALSE, fileEncoding = 'CP949')

kiosk_table <- data.table(table(kiosk$nm_k_c))
kiosk_table <- kiosk_table %>%
  mutate(percent = ((N/sum(N))*100))
kiosk_table$percent <- round(kiosk_table$percent, 2)
colnames(kiosk_table) <- c(EDA_nm[, 1][1], EDA_nm[, 1][7], EDA_nm[, 1][8])

kiosk_t_y <- data.table(table(kiosk$y_reg_mw, kiosk$nm_k_c))
kiosk_t_y <- dcast(kiosk_t_y, V2 ~ V1, value.var = 'N')
names(kiosk_t_y)[1] <- c(EDA_nm[, 1][1])

kiosk_sp_table <- data.table(table(kiosk$y_reg_mw, kiosk$nm_k_cf))
kiosk_sp_table <- dcast(kiosk_sp_table, V2 ~ V1, value.var = 'N')
names(kiosk_sp_table)[1] <- c(EDA_nm[, 1][1])

kiosk_y_table <- data.table(table(kiosk$y_reg_mw))
kiosk_y_table <- kiosk_y_table %>%
  mutate(percent = ((N/sum(N))*100))
kiosk_y_table$percent <- round(kiosk_y_table$percent, 2)
kiosk_y_table$V1 <- as.numeric(kiosk_y_table$V1)
setorder(kiosk_y_table, V1)
colnames(kiosk_y_table) <- c(EDA_nm[, 1][9], EDA_nm[, 1][7], EDA_nm[, 1][8])

kiosk_m_table <- data.table(table(kiosk$y_reg_mw, kiosk$m_reg_mw))
kiosk_m_table <- dcast(kiosk_m_table, V2 ~ V1, value.var = 'N')
kiosk_m_table$V2 <- as.numeric(kiosk_m_table$V2)
setorder(kiosk_m_table, V2)
names(kiosk_m_table)[1] <- c(EDA_nm[, 1][10])

kiosk$d_reg_mw <- factor(kiosk$d_reg_mw, levels = c(EDA_nm[, 1][27], EDA_nm[, 1][28], EDA_nm[, 1][29], EDA_nm[, 1][30], EDA_nm[, 1][31], EDA_nm[, 1][32], EDA_nm[, 1][33]))
kiosk_d_table <- data.table(table(kiosk$y_reg_mw, kiosk$d_reg_mw))
kiosk_d_table$V2 <- factor(kiosk_d_table$V2, levels = c(EDA_nm[, 1][27], EDA_nm[, 1][28], EDA_nm[, 1][29], EDA_nm[, 1][30], EDA_nm[, 1][31], EDA_nm[, 1][32], EDA_nm[, 1][33]))
kiosk_d_table <- dcast(kiosk_d_table, V2 ~ V1, value.var = 'N')
names(kiosk_d_table)[1] <- c(EDA_nm[, 1][11])

kiosk_h_table <- data.table(table(kiosk$y_reg_mw, kiosk$h_reg_mw))
kiosk_h_table <- dcast(kiosk_h_table, V2 ~ V1, value.var = 'N')
kiosk_h_table$V2 <- as.numeric(kiosk_h_table$V2)
h <- seq(0,23)
base_table <- expand.grid(V2 = h)
kiosk_h_table <- left_join(base_table, kiosk_h_table, by = "V2")
kiosk_h_table[is.na(kiosk_h_table)] <- 0
setorder(kiosk_h_table, V2)
names(kiosk_h_table)[1] <- c(EDA_nm[, 1][12])

kiosk_h_ran_table <- data.table(table(kiosk$y_reg_mw, kiosk$h_ran))
kiosk_h_ran_table <- dcast(kiosk_h_ran_table, V2 ~ V1, value.var = 'N')
setorder(kiosk_h_ran_table, -V2)
names(kiosk_h_ran_table)[1] <- c(EDA_nm[, 1][13])

kiosk_nm_min <- data.table(table(kiosk$y_reg_mw, kiosk$nm_min_mw))
kiosk_nm_min <- dcast(kiosk_nm_min, V2 ~ V1, value.var = 'N')
names(kiosk_nm_min)[1] <- c(EDA_nm[, 1][14])

kiosk_t3 <- aggregate(count ~ nm_k_cf + k_char, data = kiosk, sum)
kiosk_t3_f <- kiosk %>% group_by(nm_k_cf) %>% summarise(db = sum(nm_min_mw == EDA_nm[, 1][34]))
kiosk_t3_f$db_per <- ((kiosk_t3_f$db/sum(kiosk_t3_f$db))*100)
kiosk_t3_f$db_per <- round(kiosk_t3_f$db_per, 2)

kiosk_t3_f1 <- kiosk %>% group_by(nm_k_cf) %>% summarise(cb = sum(nm_min_mw == EDA_nm[, 1][35]))
kiosk_t3_f1$cb_per <- ((kiosk_t3_f1$cb/sum(kiosk_t3_f1$cb))*100)
kiosk_t3_f1$cb_per <- round(kiosk_t3_f1$cb_per, 2)

kiosk_t3_merge <- left_join(kiosk_t3_f, kiosk_t3_f1, by = 'nm_k_cf')

kiosk_t3_f2 <- kiosk %>% group_by(nm_k_cf) %>% summarise(gj = sum(nm_min_mw == EDA_nm[, 1][36]))
kiosk_t3_f2$gj_per <- ((kiosk_t3_f2$gj/sum(kiosk_t3_f2$gj))*100)
kiosk_t3_f2$gj_per <- round(kiosk_t3_f2$gj_per, 2)

kiosk_t3_merge <- left_join(kiosk_t3_merge, kiosk_t3_f2, by = 'nm_k_cf')
kiosk_t3_merge <- kiosk_t3_merge %>%
  mutate(tot = db + cb + gj)

kiosk_t3_merge <- kiosk_t3_merge %>%
  mutate(tot_per = (tot/sum(tot))*100)
kiosk_t3_merge$tot_per <- round(kiosk_t3_merge$tot_per, 2)

kiosk_t3 <- left_join(kiosk_t3, kiosk_t3_merge, by = 'nm_k_cf')
names(kiosk_t3)
colnames(kiosk_t3) <- c(EDA_nm[, 1][1], EDA_nm[, 1][2], EDA_nm[, 1][15], EDA_nm[, 1][16], EDA_nm[, 1][17], EDA_nm[, 1][18],
                        EDA_nm[, 1][19], EDA_nm[, 1][20], EDA_nm[, 1][21], EDA_nm[, 1][22], EDA_nm[, 1][23])

kiosk_t3_min <- data.table(table(kiosk$y_reg_mw, kiosk$nm_certi_mag))
kiosk_t3_min$V2 <- factor(kiosk_t3_min$V2, levels = c(EDA_nm[, 1][34], EDA_nm[, 1][35], EDA_nm[, 1][36], EDA_nm[, 1][37] ))
kiosk_t3_min <- dcast(kiosk_t3_min, V2 ~ V1, value.var = 'N')
names(kiosk_t3_min)[1] <- c(EDA_nm[, 1][24])

kiosk_io <- data.table(table(kiosk$y_reg_mw, kiosk$in_out))
kiosk_io <- dcast(kiosk_io, V2 ~ V1, value.var = 'N')
names(kiosk_io)[1] <- c(EDA_nm[, 1][25])

kiosk_age_ran <- data.table(table(kiosk$y_reg_mw, kiosk$age_ran))
kiosk_age_ran$V2 <- factor(kiosk_age_ran$V2, levels = c(EDA_nm[, 1][38], EDA_nm[, 1][39], EDA_nm[, 1][40], EDA_nm[, 1][41], EDA_nm[, 1][42],
                                                        EDA_nm[, 1][43], EDA_nm[, 1][44], EDA_nm[, 1][45], EDA_nm[, 1][46], EDA_nm[, 1][47]))
kiosk_age_ran <- dcast(kiosk_age_ran, V2 ~ V1, value.var = 'N')
names(kiosk_age_ran)[1] <- c(EDA_nm[, 1][26])


kiosk_t_y <- data.table(table(kiosk$y_reg_mw, kiosk$nm_k_c))
kiosk_t_y <- dcast(kiosk_t_y, V2 ~ V1, value.var = 'N')
names(kiosk_t_y)[1] <- c(EDA_nm[, 1][1])

Kiosk_EDA_All <- createWorkbook()

addWorksheet(Kiosk_EDA_All, EDA_nm[, 2][1])
addWorksheet(Kiosk_EDA_All, EDA_nm[, 2][2])
addWorksheet(Kiosk_EDA_All, EDA_nm[, 2][3])
addWorksheet(Kiosk_EDA_All, EDA_nm[, 2][4])
addWorksheet(Kiosk_EDA_All, EDA_nm[, 2][5])
addWorksheet(Kiosk_EDA_All, EDA_nm[, 2][6])
addWorksheet(Kiosk_EDA_All, EDA_nm[, 2][7])
addWorksheet(Kiosk_EDA_All, EDA_nm[, 2][8])
addWorksheet(Kiosk_EDA_All, EDA_nm[, 2][9])
addWorksheet(Kiosk_EDA_All, EDA_nm[, 2][10])
addWorksheet(Kiosk_EDA_All, EDA_nm[, 2][11])
addWorksheet(Kiosk_EDA_All, EDA_nm[, 2][12])
addWorksheet(Kiosk_EDA_All, EDA_nm[, 2][13])

writeDataTable(Kiosk_EDA_All, EDA_nm[, 2][1], kiosk_table)
writeDataTable(Kiosk_EDA_All, EDA_nm[, 2][2], kiosk_t_y)
writeDataTable(Kiosk_EDA_All, EDA_nm[, 2][3], kiosk_sp_table)
writeDataTable(Kiosk_EDA_All, EDA_nm[, 2][4], kiosk_y_table)
writeDataTable(Kiosk_EDA_All, EDA_nm[, 2][5], kiosk_m_table)
writeDataTable(Kiosk_EDA_All, EDA_nm[, 2][6], kiosk_d_table)
writeDataTable(Kiosk_EDA_All, EDA_nm[, 2][7], kiosk_h_table)
writeDataTable(Kiosk_EDA_All, EDA_nm[, 2][8], kiosk_h_ran_table)
writeDataTable(Kiosk_EDA_All, EDA_nm[, 2][9], kiosk_nm_min)
writeDataTable(Kiosk_EDA_All, EDA_nm[, 2][10], kiosk_t3)
writeDataTable(Kiosk_EDA_All, EDA_nm[, 2][11], kiosk_t3_min)
writeDataTable(Kiosk_EDA_All, EDA_nm[, 2][12], kiosk_io)
writeDataTable(Kiosk_EDA_All, EDA_nm[, 2][13], kiosk_age_ran)

saveWorkbook(Kiosk_EDA_All, 
             file="C:\\19project\\07.UCK\\03.Output\\01.Kiosk\\Kiosk_EDA_All.xlsx",
             overwrite=TRUE)