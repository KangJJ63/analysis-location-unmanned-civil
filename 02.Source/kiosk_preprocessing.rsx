
library(readxl)
library(lubridate)
library(stringr)
library(WriteXLS)
library(hms)
library(data.table)
library(dplyr)
library(ggplot2)
library(reshape2)

df <- read.csv('C:\\19project\\07.UCK\\01.Data\\03.Data_Preprocessing\\01.Kiosk\\Reference_Data\\kiosk_apply_list_gid_dist.csv', stringsAsFactors = FALSE)

df$dist_g[is.na(df$dist_g)] <- ''

df$uni_id <- str_trim(df$uni_id, side = c("both"))
df$num_mw <- str_trim(df$num_mw, side = c("both"))
df$nm_k_c <- str_trim(df$nm_k_c, side = c("both"))
df$nm_k_cf <- str_trim(df$nm_k_cf, side = c("both"))
df$bir_app_mw <- str_trim(df$bir_app_mw, side = c("both"))
df$num_min_mw <- str_trim(df$num_min_mw, side = c("both"))
df$nm_min_mw <- str_trim(df$nm_min_mw, side = c("both"))
df$dat_reg_mw <- str_trim(df$dat_reg_mw, side = c("both"))
df$tim_reg_mw <- str_trim(df$tim_reg_mw, side = c("both"))
df$d_reg_mw <- str_trim(df$d_reg_mw, side = c("both"))
df$apply_addr <- str_trim(df$apply_addr, side = c("both"))
df$x_app_uk <- str_trim(df$x_app_uk, side = c("both"))
df$y_app_uk <- str_trim(df$y_app_uk, side = c("both"))
df$res_geo_mw <- str_trim(df$res_geo_mw, side = c("both"))
df$a_gid <- str_trim(df$a_gid, side = c("both"))
df$k_char <- str_trim(df$k_char, side = c("both"))
df$addr_k <- str_trim(df$addr_k, side = c("both"))
df$x_k_uk <- str_trim(df$x_k_uk, side = c("both"))
df$y_k_uk <- str_trim(df$y_k_uk, side = c("both"))
df$k_gid <- str_trim(df$k_gid, side = c("both"))

prepro_res_geo <- read.csv('C:\\19project\\07.UCK\\01.Data\\04.Setting_data\\01.Kiosk\\01.Preprocessing\\res_geo_nm.csv', header = F, stringsAsFactors = FALSE)

df$res_geo_mw[is.na(df$res_geo_mw)] <- prepro_res_geo[, 1][1]

df <- df[!df$res_geo_mw == prepro_res_geo[, 1][2],]
df <- df[!df$res_geo_mw == prepro_res_geo[, 1][3],]
df <- df[!df$res_geo_mw == prepro_res_geo[, 1][4],]
df <- df[!df$res_geo_mw == prepro_res_geo[, 1][5],]
df <- df[!df$res_geo_mw == prepro_res_geo[, 1][6],]
df <- df[!df$res_geo_mw == prepro_res_geo[, 1][7],]

df$dat_reg_mw <- ymd(df$dat_reg_mw)
df$tim_reg_mw <- str_pad(df$tim_reg_mw, 6, side = 'left', pad = '0')
df$tim_reg_mw <- sapply(df$tim_reg_mw, function(x)
  paste(str_sub(x, -6, -5),
        str_sub(x, -4, -3),
        str_sub(x, -2),
        sep = ':'),
  USE.NAMES = FALSE
)

df$y_reg_mw <- year(df$dat_reg_mw)

df$m_reg_mw <- month(df$dat_reg_mw)
df$m_reg_mw <- str_pad(df$m_reg_mw, 2, side = 'left', pad = '0')

df$ym_reg_mw <- paste(df$y_reg_mw, df$m_reg_mw, sep = '')

df$h_reg_mw <- str_sub(df$tim_reg_mw, 1,2)

df$d_reg_mw <- lubridate::wday(df$dat_reg_mw, label = TRUE, abbr = FALSE)
df$d_reg_mw <- as.character(df$d_reg_mw)

prepro_h_ran <- read.csv('C:\\19project\\07.UCK\\01.Data\\04.Setting_data\\01.Kiosk\\01.Preprocessing\\h_ran_nm.csv', header = FALSE, stringsAsFactors = FALSE)

df$h_ran <- ifelse(df$h_reg_mw >= '09' & df$h_reg_mw < '18', prepro_h_ran[, 1][1],
                   ifelse(df$h_reg_mw >= '18' & df$h_reg_mw <= '23', prepro_h_ran[, 1][2],
                          ifelse(df$h_reg_mw >= '00' & df$h_reg_mw < '05', prepro_h_ran[, 1][3], prepro_h_ran[, 1][4]
                          )))

prepro_min_t3 <- read.csv('C:\\19project\\07.UCK\\01.Data\\04.Setting_data\\01.Kiosk\\01.Preprocessing\\min_top3_nm.csv', header = FALSE, stringsAsFactors = FALSE)

df$certi_mag <- ifelse((df$nm_min_mw == prepro_min_t3[, 1][1])|(df$nm_min_mw == prepro_min_t3[, 1][2])|
                         (df$nm_min_mw == prepro_min_t3[, 1][3]), 1, 0)

df$nm_certi_mag <- ifelse(df$nm_min_mw == prepro_min_t3[, 1][1], prepro_min_t3[, 1][1], df$nm_min_mw)
df$nm_certi_mag <- ifelse(df$nm_min_mw == prepro_min_t3[, 1][2], prepro_min_t3[, 1][2], df$nm_min_mw)
df$nm_certi_mag <- ifelse(df$nm_min_mw == prepro_min_t3[, 1][3], prepro_min_t3[, 1][3], df$nm_min_mw)
df$nm_certi_mag <- ifelse(df$certi_mag == 0, prepro_min_t3[, 1][4], df$nm_min_mw)

df$bir_app_mw <- str_pad(df$bir_app_mw, 6, side = "left", pad = "0")

df$y_bir_app <- str_sub(df$bir_app_mw, 1,2)
table(df$y_bir_app)
df$y_bir_app <- ifelse(df$y_bir_app == "00", "0", df$y_bir_app)
df$y_bir_app <- ifelse(df$y_bir_app == "01", "1", df$y_bir_app)
df$y_bir_app <- ifelse(df$y_bir_app == "02", "2", df$y_bir_app)
df$y_bir_app <- ifelse(df$y_bir_app == "05", "5", df$y_bir_app)
table(df$y_bir_app)

df_19 <- data.frame()
if (length(which(nchar(df$y_bir_app) == 2)) > 0) {  
 idx_19 <- which(nchar(df$y_bir_app) == 2)
df_19 <- df[idx_19, ]
df_19$y_bir_app <- paste0('19', df_19$y_bir_app)
} 

df_20 <- data.frame()
if (length(which(nchar(df$y_bir_app) == 1)) > 0) {
idx_20 <- which(nchar(df$y_bir_app) == 1)
df_20 <- df[idx_20, ]
df_20$y_bir_app <- paste0('200', df_20$y_bir_app)
table(df_20$y_bir_app)
}

idx_na <- which(is.na(df$y_bir_app) == TRUE)
table(idx_na)
df_na <- df[idx_na, ]

df <- rbind(df_19, df_20, df_na)
rm(df_19); 
rm(df_20);
rm(df_na);
rm(idx_19); 
rm(idx_20); 
rm(idx_na);

df$age_mw <- year(today()) - as.numeric(df$y_bir_app)

prepro_age <- read.csv('C:\\19project\\07.UCK\\01.Data\\04.Setting_data\\01.Kiosk\\01.Preprocessing\\age_ran_nm.csv', header = FALSE, stringsAsFactors = FALSE)

df$age_mw <- as.integer(df$age_mw)

df$age_ran <- NA

idx <- which(df$age_mw < 20) 
df$age_ran[idx] <- prepro_age[, 1][1]
rm(idx)
idx <- which(df$age_mw < 30 & df$age_mw >= 20)
df$age_ran[idx] <- prepro_age[, 1][2]
rm(idx)
idx <- which(df$age_mw < 40 & df$age_mw >= 30)
df$age_ran[idx] <- prepro_age[, 1][3]
rm(idx)
idx <- which(df$age_mw < 50 & df$age_mw >= 40)
df$age_ran[idx] <- prepro_age[, 1][4]
rm(idx)
idx <- which(df$age_mw < 60 & df$age_mw >= 50)
df$age_ran[idx] <- prepro_age[, 1][5]
rm(idx)
idx <- which(df$age_mw < 70 & df$age_mw >= 60)
df$age_ran[idx] <- prepro_age[, 1][6]
rm(idx)
idx <- which(df$age_mw < 80 & df$age_mw >= 70)
df$age_ran[idx] <- prepro_age[, 1][7]
rm(idx)
idx <- which(df$age_mw < 90 & df$age_mw >= 80)
df$age_ran[idx] <- prepro_age[, 1][8]
rm(idx)
idx <- which(df$age_mw < 100 & df$age_mw >= 90)
df$age_ran[idx] <- prepro_age[, 1][9]
rm(idx)
idx <- which(df$age_mw >= 100)
df$age_ran[idx] <- prepro_age[, 1][10]
rm(idx)

prepro_day <- read.csv('C:\\19project\\07.UCK\\01.Data\\04.Setting_data\\01.Kiosk\\01.Preprocessing\\jj_day_nm.csv', header = FALSE, stringsAsFactors = FALSE)

df$jj <- ifelse((df$d_reg_mw == prepro_day[, 1][1])|(df$d_reg_mw == prepro_day[, 1][2])|(df$d_reg_mw == prepro_day[, 1][3])|(df$d_reg_mw == prepro_day[, 1][4])|(df$d_reg_mw == prepro_day[, 1][5])
                , prepro_day[, 1][8], prepro_day[, 1][9])  

df$count <- 1

prepro_io <- read.csv('C:\\19project\\07.UCK\\01.Data\\04.Setting_data\\01.Kiosk\\01.Preprocessing\\in_out_nm.csv', header = FALSE, stringsAsFactors = FALSE)

df[df$add_appl == ""] <- NA

df$in_out <- ifelse(df$a_gid == "", prepro_io[, 1][1], prepro_io[, 1][2])
df$in_out <- ifelse(is.na(df$apply_addr == TRUE), prepro_io[, 1][3], df$in_out)

write.csv(df, 'C:\\19project\\07.UCK\\01.Data\\03.Data_Preprocessing\\01.Kiosk\\Kiosk_Data_Preprocessing.csv', row.names = FALSE, fileEncoding = 'CP949')