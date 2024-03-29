
library(readxl)
library(lubridate)
library(stringr)
library(WriteXLS)
library(hms)
library(data.table)
library(dplyr)
library(ggplot2)
library(openxlsx)
library(hms)
library(reshape2)

df <- read.csv('C:\\19project\\07.UCK\\01.Data\\03.Data_Preprocessing\\02.DB_Pub_Counter\\Reference_Data\\db_pub_counter_apply_list_gid_dist.csv', stringsAsFactors = FALSE)

df$dist_g[is.na(df$dist_g)] <- ''

df$uni_id <- str_trim(df$uni_id, side = c("both"))
df$nm_d_c <- str_trim(df$nm_d_c, side = c("both"))
df$dt_mw <- str_trim(df$dt_mw, side = c("both"))
df$hr_mw <- str_trim(df$hr_mw, side = c("both"))
df$cd_dngcho <- str_trim(df$cd_dngcho, side = c("both"))
df$cd_issue <- str_trim(df$cd_issue, side = c("both"))
df$cd_koreng <- str_trim(df$cd_koreng, side = c("both"))
df$cd_prvpub <- str_trim(df$cd_prvpub, side = c("both"))
df$cd_reciev <- str_trim(df$cd_reciev, side = c("both"))
df$sex_apl <- str_trim(df$sex_apl, side = c("both"))
df$nm_svr <- str_trim(df$nm_svr, side = c("both"))
df$nm_svr_id <- str_trim(df$nm_svr_id, side = c("both"))
df$age_apl <- str_trim(df$age_apl, side = c("both"))
df$add_apl <- str_trim(df$add_apl, side = c("both"))
df$x_app_uk <- str_trim(df$x_app_uk, side = c("both"))
df$y_app_uk <- str_trim(df$y_app_uk, side = c("both"))
df$res_geo_mw <- str_trim(df$res_geo_mw, side = c("both"))
df$a_gid <- str_trim(df$a_gid, side = c("both"))
df$nm_d_addr <- str_trim(df$nm_d_addr, side = c("both"))
df$x_d_uk <- str_trim(df$x_d_uk, side = c("both"))
df$y_d_uk <- str_trim(df$y_d_uk, side = c("both"))
df$d_gid <- str_trim(df$d_gid, side = c("both"))

prepro_res_geo <- read.csv('C:\\19project\\07.UCK\\01.Data\\04.Setting_data\\02.DB_Pub_Counter\\01.Preprocessing\\db_res_geo_nm.csv', header = F, stringsAsFactors = FALSE)

df$res_geo_mw[is.na(df$res_geo_mw)] <- prepro_res_geo[, 1][1]

df <- df[!df$res_geo_mw == prepro_res_geo[, 1][2],]
df <- df[!df$res_geo_mw == prepro_res_geo[, 1][3],]
df <- df[!df$res_geo_mw == prepro_res_geo[, 1][4],]
df <- df[!df$res_geo_mw == prepro_res_geo[, 1][5],]
df <- df[!df$res_geo_mw == prepro_res_geo[, 1][6],]
df <- df[!df$res_geo_mw == prepro_res_geo[, 1][7],]

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

prepro_h_ran <- read.csv('C:\\19project\\07.UCK\\01.Data\\04.Setting_data\\02.DB_Pub_Counter\\01.Preprocessing\\h_ran_nm.csv', header = FALSE, stringsAsFactors = FALSE)

df$h_ran <- ifelse(df$h_reg_mw >= '09' & df$h_reg_mw < '18', prepro_h_ran[, 1][1],
                   ifelse(df$h_reg_mw >= '18' & df$h_reg_mw <= '23', prepro_h_ran[, 1][2],
                          ifelse(df$h_reg_mw >= '00' & df$h_reg_mw < '05', prepro_h_ran[, 1][3], prepro_h_ran[, 1][4]
                          )))

prepro_age <- read.csv('C:\\19project\\07.UCK\\01.Data\\04.Setting_data\\02.DB_Pub_Counter\\01.Preprocessing\\age_ran_nm.csv', header = FALSE, stringsAsFactors = FALSE)

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

prepro_day <- read.csv('C:\\19project\\07.UCK\\01.Data\\04.Setting_data\\02.DB_Pub_Counter\\01.Preprocessing\\jj_day_nm.csv', header = FALSE, stringsAsFactors = FALSE)

df$jj <- ifelse((df$d_reg_mw == prepro_day[, 1][1])|(df$d_reg_mw == prepro_day[, 1][2])|(df$d_reg_mw == prepro_day[, 1][3])|(df$d_reg_mw == prepro_day[, 1][4])|(df$d_reg_mw == prepro_day[, 1][5])
                , prepro_day[, 1][8], prepro_day[, 1][9])  

df$count <- 1

prepro_io <- read.csv('C:\\19project\\07.UCK\\01.Data\\04.Setting_data\\02.DB_Pub_Counter\\01.Preprocessing\\in_out_nm.csv', header = FALSE, stringsAsFactors = FALSE)

df[df$add_appl == ""] <- NA

df$in_out <- ifelse(df$a_gid == "", prepro_io[, 1][1], prepro_io[, 1][2])
df$in_out <- ifelse(is.na(df$add_apl == TRUE), prepro_io[, 1][3], df$in_out)

write.csv(df, 'C:\\19project\\07.UCK\\01.Data\\03.Data_Preprocessing\\02.DB_Pub_Counter\\DB_Pub_Counter_Data_Preprocessing.csv', row.names = FALSE,
          fileEncoding = 'CP949')

df1 <- read.csv('C:\\19project\\07.UCK\\01.Data\\03.Data_Preprocessing\\02.DB_Pub_Counter\\DB_Pub_Counter_Data_Preprocessing.csv', stringsAsFactors = FALSE)

daily_issue <- aggregate(count ~ nm_d_c + nm_svr_id + dt_mw, data = df1, sum)

write.csv(daily_issue, 'C:\\19project\\07.UCK\\01.Data\\03.Data_Preprocessing\\02.DB_Pub_Counter\\DB_Pub_Counter_Daily_Issue.csv', row.names = FALSE)
