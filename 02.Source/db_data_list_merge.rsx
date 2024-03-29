Sys.setenv(JAVA_HOME  =  'C:\\Program Files\\Java\\jre1.8.0_241')
# Packages Loading ----------
library(stringr)

setwd('C:\\19project\\07.UCK\\01.Data\\01.Raw_data\\03.DB_Pub_Counter')

db_cg_data <- NULL

for(i in 1:length(dir())){
  data <- read.csv(dir()[i], stringsAsFactors = FALSE)
  db_cg_data <- rbind(db_cg_data, data)
}
rm(data);rm(i);

db_cg_data$uni_id <- str_trim(db_cg_data$uni_id, side = c("both"))
db_cg_data$nm_d_c <- str_trim(db_cg_data$nm_d_c, side = c("both"))
db_cg_data$dt_mw <- str_trim(db_cg_data$dt_mw, side = c("both"))
db_cg_data$hr_mw <- str_trim(db_cg_data$hr_mw, side = c("both"))
db_cg_data$cd_dngcho <- str_trim(db_cg_data$cd_dngcho, side = c("both"))
db_cg_data$cd_issue <- str_trim(db_cg_data$cd_issue, side = c("both"))
db_cg_data$cd_koreng <- str_trim(db_cg_data$cd_koreng, side = c("both"))
db_cg_data$cd_prvpub <- str_trim(db_cg_data$cd_prvpub, side = c("both"))
db_cg_data$cd_reciev <- str_trim(db_cg_data$cd_reciev, side = c("both"))
db_cg_data$sex_apl <- str_trim(db_cg_data$sex_apl, side = c("both"))
db_cg_data$age_apl <- str_trim(db_cg_data$age_apl, side = c("both"))
db_cg_data$nm_svr <- str_trim(db_cg_data$nm_svr, side = c("both"))
db_cg_data$nm_svr_id <- str_trim(db_cg_data$nm_svr_id, side = c("both"))
db_cg_data$add_apl <- str_trim(db_cg_data$add_apl, side = c("both"))
db_cg_data$x_app_uk <- str_trim(db_cg_data$x_app_uk, side = c("both"))
db_cg_data$y_app_uk <- str_trim(db_cg_data$y_app_uk, side = c("both"))
db_cg_data$res_geo_mw <- str_trim(db_cg_data$res_geo_mw, side = c("both"))

write.csv(db_cg_data, 'C:\\19project\\07.UCK\\01.Data\\02.Use_Data\\DB_Pub_Counter_Raw.csv',
          row.names = FALSE, fileEncoding = 'CP949')

setwd('C:\\19project\\07.UCK\\01.Data\\01.Raw_data\\04.DB_Pub_Counter_list')

db_cg_list_data <- NULL

for(i in 1:length(dir())){
  data1 <- read.csv(dir()[i], stringsAsFactors = FALSE)
  db_cg_list_data <- rbind(db_cg_list_data, data1)
}
rm(data1);rm(i);

db_cg_list_data$nm_d_c <- str_trim(db_cg_list_data$nm_d_c, side = c("both"))
db_cg_list_data$nm_d_addr <- str_trim(db_cg_list_data$nm_d_addr, side = c("both"))
db_cg_list_data$x_d_uk <- str_trim(db_cg_list_data$x_d_uk, side = c("both"))
db_cg_list_data$y_d_uk <- str_trim(db_cg_list_data$y_d_uk, side = c("both"))

write.csv(db_cg_list_data, 'C:\\19project\\07.UCK\\01.Data\\02.Use_Data\\DB_Pub_Counter_List.csv',
          row.names = FALSE, fileEncoding = 'CP949')
