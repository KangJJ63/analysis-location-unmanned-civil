
Sys.setenv(JAVA_HOME  =  'C:\\Program Files\\Java\\jre1.8.0_241')

library(stringr)

kiosk_data <- NULL


setwd('C:\\19project\\07.UCK\\01.Data\\01.Raw_data\\01.Kiosk')

for(i in 1:length(dir())){
  data <- read.csv(dir()[i], stringsAsFactors = FALSE)
  kiosk_data <- rbind(kiosk_data, data)
}
rm(data);rm(i);


kiosk_data$uni_id <- str_trim(kiosk_data$uni_id, side = c("both"))
kiosk_data$num_mw <- str_trim(kiosk_data$num_mw, side = c("both"))
kiosk_data$nm_k_c <- str_trim(kiosk_data$nm_k_c, side = c("both"))
kiosk_data$bir_app_mw <- str_trim(kiosk_data$bir_app_mw, side = c("both"))
kiosk_data$num_min_mw <- str_trim(kiosk_data$num_min_mw, side = c("both"))
kiosk_data$nm_min_mw <- str_trim(kiosk_data$nm_min_mw, side = c("both"))
kiosk_data$dat_reg_mw <- str_trim(kiosk_data$dat_reg_mw, side = c("both"))
kiosk_data$tim_reg_mw <- str_trim(kiosk_data$tim_reg_mw, side = c("both"))
kiosk_data$d_reg_mw <- str_trim(kiosk_data$d_reg_mw, side = c("both"))
kiosk_data$apply_addr <- str_trim(kiosk_data$apply_addr, side = c("both"))
kiosk_data$x_app_uk <- str_trim(kiosk_data$x_app_uk, side = c("both"))
kiosk_data$y_app_uk <- str_trim(kiosk_data$y_app_uk, side = c("both"))
kiosk_data$res_geo_mw <- str_trim(kiosk_data$res_geo_mw, side = c("both"))

write.csv(kiosk_data, 'C:\\19project\\07.UCK\\01.Data\\02.Use_Data\\Kiosk_Raw.csv',
          row.names = FALSE, fileEncoding = 'CP949')

setwd('C:\\19project\\07.UCK\\01.Data\\01.Raw_data\\02.Kiosk_List')

kiosk_list <- NULL

for(i in 1:length(dir())){
  data <- read.csv(dir()[i], stringsAsFactors = FALSE)
  kiosk_list <- rbind(kiosk_list, data)
}
rm(data);rm(i);

kiosk_list$nm_k_c <- str_trim(kiosk_list$nm_k_c, side = c("both"))
kiosk_list$nm_k_cf <- str_trim(kiosk_list$nm_k_cf, side = c("both"))
kiosk_list$k_char <- str_trim(kiosk_list$k_char, side = c("both"))
kiosk_list$addr_k <- str_trim(kiosk_list$addr_k, side = c("both"))
kiosk_list$x_k_uk <- str_trim(kiosk_list$x_k_uk, side = c("both"))
kiosk_list$y_k_uk <- str_trim(kiosk_list$y_k_uk, side = c("both"))

write.csv(kiosk_list, 'C:\\19project\\07.UCK\\01.Data\\02.Use_Data\\Kiosk_List.csv', row.names = FALSE, fileEncoding = 'CP949')