# Date_cr: 2019-12-01
# Date_up: 2019-12-01
# Project: 2019 UCK
# Code Nm: load_svc_wkdy_time.R 
# Coder N: pkim 

# Remark : 서비스인구데이터  
#          월별 요일별 시간대별로 정리

# 00. Packs and Setting 
# 01. Load file, aggregate with STD_YM, INDEX_ID and save (2) files 
# 0101. 서비스 인구 colnames
# 0102. 빈 pop_STD_YM_INDEX_ID_TIME_WKDY 파일 준비
# 0103. for(load, aggregate, rbind)
# 0104. save pop_STD_YM_INDEX_ID_TIME_WKDY


# 00. Packs and Setting ----
# Settings ----

# Package Loading ---- 

library(data.table)
library(dplyr)
library(lubridate)
library(stringr)

#숫자 자릿수 옵션(지수형 표시 없앰)
options(scipen=10000)
# system messages in English 
Sys.setenv(LANG = "en_US.UTF-8")

# Working Directory
work_path <- "C:\\19project\\07.UCK"
setwd(work_path)

# svc load file path (SERVICE TIME WKDY POP)
svc_load_file_path <- "\\01.Data\\01.Raw_data\\07.Service_Pop\\"

# save file path 
save_file_path <- "\\01.Data\\03.Data_Preprocessing\\05.Service_Pop\\"

# output 
# 0104. # pop_STD_YM_INDEX_ID_TIME_WKDY_RData (save_file_path)
pop_STD_YM_INDEX_ID_TIME_WKDY_RData <- "pop_STD_YM_INDEX_ID_TIME_WKDY.RData"

# 0203. pop_YR_INDEX_ID_RData (save_file_path)
pop_YR_INDEX_ID_RData <- "pop_YR_INDEX_ID.RData"

# 0104. # pop_STD_YM_INDEX_ID_TIME_WKDY_csv (save_file_path)
pop_STD_YM_INDEX_ID_TIME_WKDY_CSV <- "pop_STD_YM_INDEX_ID_TIME_WKDY.csv"

# 0203. pop_YR_INDEX_ID_RData (save_file_path)
pop_YR_INDEX_ID_CSV <- "pop_YR_INDEX_ID.csv"

# 01. Load file, aggregate with STD_YM, INDEX_ID and save (2) files ---- 
# 0101. 서비스 인구 colnames ----

#col_srv_time_wkdy_pop <- c("STD_YM", "INDEX_ID", "HCODE", "WKDY_CD", "TIME", "H_POP", "W_POP", "V_POP")
col_srv_time_wkdy_pop = c('STD_YM','INDEX_ID','WKDY_CD','TIME','H_POP','W_POP','V_POP')

# 0102. 빈 pop_STD_YM_INDEX_ID_TIME_WKDY 파일 준비----  
pop_STD_YM_INDEX_ID_TIME_WKDY <- tibble()

# 0103. for(load, aggregate, rbind) ----

# 서비스인구 파일 위치 세팅 (해당 위치의 폴더 내 파일을 순차적으로 불러올 예정)
pp <- paste0(work_path, svc_load_file_path)

# 서비스인구 파일들 하나씩 불러서 읽고, 처리하여 저장 
for (i  in 1:length(dir(path = pp)) )   {
  
  # load and preprocess (이상한 첫줄 지우고, 컬네임 교체 후, 숫자로 형식 변환) 

  #loaded_temp <- read.table(paste0(pp, dir(path = pp)[i]), sep="|", encoding = "UTF-8" ,stringsAsFactors = F )
  #loaded_temp <- loaded_temp[-1,]
  #colnames(loaded_temp) <- col_srv_time_wkdy_pop
  
  loaded_temp <- read.table(paste0(pp, dir(path = pp)[i]), sep="|", encoding = "cp949" ,stringsAsFactors = F ,header=FALSE,skip = 1)
  colnames(loaded_temp) <- col_srv_time_wkdy_pop
  
  loaded_temp$H_POP <- as.numeric(loaded_temp$H_POP)
  loaded_temp$W_POP <- as.numeric(loaded_temp$W_POP)
  loaded_temp$V_POP <- as.numeric(loaded_temp$V_POP)    
  
  # CD_WK ( 주말 WE , 주중 WD) 
  loaded_temp$CD_WK <- ""
  loaded_temp$CD_WK <- ifelse(loaded_temp$WKDY_CD %in% c("SAT", "SUN") , "WE", "WD")
  
  # CD_TIME ( 주간 DT - "09",...,"17", 야간 NT - "18",...,"23", 심야 MN - "00",...,"04", 새벽 DW - "05",...,"08")
  loaded_temp$CD_TIME <- ""
  loaded_temp$CD_TIME <- ifelse(loaded_temp$TIME %in% c("09","10","11","12","13","14","15","16","17"),
                                "DT", loaded_temp$CD_TIME)
  loaded_temp$CD_TIME <- ifelse(loaded_temp$TIME %in% c("18", "19", "20", "21", "22", "23"),
                                "NT", loaded_temp$CD_TIME)
  loaded_temp$CD_TIME <- ifelse(loaded_temp$TIME %in% c("00", "01", "02", "03", "04"),
                                "MN", loaded_temp$CD_TIME)
  loaded_temp$CD_TIME <- ifelse(loaded_temp$TIME %in% c("05", "06", "07", "08"),
                                "DW", loaded_temp$CD_TIME)
  
  loaded_temp_wk <- loaded_temp %>% 
    group_by(INDEX_ID, STD_YM, CD_WK, TIME ) %>% 
    summarize(H = sum(H_POP), W = sum(W_POP), V = sum(V_POP))
  
  pop_STD_YM_INDEX_ID_TIME_WKDY <- bind_rows(pop_STD_YM_INDEX_ID_TIME_WKDY, loaded_temp_wk)
  
}

# 확인용 
#head(pop_STD_YM_INDEX_ID_TIME_WKDY[pop_STD_YM_INDEX_ID_TIME_WKDY$STD_YM == "201801",])
#   INDEX_ID     STD_YM CD_WK TIME      H     W     V
#   <chr>        <chr>  <chr> <chr> <dbl> <dbl> <dbl>
# 1 다바30ab98bb 201801 WD    00     3.34 0.47   0.51
# 2 다바30ab98bb 201801 WD    01     3.34 0.15   0.35
# 3 다바30ab98bb 201801 WD    02     3.36 0.140  0.33
# 4 다바30ab98bb 201801 WD    03     3.37 0.140  0.33
# 5 다바30ab98bb 201801 WD    04     3.37 0.140  0.32
# 6 다바30ab98bb 201801 WD    05     3.35 0.15   0.34

#dim(pop_STD_YM_INDEX_ID_TIME_WKDY)  #[1] 5,474,176       7
#length(unique(pop_STD_YM_INDEX_ID_TIME_WKDY$INDEX_ID)) #10,623: Num. of celss with POPs in 2018 

# 0104. save pop_STD_YM_INDEX_ID_TIME_WKDY ---- 
pop_STD_YM_INDEX_ID_TIME_WKDY_file <- 
  paste0(work_path, save_file_path, pop_STD_YM_INDEX_ID_TIME_WKDY_RData)
save(pop_STD_YM_INDEX_ID_TIME_WKDY, file=pop_STD_YM_INDEX_ID_TIME_WKDY_file)

pop_STD_YM_INDEX_ID_TIME_WKDY_CSV_file <- 
  paste0(work_path, save_file_path, pop_STD_YM_INDEX_ID_TIME_WKDY_CSV)
write.csv(pop_STD_YM_INDEX_ID_TIME_WKDY, file= pop_STD_YM_INDEX_ID_TIME_WKDY_CSV_file, 
          row.names = F, fileEncoding = "CP949")




# 02. 주중. 주간시간대. 연도별 서비스인구 합계----
# str(pop_STD_YM_INDEX_ID_TIME_WKDY) 
# 13395152 * 7 

# 0201. filtering (WD, 09 ~ 17) ----
tt <- pop_STD_YM_INDEX_ID_TIME_WKDY[pop_STD_YM_INDEX_ID_TIME_WKDY$CD_WK == "WD",]  #  6704743
tt <- tt[tt$TIME %in% c("09", "10", "11", "12", "13", "14", "15", "16", "17") ,]   #  2512810

# 0202. STD_Y 변수 생성 ----
tt$STD_Y <- substr(tt$STD_YM, 1, 4)
a_tt <- aggregate( cbind(H, W, V) ~ INDEX_ID + STD_Y, tt, sum) #30242 
head(a_tt)

sum(is.na(a_tt))  #0 은 0 으로 표기 되어 있음. 
a_tt[is.na(a_tt)] <- 0 # 나중을 위하여 

pop_YR_INDEX_ID <- a_tt 

# 0203. save pop_YR_INDEX_ID (주중, 주간, 연단위 합계)
pop_YR_INDEX_ID_file <- paste0(work_path, save_file_path, pop_YR_INDEX_ID_RData)
save(pop_YR_INDEX_ID, file = pop_YR_INDEX_ID_file)

pop_YR_INDEX_ID_CSV_file <- paste0(work_path, save_file_path, pop_YR_INDEX_ID_CSV)
write.csv(pop_YR_INDEX_ID, file= pop_YR_INDEX_ID_CSV_file, 
          row.names = F, fileEncoding = "CP949")

rm(pop_STD_YM_INDEX_ID_TIME_WKDY)
rm(tt)
rm(pop_YR_INDEX_ID)

#0204. output 폴더 생성
output_path = 'C:/19Project/07.UCK/03.Output'
sub_output_path = list('01.Kiosk/01.Kiosk_EDA/01.Each_Kiosk_Result',
                       '01.Kiosk/01.Kiosk_EDA/02.Each_Same_Spot_Kiosk_Result',
                       '01.Kiosk/01.Kiosk_EDA/03.Each_Kiosk_1day_Result',
                       '01.Kiosk/01.Kiosk_EDA/04.Each_Same_Spot_Kiosk_1day_Result',
                       '02.DB_Pub_Counter/01.DB_Pub_Counter_EDA/01.Each_DB_Pub_Counter_Result',
                       '02.DB_Pub_Counter/01.DB_Pub_Counter_EDA/02.Each_DB_Pub_Counter_1day_Result',
                       '03.DCB',
                       '04.SSGI',
                       '05.JYD',
                       '06.Visualization',
                       '07.Modeling')

ifelse(dir.exists(output_path),'Folder exists aleady',dir.create(output_path))

for (sub_path in sub_output_path){
  temp = strsplit(sub_path,split = "/")
  m_path = output_path
  for (i in c(1:length(temp[[1]]))){
    m_path = paste(m_path,temp[[1]][i],sep = '/')
    print(m_path)
    ifelse(dir.exists(m_path),'Folder exists aleady',dir.create(m_path))
  }
}
