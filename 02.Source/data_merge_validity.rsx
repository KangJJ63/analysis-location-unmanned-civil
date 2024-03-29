# Date_cr: 2019-12-01
# Date_up: 2019-12-01
# Project: 2019 UCK
# Code Nm: data_merge_validity.R
# Coder N: pkim 
# REMARK : 설치 타당성 모델 개발을 위한 데이터 생성 루틴임
#          표준화 프로그램 

# 00. Packs and Setting
# 01. 주용도 코드( 800m )
# 02. 인구(census), 버스정류소 : jyd_pop_cen_servi_bus 
# 03. pop_YR_INDEX_ID(서비스인구)
# 04. 소상공인 ssgi
# 0401. 격자별 ssgi L 대분류
# 040101. ssgi_L 의 more 변수 가져오기
# 0404. + ssgi_L 
# 05. kiosk 불러서 YN_kiosk 처리 등
# 0501. kiosk 로드. 
# 0502. k_char '기타' 제거 
# 0503. YN_kiosk 에 1 처리 
# 09. jyd_cen_bus_svc_ssgi + kiosk 정보 
# 0901. 변수 점검 
# 10. save 


# 00. Packs and Setting ----

# Settings 
mod <- read.csv('C:\\19project\\07.UCK\\01.Data\\04.Setting_data\\06.Modeling\\modeling_kr_nm.csv', header = FALSE, stringsAsFactors = FALSE)

select_yr <- read.csv('C:\\19project\\07.UCK\\01.Data\\04.Setting_data\\06.Modeling\\select_year.csv', header = FALSE, stringsAsFactors = FALSE)

yr_anal <- select_yr[, 2][1]

# Packages Loading 
library(data.table)
library(dplyr)

#숫자 자릿수 옵션(지수형 표시 없앰)
options(scipen=10000)
#ggplot2 option
#theme_update(plot.title = element_text(hjust = 0.5))
# system messages in English 
Sys.setenv(LANG = "en_US.UTF-8")

#mw_2018_table <- "data_processing_2018_V0.57.csv" #/DATA/
#matching_table <- "250gid_add_gid_mt_edit_1.csv"
#bus_table <- "hs_jyd_pop_bus_380_800m_rename.csv"

# 01. 주용도 코드( 800m )  ----
# (base_grid 를 이것으로 사용) 
# 주용도 코드 파일의 전체 격자를 이용한다.
# 격자

gid <- read.csv('C:\\19project\\07.UCK\\01.Data\\02.Use_Data\\GID_ID.csv', stringsAsFactors = FALSE)
gid <- data.table(gid[, 1])
names(gid)[1] <- c('gid')

# 02. 인구(census), 버스정류소 : jyd_pop_cen_servi_bus ----
# 버스, 국토지리정보원 인구 파일 2개 가져와서 결합
bs <- read.csv('C:\\19project\\07.UCK\\01.Data\\03.Data_Preprocessing\\06.Bus_Stop\\Bus_Stop_380m_800m_Buff_Count.csv', stringsAsFactors = FALSE)
cens <- read.csv('C:\\19project\\07.UCK\\01.Data\\03.Data_Preprocessing\\07.NGII_Pop\\NGII_Tot_Eld_WAG.csv', stringsAsFactors = FALSE)

bs_cens <- left_join(bs, cens, by = 'gid')


#names(jyd_pop_cen_servi_bus)
#cen_bus <- bs_cens[,c("gid", 
#                     "eld_1810_v", "pro_1810_v", "you_1810_v",
#                     "chi_1810_v", "pri_1810_v", "mid_1810_v", "hig_1810_v",
#                     "c_bus_stat")]
cen_bus <- bs_cens[,c("gid", "BS", "Eld_10", "WAG_10")]

jyd_cen_bus <- left_join(gid, cen_bus, by = "gid")
jyd_cen_bus[is.na(jyd_cen_bus)] <- 0

# 03. pop_YR_INDEX_ID(서비스인구)----
load('C:\\19project\\07.UCK\\01.Data\\03.Data_Preprocessing\\05.Service_Pop\\pop_YR_INDEX_ID.RData') #pop_svc_2018_hs 
#head(pop_YR_INDEX_ID)
svc <- pop_YR_INDEX_ID[pop_YR_INDEX_ID$STD_Y == as.character(yr_anal),]
#head(svc)
svc <- svc[, c("INDEX_ID", "H", "W", "V")]
names(svc) <- c("gid", "H", "W", "V")

jyd_cen_bus_svc <- left_join(jyd_cen_bus, svc, by = "gid")
jyd_cen_bus_svc[is.na(jyd_cen_bus_svc)] <- 0
#head(jyd_cen_bus_svc)


# 04. 소상공인 ssgi ----

# 0401. 격자별 ssgi L 대분류
ssgi_L <- read.csv('C:\\19project\\07.UCK\\01.Data\\03.Data_Preprocessing\\04.SSGI\\SSGI_Select_Area_LMS_Buff_380M\\SSGI_L_Buff_380M.csv',
                   stringsAsFactors = FALSE)

ssgi_L[is.na(ssgi_L)] <- 0

sum_len <- length(colnames(ssgi_L))
col_len <- (sum_len - 1) / 2 + 1

ssgi_L$n_biz <- apply(ssgi_L[,2:col_len],1,sum)

ssgi_L$n_biz_sum <- apply(ssgi_L[,(col_len+1):sum_len],1,sum) 

# 0404. + ssgi_L ----

jyd_cen_bus_svc_ssgi <- left_join(jyd_cen_bus_svc, ssgi_L, by="gid")
jyd_cen_bus_svc_ssgi[is.na(jyd_cen_bus_svc_ssgi) ] <- 0



# 05. kiosk 불러서 YN_kiosk 처리 등 ----
# 0501. kiosk 로드. ----
kiosk <- read.csv('C:\\19project\\07.UCK\\01.Data\\03.Data_Preprocessing\\01.Kiosk\\Reference_Data\\kiosk_list_gid.csv', stringsAsFactors = FALSE) 
# 0502. k_char '기타' 제거 ----
kiosk <- kiosk[kiosk$k_char != mod[, 1][1],] 

# 0503. YN_kiosk 에 1 처리 ----
kiosk$YN_kiosk <- 1
kiosk <- kiosk[,c("k_gid","YN_kiosk")]
colnames(kiosk) <- c("gid", "YN_kiosk")
table(kiosk$YN_kiosk)
kiosk <- unique(kiosk)


# 09. jyd_cen_bus_svc_ssgi + kiosk 정보 ----

jyd_cen_bus_svc_ssgi_k <- left_join(jyd_cen_bus_svc_ssgi, kiosk, by = "gid")

jyd_cen_bus_svc_ssgi_k[is.na(jyd_cen_bus_svc_ssgi_k) ] <- 0


# 0901. 변수 점검 ----

df <- jyd_cen_bus_svc_ssgi_k

a3 <- data.frame(max =apply(Filter(is.numeric, df), 2, max), 
                 min=apply(Filter(is.numeric, df), 2, min))
b3 <- a3[(a3$max - a3$min) == 0,]
exclude_vars3 <- row.names(b3)
include_vars <- setdiff(row.names(a3), exclude_vars3)  #20

#ssgi_hs_L_M 에서, include_vars 만 가져 온다. ----
include_vars2 <- c("gid", include_vars)
#ssgi_hs_S_selected <- ssgi_hs_S[,include_vars2]
#ssgi_hs_S_selected <- ssgi_hs_S[,colnames(ssgi_hs_S) %in% include_vars2]

df <- dplyr::select(df, include_vars2)  #22
#colnames(df)

# 변수 순서 변경 
names_df <- names(df)
names_df_others <- setdiff(names_df, c("YN_kiosk", "gid"))
new_names_df <- c("gid","YN_kiosk", names_df_others)

df <- df[,new_names_df]
df[is.na(df)]<- 0

#table(df$YN_kiosk)
#0:11850, 1:45

# 10. save ----
kiosk_validity <- df

save(kiosk_validity, file = 'C:\\19project\\07.UCK\\01.Data\\05.Modeling\\kiosk_validity.RData')

#write.table(kiosk_validity, file="kiosk_validity.txt", sep = "\t", row.names = F, fileEncoding = "UTF-8")
