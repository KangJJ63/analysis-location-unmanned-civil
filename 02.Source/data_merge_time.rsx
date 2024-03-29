
# 00. packs and setting ----

# settings 
select_yr <- read.csv('C:\\19project\\07.UCK\\01.Data\\04.Setting_data\\06.Modeling\\select_year.csv', header = FALSE, stringsAsFactors = FALSE)

yr_anal <- select_yr[, 2][1]

# Packages Loading 
library(corrplot)
library(data.table)
library(dplyr)
library(stringr)
library(imputeTS)
library(dummies)
library(ggplot2)

#숫자 자릿수 옵션(지수형 표시 없앰)
options(scipen=10000)
#ggplot2 option
theme_update(plot.title = element_text(hjust = 0.5))

# 필요 한글 
# "기타" 발급기 중 기타 제거 해야 함
mod <- read.csv('C:\\19project\\07.UCK\\01.Data\\04.Setting_data\\06.Modeling\\modeling_kr_nm.csv', header = FALSE, stringsAsFactors = FALSE)

kiosk_etc <- mod[, 1][1]
# "정좌표" 민원 중 정좌표 민원만 사용
good_xy <- mod[, 1][7] 
# 주중 추출 
wkdays <- c(mod[, 1][2], mod[, 1][3], mod[, 1][4], mod[, 1][5], mod[, 1][6])

#-------------------------------------------
# 01. mw(MooInMinWon) File Preparation----

#0101 Load mw File 
mw <- read.csv('C:\\19project\\07.UCK\\01.Data\\03.Data_Preprocessing\\01.Kiosk\\Kiosk_Data_Preprocessing.csv', stringsAsFactors = FALSE)

# 분석 연도 yr_anal 을 처리 하는 루틴 start ----
mw$dat_reg_mw <- lubridate::ymd(mw$dat_reg_mw)
# 발급기별 nm_k_cf 최소 날짜,  최대 날짜 
k_dat_min <- aggregate( dat_reg_mw ~ nm_k_cf, mw, min)
k_dat_max <- aggregate( dat_reg_mw ~ nm_k_cf, mw, max)
k_dat <- left_join(k_dat_min, k_dat_max, by = "nm_k_cf")
# 해당 년에서 최소 1-10 이전부터 12-21 이후 까지는 있는 nm_k_cf 를 찾는다. 
# 기준 날짜 생성 
#start_dt_yr_anal <- lubridate::ymd(paste0(as.character(yr_anal), "-01-10"))
#end_dt_yr_anal <- lubridate::ymd(paste0(as.character(yr_anal), "-12-21"))
#k_dat <- k_dat[k_dat$dat_reg_mw.x <= start_dt_yr_anal & k_dat$dat_reg_mw.y >= end_dt_yr_anal,]
kiosk_yr_anal <- k_dat[,c("nm_k_cf")]

# mw$nm_k_cf 가 해당 kiosk_yr_anal 에 있는 것만 추출
mw <- mw[mw$nm_k_cf %in% kiosk_yr_anal, ] #421226 --> 418833 
# yr_anal 에 민원발급이 1년을 채우는 nm_k_cf 찾기 - end 
mw <- mw[(mw$y_reg_mw == yr_anal),]
table(mw$y_reg_mw)
#2018
#381145
# 분석 연도 yr_anal 을 처리 하는 루틴 end ----

#as.data.frame(prop.table(table(mw_2018$res_geo_mw, useNA = "ifany"  )))
#as.data.frame(table(mw_2018$res_geo_mw, useNA = "ifany")) 37만건만 정좌표

mw <- mw[mw$res_geo_mw == good_xy, ] #370601 obs * 50 vars 

# 시간대 정보를 character 로 처리: h_reg_mw 을 2 자리 character로 
mw$h_reg_mw <- str_pad(mw$h_reg_mw, 2, side="left", pad="0")

# 연도 정보를 character 로 처리: ym_reg_mw 을 character로
mw$ym_reg_mw <- as.character(mw$ym_reg_mw)

# 사용할 필요 컬럼만 가져오기 + 민원종별 3종 가져오기 
# : id, nm_k_cf, k_gid(다사39bb11ab), y_m_reg_mw(201801), h_reg_mw, d_reg_mw, h_ran, in_out 
# : 3종(c_m_mw==1)과 필요컬럼 선택 
mw_select <- mw[mw$certi_mag == 1, c("uni_id", "nm_k_cf", "k_char", "k_gid", "d_reg_mw",
                                  "ym_reg_mw", "h_reg_mw", "dist_g", "in_out", "h_ran")]

# 무인민원발급기 특성 "기타"(kiosk_etc)제외 ("기타"는 일반시민이 접근할 수 없는 곳에 있는 사업장용임)
# : k_char '기타' 제외 #312730 (5480 축소)  318213-312730 3개 사업장 발급기 
mw_select_2 <- mw_select[mw_select$k_char != kiosk_etc,]

rm(mw, mw_select)

# 무인발급기 이용 주중(월~금) 발급한 것만 사용 
# : d_reg_mw 이 월~금 (주중)
mw_WD_by_kgid <- mw_select_2 %>% 
  filter(d_reg_mw %in% wkdays) %>% 
  group_by(k_gid, ym_reg_mw, h_reg_mw) %>% 
  summarize(cnt_mw3 = n())

#sum(mw_WD_by_kgid$cnt_mw3) #화성 2018년 283,964

rm(mw_select_2)

# 02. Data Set Frame (frm) Setting ----

# 0201. Train/Test 용 Data Set Frame 생성 : k_gid 있는 격자에 대해서만 생성 ---- 
# frm_wd_dt - day time frame (09~17) Weekday, DayTime : k_gid 기준
# k_gid 는 mw_WD_by_kgid 의 k_gid 를 사용한다. 
# 시간대(9:17), STD_YM(1:12), WK=c("WD"(주중))
# 시간대 x 연월 x 주간(1) x k_gid 를 이용하여 생성. 

frm_wd_dt <- expand.grid(TIME=c(09:17), STD_YM=c(1:12), WK=c("WD"), k_gid=unique(mw_WD_by_kgid$k_gid))
#str(frm_wd_dt)
frm_wd_dt$TIME <- str_pad(frm_wd_dt$TIME, 2, side="left", pad="0")  # 시간대를 두 자리로 
frm_wd_dt$STD_YM <- paste(yr_anal, str_pad(frm_wd_dt$STD_YM, 2,
                                          side = "left", pad="0"), sep = "") # 월(STD_YM)을 2018_** 로 
#순서 정리 
frm_wd_dt <- frm_wd_dt[,c("k_gid", "STD_YM", "WK", "TIME")]

# 0202 Prediction 용 Data Set Frame 생성 : full frame 즉, 전체 gid 기준  ----
# 시간대(9:17), STD_YM(1:12), WK=c("WD"(주중)) 전체 gid 기준 
# 주용도의 11895 격자를 쓰자. 

gg <- read.csv('C:\\19project\\07.UCK\\01.Data\\02.Use_Data\\GID_ID.csv', stringsAsFactors = FALSE)
gids <- unique(gg$gid)

frm <- expand.grid(TIME=c(09:17), STD_YM=c(1:12), WK=c("WD"), k_gid=gids)
frm$TIME <- str_pad(frm$TIME, 2, side="left", pad="0")
frm$STD_YM <- paste(yr_anal, str_pad(frm$STD_YM, 2, side = "left", pad="0"), sep = "")
frm <- frm[,c("k_gid", "STD_YM", "WK", "TIME")]

# 03. frm + mw ----
# 0301 f_mw_WD_by_kgid: frm_wd_dt + mw_WD_by_kgid

f_mw_WD_by_kgid <- left_join(frm_wd_dt, mw_WD_by_kgid, by=c("k_gid", "STD_YM"="ym_reg_mw", "TIME"="h_reg_mw"))
#head(f_mw_WD_by_kgid, 25) #4104 * 5 
f_mw_WD_by_kgid$cnt_mw3 <- na_replace(f_mw_WD_by_kgid$cnt_mw3, 0)
sum(f_mw_WD_by_kgid$cnt_mw3) #267066 주중, 9시~18시(09-17) 까지 건수 
rm(frm_wd_dt, mw_WD_by_kgid)

# 04. frm + pop_service ----
#0401 f_mw_WD_by_kgid_pop
#pop_STD_YM_INDEX_ID_TIME_WKDY_2018 

#040101 Load pop_STD_YM_INDEX_ID_TIME_WKDY.RData 
load('C:\\19project\\07.UCK\\01.Data\\03.Data_Preprocessing\\05.Service_Pop\\pop_STD_YM_INDEX_ID_TIME_WKDY.RData')  #5474176 * 7 : pop_STD_YM_INDEX_ID_TIME_WKDY
pop_STD_YM_INDEX_ID_TIME_WKDY$STD_YM = as.character(pop_STD_YM_INDEX_ID_TIME_WKDY$STD_YM)
pop_STD_YM_INDEX_ID_TIME_WKDY$TIME = as.character(pop_STD_YM_INDEX_ID_TIME_WKDY$TIME)
#040102 Join f_mw + pop 
f_mw_WD_by_kgid_pop <- left_join(f_mw_WD_by_kgid, pop_STD_YM_INDEX_ID_TIME_WKDY, 
                                 by=c("k_gid"="INDEX_ID", "WK"="CD_WK", "STD_YM", "TIME"))
#no NAs check
#sum(is.na(f_mw_WD_by_kgid_pop$H))
#sum(is.na(f_mw_WD_by_kgid_pop$W))
#sum(is.na(f_mw_WD_by_kgid_pop$V))

#나중엘 위해 NA 는 0 으로 
f_mw_WD_by_kgid_pop$H <- na_replace(f_mw_WD_by_kgid_pop$H, 0)
f_mw_WD_by_kgid_pop$W <- na_replace(f_mw_WD_by_kgid_pop$W, 0)
f_mw_WD_by_kgid_pop$V <- na_replace(f_mw_WD_by_kgid_pop$V, 0)

rm(f_mw_WD_by_kgid)

#0402 Join f_full + pop 
f_pop <- left_join(frm, pop_STD_YM_INDEX_ID_TIME_WKDY, 
                   by=c("k_gid"="INDEX_ID", "WK"="CD_WK", "STD_YM", "TIME"))
#sum(is.na(f_pop$H)) #119803
#sum(is.na(f_pop$W))
#sum(is.na(f_pop$V))
f_pop$H <- na_replace(f_pop$H, 0)
f_pop$W <- na_replace(f_pop$W, 0)
f_pop$V <- na_replace(f_pop$V, 0)

rm(frm, pop_STD_YM_INDEX_ID_TIME_WKDY)

#05. frm + JYD_800m 주용도코드 800m 인근----
#0501 Load jyd cd 
load('C:\\19project\\07.UCK\\01.Data\\05.Modeling\\jyd_cd_800m_d.RData') #11895

#head(jyd_cd_800m_d) 
#          gid JCD_01 JCD_02 JCD_03 JCD_04 JCD_05 JCD_06 ... JCD_25 JCD_26 JCD_27
# 다바14ba99bb      0      0      0      0      0      0 ...      0      0      0

#0502 Join f + jyd 
f_mw_WD_by_kgid_pop_JYD <- left_join(f_mw_WD_by_kgid_pop, jyd_cd_800m_d, by=c("k_gid"="gid"))
rm(f_mw_WD_by_kgid_pop)

#0503 Join f full + jyd 
f_pop_JYD <- left_join(f_pop, jyd_cd_800m_d, by=c("k_gid"="gid"))
f_pop_JYD[is.na(f_pop_JYD)] <- 0
rm(f_pop, jyd_cd_800m_d)


# 06. bus station 버스정류소 380m 내 갯수----
#0601 L jyd_pop_..._bus table 
bus <- read.csv('C:\\19project\\07.UCK\\01.Data\\03.Data_Preprocessing\\06.Bus_Stop\\Bus_Stop_380m_800m_Buff_Count.csv', stringsAsFactors = FALSE)
bus <- bus[,c("gid", "BS_380")]

#0602 join f + bus 
f_mw_WD_by_kgid_pop_JYD <- left_join(f_mw_WD_by_kgid_pop_JYD, bus, by=c("k_gid"="gid"))

#0603 join f full + bus 
f_pop_JYD <- left_join(f_pop_JYD, bus, by=c("k_gid"="gid"))

rm(bus)

#07. frm + ssgi (L, M )----
#0701 격자별 ssgi L 대분류
ssgi_L <- read.csv('C:\\19project\\07.UCK\\01.Data\\03.Data_Preprocessing\\04.SSGI\\SSGI_Select_Area_LMS_Buff_380M\\SSGI_L_Buff_380M.csv', stringsAsFactors = FALSE)
ssgi_L[is.na(ssgi_L)] <- 0

sum_len <- length(colnames(ssgi_L))
col_len <- (sum_len - 1) / 2 + 1

ssgi_L$n_biz <- apply(ssgi_L[,2:col_len],1,sum)

ssgi_L$n_biz_sum <- apply(ssgi_L[,(col_len+1):sum_len],1,sum) 

#ssgi_L$n_biz <- ssgi_L$D + ssgi_L$F + ssgi_L$L + ssgi_L$N + 
#  ssgi_L$O + ssgi_L$P + ssgi_L$Q + ssgi_L$R + ssgi_L$S
#ssgi_L$n_biz_sum <- ssgi_L$D_sum + ssgi_L$F_sum + ssgi_L$L_sum + ssgi_L$N_sum + 
#  ssgi_L$O_sum + ssgi_L$P_sum + ssgi_L$Q_sum + ssgi_L$R_sum + ssgi_L$S_sum 

# 0701_best 
#Load best_L 
load('C:\\19project\\07.UCK\\01.Data\\03.Data_Preprocessing\\04.SSGI\\ssgi_best_L.RData')
#Select best_L vars 
ssgi_L <- dplyr::select(ssgi_L, c("gid", best_L))

# 0702 격자별 ssgi M 중분류 
ssgi_M <- read.csv('C:\\19project\\07.UCK\\01.Data\\03.Data_Preprocessing\\04.SSGI\\SSGI_Select_Area_LMS_Buff_380M\\SSGI_M_Buff_380M.csv', stringsAsFactors = FALSE)
ssgi_M[is.na(ssgi_M)] <- 0

# 0702_best 
#Load best_M
load('C:\\19project\\07.UCK\\01.Data\\03.Data_Preprocessing\\04.SSGI\\ssgi_best_M.RData')
#Select best_M vars 
ssgi_M <- dplyr::select(ssgi_M, c("gid", best_M))


#소상공 소분류 없이 맞추어 보자. 
#0703 격자별 ssgi S 소분류
#ssgi_S_load_file <- paste(work_path, ssgi_S_380m_buf_table, sep = "")
#ssgi_S <- fread(ssgi_S_load_file, encoding = "EUC-KR", sep = ",", stringsAsFactors = F)
#ssgi_S[is.na(ssgi_S)] <- 0

ssgi_L_M <- left_join(ssgi_L, ssgi_M, by="gid")
rm(ssgi_L, ssgi_M)

#ssgi_L_M_S <- left_join(ssgi_L_M, ssgi_S, by="gid")
#rm(ssgi_L_M, ssgi_S)
#ssgi <- ssgi_L_M_S
#rm(ssgi_L_M_S)


#0704 join f + ssgi 
f_mw_WD_by_kgid_pop_JYD_ssgi <- left_join(f_mw_WD_by_kgid_pop_JYD, ssgi_L_M, by=c("k_gid"="gid"))
rm(f_mw_WD_by_kgid_pop_JYD)

#0705 join f full + ssgi 
f_pop_JYD_ssgi <- left_join(f_pop_JYD, ssgi_L_M, by=c("k_gid"="gid"))
rm(f_pop_JYD)

rm(ssgi_L_M)


# 08. frm + additive informatin (건축물대장 외) ----
# additive_file <- paste(work_path, load_file_path, additive_table, sep = "")
# additive_info <- fread(file = additive_file, encoding = "EUC-KR")
# head(additive_info)
# names(additive_info) <- c("gid", "c_bld", "c_bld_380", "c_prk", "c_prk_380", "a_bld", "a_bld_380", 
#                           "r_rd", "r_rd_380", "dist_kiosk", "dist_mng")
# additive_info <- na_replace(additive_info, 0)
#  필요한 컬럼만 갖고 오기 위하여 
# summary_additive_info <- as.data.frame(do.call(rbind, lapply(additive_info, summary)))
# 
#  추가 정보의 상관관계 
# tt <- left_join(f_mw_WD_by_kgid_pop_JYD_ssgi, additive_info, by=c("k_gid" = "gid") )
# tt <- tt[,c(5,(ncol(tt)-9):ncol(tt))]
# head(tt)
# ttt<-cor(tt)
# corrplot(ttt)
# 
#  추가 정보의 변수 선택 
# additive_info <- additive_info %>% dplyr::select(gid, c_bld, c_prk, a_bld, r_rd_380)
# 
# 08-01. mw + additive info 
# f_mw_WD_by_kgid_pop_JYD_ssgi_a <- left_join(f_mw_WD_by_kgid_pop_JYD_ssgi, additive_info, by = c("k_gid"="gid"))
# 
# 08-02. f + additive info 
# f_pop_JYD_ssgi_a <- left_join(f_pop_JYD_ssgi, additive_info, by = c("k_gid"="gid"))

#10. STD_YM , TIME dummy 처리 ---- 
#1001 dummy (STD_YM) ----
#100101 f_mw 
dummy_STD_YM <- dummy(f_mw_WD_by_kgid_pop_JYD_ssgi$STD_YM, sep = "_")
f_mw_WD_by_kgid_pop_JYD_ssgi_STDYM <- cbind(f_mw_WD_by_kgid_pop_JYD_ssgi, dummy_STD_YM)
#head(f_mw_WD_by_kgid_pop_JYD_ssgi_a_STDYM, 1)
rm(dummy_STD_YM, f_mw_WD_by_kgid_pop_JYD_ssgi)

#100102 f full 
dummy_STD_YM2 <- dummy(f_pop_JYD_ssgi$STD_YM, sep = "_")
f_pop_JYD_ssgi_STDYM <- cbind(f_pop_JYD_ssgi, dummy_STD_YM2)
#head(f_pop_JYD_ssgi_a_STDYM, 1)
rm(dummy_STD_YM2, f_pop_JYD_ssgi)

#1002 dummy (TIME) ----
#100201 f_mw 
dummy_TIME <- dummy(f_mw_WD_by_kgid_pop_JYD_ssgi_STDYM$TIME, sep = "_")
f_mw_WD_by_kgid_pop_JYD_ssgi_STDYM_TIME <- cbind(f_mw_WD_by_kgid_pop_JYD_ssgi_STDYM, dummy_TIME)
rm(dummy_TIME, f_mw_WD_by_kgid_pop_JYD_ssgi_STDYM)

#100202 f full  
dummy_TIME2 <- dummy(f_pop_JYD_ssgi_STDYM$TIME, sep = "_")
f_pop_JYD_ssgi_STDYM_TIME <- cbind(f_pop_JYD_ssgi_STDYM, dummy_TIME2)
rm(dummy_TIME2, f_pop_JYD_ssgi_STDYM)

# 11. Save Output File ----

# 1101 f save 
f_mw_WD_DT <- f_mw_WD_by_kgid_pop_JYD_ssgi_STDYM_TIME
rm(f_mw_WD_by_kgid_pop_JYD_ssgi_STDYM_TIME)

# 변수 변화 없는 것 확인 
a <- data.frame(max =apply(Filter(is.numeric, f_mw_WD_DT), 2, max), min=apply(Filter(is.numeric, f_mw_WD_DT), 2, min))
b <- a[(a$max - a$min) == 0,]
exclude_vars <- row.names(b)
exclude_vars

#f_mw_WD_DT <- select(f_mw_WD_DT, -exclude_vars)
f_mw_WD_DT <- f_mw_WD_DT[, !(colnames(f_mw_WD_DT) %in% exclude_vars)]
f_mw_WD_DT[is.na(f_mw_WD_DT)] <- 0
#변화 없는 것 제외하고 저장
#write.table(f_mw_WD_DT, file="f_mw_WD_DT.csv", sep = ",", fileEncoding = "EUC-KR", row.names = F)
save(f_mw_WD_DT, file='C:\\19project\\07.UCK\\01.Data\\05.Modeling\\f_mw_WD_DT.RData')

#head(f_mw_WD_DT,1)

#1102 f full save 
f_WD_DT <- f_pop_JYD_ssgi_STDYM_TIME
f_WD_DT[is.na(f_WD_DT)] <- 0
rm(f_pop_JYD_ssgi_STDYM_TIME)

#변화 관계 없이 저장 
#1102 

save(f_WD_DT, file ='C:\\19project\\07.UCK\\01.Data\\05.Modeling\\f_WD_DT.RData')