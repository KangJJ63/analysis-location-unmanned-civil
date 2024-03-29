# Date_cr: 2019-12-01
# Date_up: 2019-12-01
# Project: 2019 UCK
# Code Nm: ssgi_biz_data.R
# Coder N: pkim 
# Remark : 소상공인 정보 로드 및 검토 데이터 말기

# FLOW 
# 00. Packs and Setting
# 01. 민원화일 처리 
# 0101. Load mw File 

# 02. 격자별 ssgi L 대분류 처리 
# 0201. 소상공인 대분류 요약 
# 0202. CD_NM_SSGI_L.TXT 으로 한글 업종명 붙여 주기 
# 0203. 대분류 격자별 상관계수 
# 020301 민원 건수와 상관계수 구하기  
# 020302 대분류 업종별 카운드 및 상관계수 테이블 작성 및 저장 
# 020303. 소상공인 대분류 필터링
# 0204. 대분류 barplot part 
# 0205. 대분류 summary 저장 
# 020501. summary_ssgi_L_r_more_better 저장 
# 020502. summary_ssgi_L_r_more_better_best 저장 

# 03. 격자별 ssgi M 중분류 처리
# 0301. 소상공인 중분류 요약
# 0302. CD_NM_SSG_M.TXT 으로 한글 업종명 붙여 주기 
# 0303. 중분류 격자별 상관계수
# 030301. 민원건수 이용하여 상관계수 구하기  
# 030302. 중분류 업종별 카운트 및 상관계수 테이블 작성 및 저장 
# 030303. 소상공인 중분류 필터링 
# 0304. 중분류 barplot part 
# 0305. 중분류 summary 저장
# 030501. summary_ssgi_M_r_more_better 저장 
# 030502. summary_ssgi_M_r_more_better_best 저장 

# 04. 격자별 ssgi S 소분류 처리 
# 0401. 소상공인 소분류 요약 
# 0402. CD_NM_SSGI_S.TXT 으로 한글 업종명 붙여주기 
# 0403. 소분류 격자별 상관계수 
# 040301. 민원건수 이용하여 상관계수 만들기 
# 040302. 소분류 업종별 카운트 및 상관계수 테이블 작성 및 저장 
# 040303. 소상공인 소분류 필터링 
# 0404. 소분류 barplot part
# 0405. 소분류 summary 저장 
# 040501. summary_ssgi_S_r_more_better 저장 
# 040502. summary_ssgi_S_r_more_better_best 저장 

# 05. 소상공인 대중소 LMS별 결합 및 저장
# 0501. LMS 전체 결합 및 저장 
# 0502. LMS별 best 전체 결합 및 저장 
# 0503. LMS별 more, more_better, best 변수 저장 

##Output= output string

# 00. Packs and Setting ----

# Packages Loading 
library(corrplot)
library(data.table)
library(dplyr)
library(stringr)
library(imputeTS)
library(dummies)
library(ggplot2)
library(lubridate)

#숫자 자릿수 옵션(지수형 표시 없앰)
options(scipen=10000)
#ggplot2 option
theme_update(plot.title = element_text(hjust = 0.5))

# setting # 민원량과 상관관계 구해야 하므로, 
select_year <- read.csv('C:\\19Project\\07.UCK\\01.Data\\04.Setting_data\\06.Modeling\\select_year.csv', header = FALSE, stringsAsFactors = FALSE)
yr_anal <- select_year[, 2][1] 

# 필요 한글 
# "기타" 발급기 중 기타 제거 해야 함
prenm <- read.csv('C:\\19project\\07.UCK\\01.Data\\04.Setting_data\\04.SSGI\\ssgi_kr_nm.csv', header = FALSE, stringsAsFactors = FALSE)
kiosk_etc <- prenm[, 1][1]
# "정좌표" 민원 중 정좌표 민원만 사용
good_xy <- prenm[, 1][2] 

# Working Directory
work_path <- "C:\\19project\\07.UCK"
setwd(work_path)

# load file path 
load_file_path <- "\\01.Data\\03.Data_Preprocessing\\04.SSGI\\SSGI_Select_Area_LMS_Buff_380M\\"
# mw file path 
mw_file_path <- "\\01.Data\\03.Data_Preprocessing\\01.Kiosk\\"
# save file path 
save_file_path <- "\\01.Data\\05.Modeling\\"
# ssgi file path 
ssgi_file_path <- "\\01.Data\\03.Data_Preprocessing\\04.SSGI\\SSGI_Select_Area_LMS_Buff_380M\\"
# setting_file_path (CD_NM_SSGL_L_table)
setting_file_path <- "\\01.Data\\04.Setting_data\\04.SSGI\\"
# output_file_path (summary 저장) 
output_file_path <- "\\03.Output\\04.SSGI\\"

ssgi_dp_path <- '\\01.Data\\03.Data_Preprocessing\\04.SSGI\\'

#input files ----
#0101. 민원 file name 
mw_table <- "Kiosk_Data_Preprocessing.csv"

# 소상공인 file names 
#0301
ssgi_L_380m_buf_table <- "SSGI_L_Buff_380M.csv"
#0401
ssgi_M_380m_buf_table <- "SSGI_M_Buff_380M.csv"
#0501
ssgi_S_380m_buf_table <- "SSGI_S_Buff_380M.csv"

# 소상공인 코드값 
#0302 
CD_NM_SSGI_L_table <- "CD_NM_SSGI_L.TXT"
#0402 
CD_NM_SSGI_M_table <- "CD_NM_SSGI_M.TXT"
#0502
CD_NM_SSGI_S_table <- "CD_NM_SSGI_S.TXT"

# 소상공인 부천 file names 
#ssgi_bc_gid_file_name <- "/DATA/소상공인/bc/ssgi_bc_gid.csv"
#ssgi_L_bc_380m_buf_file_name <- "/DATA/소상공인/bc/ssgi_L_bc_380m_buf.csv"
#ssgi_M_bc_380m_buf_file_name <- "/DATA/소상공인/bc/ssgi_M_bc_380m_buf.csv"
#ssgi_S_bc_380m_buf_file_name <- "/DATA/소상공인/bc/ssgi_S_bc_380m_buf.csv"

# output files ----
#020502
summary_ssgi_L_r_more_better_best_output <- "summary_ssgi_L_r_more_better_best.txt"
#030502 
summary_ssgi_M_r_more_better_best_output <- "summary_ssgi_M_r_more_better_best.txt"
#040502 
summary_ssgi_S_r_more_better_best_output <- "summary_ssgi_S_r_more_better_best.txt"

more_L_RData <- "ssgi_more_L.RData"
more_better_L_RData <- "ssgi_more_better_L.RData"
best_L_RData <- "ssgi_best_L.RData"
more_M_RData <- "ssgi_more_M.RData"
more_better_M_RData <- "ssgi_more_better_M.RData"
best_M_RData <- "ssgi_best_M.RData"
more_S_RData <- "ssgi_more_S.RData"
more_better_S_RData <- "ssgi_more_better_S.RData"
best_S_RData <- "ssgi_best_S.RData"

# 01. 민원 화일 처리  ----------

# 0101. Load mw File 
mw_file <- paste(work_path, mw_file_path, mw_table, sep="")
mw <- fread(file = mw_file, sep=",", encoding = "unknown", stringsAsFactors = F)
#nrow(mw) #421226 

# yr_anal 에 민원발급이 1년을 채우는 nm_k_cf 찾기 - start 
#head(mw,1)
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

# 격자나 좌표 관련된 데이터는 "정좌표"만 사용. 
mw <- mw[mw$res_geo_mw == good_xy, ] #372903 obs * 50 vars 

#.. 3종(c_m_mw==1), 필요컬럼 #300581
mw_select <- mw[mw$certi_mag == 1, c("uni_id", "nm_k_cf", "k_char", "k_gid", "d_reg_mw",
                                                 "ym_reg_mw", "h_reg_mw", "dist_g", "in_out", "h_ran")]

#.. k_char '기타' 제외 #297460 
mw_select <- mw_select[mw_select$k_char != kiosk_etc,]

# mw_select / gid : 38개 발급기 
mw_select_gid <- aggregate(uni_id ~ k_gid, data = mw_select, length)  #38

# mwk 으로 이름 간단하게 
mwk <- mw_select_gid
rm(mw, mw_select, mw_select_gid)

# 02. 격자별 ssgi L 대분류 처리 ----
# 0201. 소상공인 대분류 요약 ---- 
ssgi_L_file <- paste0(work_path, ssgi_file_path, ssgi_L_380m_buf_table)
ssgi_L <- fread(file = ssgi_L_file, encoding = "unknown", sep = ",", stringsAsFactors = F)

ssgi_L[is.na(ssgi_L)] <- 0

# n_biz, n_biz_sum 작성 
ssgi_L$n_biz <- ssgi_L$D + ssgi_L$F + ssgi_L$L + ssgi_L$N + 
                     ssgi_L$O + ssgi_L$P + ssgi_L$Q + ssgi_L$R + ssgi_L$S
ssgi_L$n_biz_sum <- ssgi_L$D_sum + ssgi_L$F_sum + ssgi_L$L_sum + ssgi_L$N_sum + 
                      ssgi_L$O_sum + ssgi_L$P_sum + ssgi_L$Q_sum + ssgi_L$R_sum + ssgi_L$S_sum 

# ssgi L 별 요약정보(count, sum, summary) 
sum_L <- data.frame(sum_ssgi = do.call(rbind, lapply(ssgi_L[,-1], sum)))  #sum(ssgi_hs_L$)
summary_L <- do.call(rbind, lapply(ssgi_L[,-1], summary)) #summary 
cnt_L <- do.call(rbind, lapply(ssgi_L[,-1], function(x) sum(x>0))) #값이 있는 격자 갯수

# ssgi L 별 요약정보로 summary 테이블 작성
summary_ssgi_L <- cbind(cnt_L, sum_L, summary_L)
summary_ssgi_L$CD_SSGI_L <- rownames(summary_ssgi_L)
summary_ssgi_L$CD_SSGI_L2 <- substr(summary_ssgi_L$CD_SSGI_L, 1,1)

# 0202. CD_NM_SSGI_L.TXT 으로 한글 업종명 붙여 주기 
CD_NM_SSGI_L_file <- paste0(work_path, setting_file_path , CD_NM_SSGI_L_table)
CD_NM_SSGI_L <- fread(file = CD_NM_SSGI_L_file, encoding = "UTF-8")
summary_ssgi_L_nm <- left_join(summary_ssgi_L, CD_NM_SSGI_L, by= c("CD_SSGI_L2" = "CD_SSGI_L"))
summary_ssgi_L_nm <- 
  summary_ssgi_L_nm[,c("CD_SSGI_L", "NM_SSGI_L", "cnt_L", "sum_ssgi","Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.")] 
# 저장 summary_ssgi_L_nm : summary_ssgi_L.txt  
summary_ssgi_L_nm_file <- paste0(work_path, output_file_path, "summary_ssgi_L.txt")
write.table(summary_ssgi_L_nm, file = summary_ssgi_L_nm_file, sep = "\t", fileEncoding = "CP949")


# 0203. 대분류 격자별 상관계수 ----
# 020301 민원 건수와 상관계수 구하기  
# 민원건수와 소상공인 대분류 파트 결합 후 data frame 으로 만들기 
mwk_ssgi_L <- left_join(mwk, ssgi_L, by=c("k_gid"="gid"))
cor_L <- cor(Filter(is.numeric, mwk_ssgi_L))
cor_L <- cor_L[,1]
cor_L <- cor_L[-1]
cor_L2 <- data.frame(CD_SSGI_L = names(cor_L), cor_L = cor_L)
cor_L2 <- na_replace(cor_L2,0)

# 상관계수의 mean 
#summary(cor_L2$cor_L)[4] #mean 

# 020302 대분류 업종별 카운드 및 상관계수 테이블 작성 및 저장 
#업종별 카운트 및 상관계수 테이블 작성 및 저장(생략)
summary_ssgi_L_r <- left_join(summary_ssgi_L_nm, cor_L2, by=c("CD_SSGI_L"="CD_SSGI_L"))
summary_ssgi_L_r <- summary_ssgi_L_r[,c("CD_SSGI_L", "NM_SSGI_L", "cnt_L", "sum_ssgi", "cor_L")]

summary_ssgi_L_r_file <- paste0(work_path, output_file_path, "summary_ssgi_L_r.txt")
write.table(summary_ssgi_L_r, file=summary_ssgi_L_r_file, sep = "\t", fileEncoding = "CP949")

# 020303. 소상공인 대분류 필터링
# 020303_more. 
# 소상공인 대분류를 _sum이 있는 부분과 없는 부분으로 나누고 이 중 높은 것 살리기 
# _sum 이 없는 부분 
cor_L2_1 <- cor_L2[!grepl("_sum", rownames(cor_L2)),]
# _sum 이 있는 부분 
cor_L2_2 <- cor_L2[grepl("_sum", rownames(cor_L2)),]
# _sum 이 없는 부분과 _sum 이 있는 부분을 cbind 
cor_L2_12 <- cbind(cor_L2_1, cor_L2_2)
# colnames 바꾸어 줌. 
names(cor_L2_12) <- c("cd1", "cor1", "cd2", "cor2")
# better 라는 변수에, cor1 이 같거나 높으면 해당 변수의 이름(as.character 이용), 아니면 다른 변수 이름
cor_L2_12 <- na_replace(cor_L2_12)
cor_L2_12$more <- ifelse(abs(cor_L2_12$cor1) >= abs(cor_L2_12$cor2)
                              , as.character(cor_L2_12$cd1), as.character(cor_L2_12$cd2)) 

# 상관계수가 높은 변수 이름 모음. 
more_L <- unique(cor_L2_12$more)
# 나은 변수 이름만 있는 상관계수 data frame : cor_L2_more
cor_L2_more <- cor_L2[more_L,]

# 020303_more_better. 
# 대분류 상관계수 중 평균(summary[4]) 보다 크거나 같은 것만 모아서 more_better_L 
cor_L2_more_better <- cor_L2_more[cor_L2_more$cor_L >= summary(cor_L2$cor_L)[4],]
more_better_L <- as.character(unique(cor_L2_more_better$CD_SSGI_L))

# 대분류 중 선택된 내용 
#more_better_L

# 020303_more_better_best. 
# more_better 중 cnt_L (해당 업종 있는 격자수) 이 중앙값[3] 이상만 갖고 와서 m_b_best 
# summary_ssgi_L_r_more_better 
summary_ssgi_L_r_more_better <- summary_ssgi_L_r[summary_ssgi_L_r$CD_SSGI_L %in% more_better_L,]
tt <- summary(summary_ssgi_L_r_more_better$cnt_L)[3]
summary_ssgi_L_r_more_better_best <- 
  summary_ssgi_L_r_more_better[summary_ssgi_L_r_more_better$cnt_L >= tt,]

best_L <- summary_ssgi_L_r_more_better_best$CD_SSGI_L

cor_L2_more_better_best <- cor_L2_more_better[cor_L2_more_better$CD_SSGI_L %in% best_L,]
#best_L

# 0204. 대분류 barplot part ----
# 대분류 상관계수 barplot 
# barplot(cor_L, main="상관계수: 민원발급량 vs 소상공인 수(업종별 대분류)", ylab="상관계수", las=2)
# abline(h=summary(cor_L2$cor_L)[4], col="grey")
# 
# 대분류 상관계수 중 뽑은 변수(more better)만 barplot 
# barplot(cor_L2_more_better$cor_L, main="상관계수: 민원발급량 vs 소상공인 수(업종별 대분류)"
#         , ylab="상관계수", names.arg =cor_L2_more_better$CD_SSGI_L, las=2)
# abline(h=summary(cor_L2$cor_L)[4], col="black")
# 
# 대분류 상관계수 중 뽑은 변수(more_better_best) 만 barplot 
# barplot(cor_L2_more_better_best$cor_L, main="상관계수: 민원발급량 vs 소상공인 수(업종별 대분류)-주요변수"
#         , ylab="상관계수", names.arg =cor_L2_more_better_best$CD_SSGI_L, las=2)
# abline(h=summary(cor_L2$cor_L)[4], col="black")

# 0205. 대분류 summary 저장 ----

# 020501. summary_ssgi_L_r_more_better 저장 (생략) 
# summary_ssgi_L_r_more_better_file <- paste0(work_path, save_file_path, "summary_ssgi_L_r_more_better.txt")
# #write.table(summary_ssgi_L_r_more_better, file=summary_ssgi_L_r_more_better_file, sep = "\t", fileEncoding = "EUC-KR")
# 
# 020502. summary_ssgi_L_r_more_better_best 저장 
summary_ssgi_L_r_more_better_best_file <- paste0(work_path, save_file_path, summary_ssgi_L_r_more_better_best_output)
write.table(summary_ssgi_L_r_more_better_best, file=summary_ssgi_L_r_more_better_best_file, sep = "\t", fileEncoding = "CP949")

# 03. 격자별 ssgi M 중분류 처리 ----

# 0301. 소상공인 중분류 요약
ssgi_M_load_file <- paste(work_path, ssgi_file_path , ssgi_M_380m_buf_table, sep = "")
ssgi_M <- fread(ssgi_M_load_file, encoding = "unknown", sep = ",", stringsAsFactors = F)
ssgi_M[is.na(ssgi_M)] <- 0

# 요약정보(count, sum, summary) 
cnt_M <- do.call(rbind, lapply(ssgi_M[,-1], function(x) sum(x>0))) #값이 있는 격자 갯수
sum_M <- data.frame(sum_ssgi = do.call(rbind, lapply(ssgi_M[,-1], sum)))  
summary_M <- do.call(rbind, lapply(ssgi_M[,-1], summary)) #summary 

#업종별 카운트 및 서머리 테이블 작성 및 저장
summary_ssgi_M <- cbind(cnt_M, sum_M, summary_M)
summary_ssgi_M$CD_SSGI_M <- rownames(summary_ssgi_M)
summary_ssgi_M$CD_SSGI_M2 <- substr(summary_ssgi_M$CD_SSGI_M, 1,3)

# 0302. CD_NM_SSG_M.TXT 으로 한글 업종명 붙여 주기 
CD_NM_SSGI_M_file <- paste0(work_path, setting_file_path, CD_NM_SSGI_M_table) 
CD_NM_SSGI_M <- fread(file = CD_NM_SSGI_M_file, encoding = "UTF-8")
summary_ssgi_M_nm <- left_join(summary_ssgi_M, CD_NM_SSGI_M, by= c("CD_SSGI_M2" = "CD_SSGI_M"))
summary_ssgi_M_nm <- summary_ssgi_M_nm[,c("CD_SSGI_M", "NM_SSGI_M", "cnt_M", "sum_ssgi", "Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.")]
# 중분류 업종명 및 격자갯수, 갯수, 서머리 
summary_ssgi_M_nm_file <- paste0(work_path, output_file_path, "summary_ssgi_M.txt")
write.table(summary_ssgi_M_nm, file=summary_ssgi_M_nm_file, sep = "\t", fileEncoding = "CP949", row.names = F)

# 0303. 중분류 격자별 상관계수 ----
# 030301. 민원건수 이용하여 상관계수 구하기  
mwk_ssgi_M <- left_join(mwk, ssgi_M, by=c("k_gid"="gid"))

cor_M <- cor(Filter(is.numeric, mwk_ssgi_M))
cor_M <- cor_M[,1]
cor_M <- cor_M[-1]
cor_M2 <- data.frame(CD_SSGI_M = names(cor_M), cor_M = cor_M)
cor_M2 <- na_replace(cor_M2)
# 중분류 상관계수의 mean 
#summary(cor_M2$cor_M)[4] #mean  0.1870847
# 중분류 상관계수의 3rd Qu. 
#summary(cor_M2$cor_M)[5] #3rd Qu.  0.3410754

# 030302. 중분류 업종별 카운트 및 상관계수 테이블 작성 및 저장 (생략)
summary_ssgi_M_r <- left_join(summary_ssgi_M_nm, cor_M2, by=c("CD_SSGI_M"="CD_SSGI_M"))
summary_ssgi_M_r <- summary_ssgi_M_r[,c("CD_SSGI_M", "NM_SSGI_M", "cnt_M", "sum_ssgi", "cor_M")]
 
summary_ssgi_M_r_file <- paste0(work_path, output_file_path, "summary_ssgi_M_r.txt")
write.table(summary_ssgi_M_r, file=summary_ssgi_M_r_file, sep = "\t", fileEncoding = "EUC-KR")

# 030303. 소상공인 중분류 필터링 
# 030303_more. 
# _sum 이 없는 부분 
cor_M2_1 <- cor_M2[!grepl("_sum", rownames(cor_M2)),]
# _sum 이 있는 부분 
cor_M2_2 <- cor_M2[grepl("_sum", rownames(cor_M2)),]
# _sum 이 없는 부분과 _sum 이 있는 부분을 cbind 
cor_M2_12 <- cbind(cor_M2_1, cor_M2_2)
# colnames 바꾸어 줌. 
names(cor_M2_12) <- c("cd1", "cor1", "cd2", "cor2")
# more 라는 변수에, cor1 이 같거나 높으면 해당 변수의 이름(as.character 이용), 아니면 다른 변수 이름
cor_M2_12 <- na_replace(cor_M2_12)
cor_M2_12$more <- ifelse(abs(cor_M2_12$cor1) >= abs(cor_M2_12$cor2)
                              , as.character(cor_M2_12$cd1), as.character(cor_M2_12$cd2)) 

# 나은 변수 이름 모음. 
more_M <- unique(cor_M2_12$more)

# 나은 변수 이름만 있는 상관계수 data frame : cor_M2_more
cor_M2_more <- cor_M2[more_M,]

# 030303_more_better. 
# 중분류 상관계수 중 3분위(summary[5]) 보다 크거나 같은 것만 모아서 more_better_L 

cor_M2_more_better <- cor_M2_more[cor_M2_more$cor_M >= summary(cor_M2$cor_M)[5],]
more_better_M <- as.character(unique(cor_M2_more_better$CD_SSGI_M))

# 중분류 중 선택된 내용 
more_better_M #33

# 030303_more_better_best
# more_better 중 cnt_hs_M 이 중앙값[3] 이상만 갖고 와서 m_b_best : best L 
# summary_ssgi_M_r_more_better 
summary_ssgi_M_r_more_better <- summary_ssgi_M_r[summary_ssgi_M_r$CD_SSGI_M %in% more_better_M,]
tt <- summary(summary_ssgi_M_r_more_better$cnt_M)[3]
summary_ssgi_M_r_more_better_best <- 
  summary_ssgi_M_r_more_better[summary_ssgi_M_r_more_better$cnt_M >= tt,]

best_M <- summary_ssgi_M_r_more_better_best$CD_SSGI_M
cor_M2_more_better_best <- cor_M2_more_better[cor_M2_more_better$CD_SSGI_M %in% best_M,]

# 0304. 중분류 barplot part ----
# 
# barplot(cor_M, main="상관계수: 민원발급량 vs 소상공인 수(업종별 중분류)", ylab="상관계수", las=2)
# abline(h=summary(cor_M2$cor_M)[5], col="grey") #3분위수 이상
# 
# barplot(cor_M2_more_better$cor_M, main="상관계수: 민원발급량 vs 소상공인 수(업종별 중분류)"
#         , ylab="상관계수", names.arg =cor_M2_more_better$CD_SSGI_M, las=2)
# abline(h=summary(cor_M2$cor_M)[5], col="black")  #3분위수 이상 
# 
# barplot(cor_M2_more_better_best$cor_M, main="상관계수: 민원발급량 vs 소상공인 수(업종별 중분류)-주요변수"
#         , ylab="상관계수", names.arg =cor_M2_more_better_best$CD_SSGI_M, las=2)
# abline(h=summary(cor_M2$cor_M)[5], col="black")  # 평균 이상 
# 

# 0305. 중분류 summary 저장 ----

# 030501. summary_ssgi_M_r_more_better 저장 (생략)
#summary_ssgi_M_r_more_better_file <- paste0(work_path, save_file_path, "summary_ssgi_M_r_more_better.txt")
#write.table(summary_ssgi_M_r_more_better, file=summary_ssgi_M_r_more_better_file, sep = "\t", fileEncoding = "EUC-KR")

# 030502. summary_ssgi_M_r_more_better_best 저장
summary_ssgi_M_r_more_better_best_file <- paste0(work_path, output_file_path, summary_ssgi_M_r_more_better_best_output)
write.table(summary_ssgi_M_r_more_better_best, file=summary_ssgi_M_r_more_better_best_file, sep = "\t", fileEncoding = "CP949")

# 04. 격자별 ssgi S 소분류 처리 ----

# 0401. 소상공인 소분류 요약 ----
ssgi_S_load_file <- paste(work_path, ssgi_file_path,  ssgi_S_380m_buf_table, sep = "")
ssgi_S <- fread(ssgi_S_load_file, encoding = "unknown", sep = ",", stringsAsFactors = F)
ssgi_S[is.na(ssgi_S)] <- 0

# ssgi_S 요약정보(count, sum, summary) 
cnt_S <- do.call(rbind, lapply(ssgi_S[,-1], function(x) sum(x>0))) #값이 있는 격자 갯수
sum_S <- data.frame(sum_ssgi = do.call(rbind, lapply(ssgi_S[,-1], sum)))  #sum(ssgi_S$)
summary_S <- do.call(rbind, lapply(ssgi_S[,-1], summary)) #summary 

#업종별 카운트 및 서머리 테이블 작성 및 저장
summary_ssgi_S <- cbind(cnt_S, sum_S, summary_S)
summary_ssgi_S$CD_SSGI_S <- rownames(summary_ssgi_S)
summary_ssgi_S$CD_SSGI_S2 <- substr(summary_ssgi_S$CD_SSGI_S, 1,6)

# 0402. CD_NM_SSGI_S.TXT 으로 한글 업종명 붙여주기 ----

CD_NM_SSGI_S_file <- paste0(work_path, setting_file_path, CD_NM_SSGI_S_table) 
CD_NM_SSGI_S <- fread(file = CD_NM_SSGI_S_file, encoding = "UTF-8")

summary_ssgi_S_nm <- left_join(summary_ssgi_S, CD_NM_SSGI_S, by= c("CD_SSGI_S2" = "CD_SSGI_S"))
summary_ssgi_S_nm <- summary_ssgi_S_nm[,c("CD_SSGI_S", "NM_SSGI_S", "cnt_S", "sum_ssgi", "Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.")]

summary_ssgi_S_nm_file <- paste0(work_path, output_file_path, "summary_ssgi_S.txt")
write.table(summary_ssgi_S_nm, file=summary_ssgi_S_nm_file, sep = "\t", fileEncoding = "EUC-KR", row.names = F)

# 0403. 소분류 격자별 상관계수 ----
# 040301. 민원건수 이용하여 상관계수 만들기 
mwk_ssgi_S <- left_join(mwk, ssgi_S, by=c("k_gid"="gid"))

cor_S <- cor(Filter(is.numeric, mwk_ssgi_S))
cor_S <- cor_S[,1]
cor_S <- cor_S[-1]
cor_S2 <- data.frame(CD_SSGI_S = names(cor_S), cor_S = cor_S)
cor_S2 <- na_replace(cor_S2, 0)

# barplot 
# barplot(cor_S, main="상관계수: 민원발급량 vs 소상공인 수(업종별 소분류)", ylab="상관계수", las=2)
# abline(h=.3, col="grey")

#histogram cor_s
# hist(cor_S, breaks = 15, xlim = c(-1,1) , 
#      main = "상관계수(소상공인 소분류별 업종수와 민원발급량)의 히스토그램",
#      xlab = "상관계수", ylab = "업종수" )

# 040302. 소분류 업종별 카운트 및 상관계수 테이블 작성 및 저장 (저장 생략) 
summary_ssgi_S_r <- left_join(summary_ssgi_S_nm, cor_S2, by=c("CD_SSGI_S"="CD_SSGI_S"))

summary_ssgi_S_r <- summary_ssgi_S_r[,c("CD_SSGI_S", "NM_SSGI_S", "cnt_S", "sum_ssgi", "cor_S")]

summary_ssgi_S_r_file <- paste0(work_path, output_file_path, "summary_ssgi_S_r.txt")
write.table(summary_ssgi_S_r, file=summary_ssgi_S_r_file, sep = "\t", fileEncoding = "CP949")

#sort 
summary_ssgi_S_r_o <- summary_ssgi_S_r[order(abs(summary_ssgi_S_r$cor_S), decreasing = T),]

# 040303. 소상공인 소분류 필터링 
# 040303_more. 
# _sum 이 없는 부분 
cor_S2_1 <- cor_S2[!grepl("_sum", rownames(cor_S2)),]
# _sum 이 있는 부분 
cor_S2_2 <- cor_S2[grepl("_sum", rownames(cor_S2)),]
# _sum 이 없는 부분과 _sum 이 있는 부분을 cbind 
cor_S2_12 <- cbind(cor_S2_1, cor_S2_2)
# colnames 바꾸어 줌. 
names(cor_S2_12) <- c("cd1", "cor1", "cd2", "cor2")
# more 라는 변수에, cor1 이 같거나 높으면 해당 변수의 이름(as.character 이용), 아니면 다른 변수 이름
cor_S2_12 <- na_replace(cor_S2_12)
cor_S2_12$more <- ifelse(abs(cor_S2_12$cor1) >= abs(cor_S2_12$cor2)
                         , as.character(cor_S2_12$cd1), as.character(cor_S2_12$cd2)) 

# 나은 변수 이름 모음. 
more_S <- unique(cor_S2_12$more)

# 나은 변수 이름만 있는 상관계수 data frame : cor_M2_more
cor_S2_more <- cor_S2[more_S,]

# 040303_more_better. 
# 소분류 상관계수 중 90%분위(quantile(cor_S2_more$cor_S, .9)) 보다 크거나 같은 것만 모아서 more_better_S 
cor_S2_more_better <- cor_S2_more[cor_S2_more$cor_S >= quantile(cor_S2_more$cor_S, .9),]
more_better_S <- as.character(unique(cor_S2_more_better$CD_SSGI_S))

# 소분류 중 선택된 내용 
#more_better_S #49

# 040303_more_better_best
# more_better 중 cnt_hs_S 이 Q3[5] 이상만 갖고 와서 m_b_best : best S 
# summary_ssgi_S_r_more_better 
summary_ssgi_S_r_more_better <- summary_ssgi_S_r[summary_ssgi_S_r$CD_SSGI_S %in% more_better_S,]
tt <- summary(summary_ssgi_S_r_more_better$cnt_S)[5]
summary_ssgi_S_r_more_better_best <- 
  summary_ssgi_S_r_more_better[summary_ssgi_S_r_more_better$cnt_S >= tt,]

best_S <- summary_ssgi_S_r_more_better_best$CD_SSGI_S 
#13 
cor_S2_more_better_best <- cor_S2_more_better[cor_S2_more_better$CD_SSGI_S %in% best_S,]

# 0404. 소분류 barplot part ----

# barplot(cor_S, main="상관계수: 민원발급량 vs 소상공인 수(업종별 소분류)", ylab="상관계수", las=2)
# abline(h=summary(cor_S2$cor_S)[5], col="grey")
# 
# barplot(cor_S2_more_better$cor_S, main="상관계수: 민원발급량 vs 소상공인 수(업종별 소분류)"
#         , ylab="상관계수", names.arg =cor_S2_more_better$CD_SSGI_S, las=2)
# abline(h=summary(cor_S2$cor_S)[5], col="black")
# 
# barplot(cor_S2_more_better_best$cor_S, main="상관계수: 민원발급량 vs 소상공인 수(업종별 소분류)-주요변수"
#         , ylab="상관계수", names.arg =cor_S2_more_better_best$CD_SSGI_S, las=2)
# abline(h=summary(cor_S2$cor_S)[5], col="black")

# 0405. 소분류 summary 저장 ----

# 040501. summary_ssgi_S_r_more_better 저장 (생략) (분석보고서에만 활용)
#summary_ssgi_S_r_more_better_file <- paste0(work_path, output_file_path, "summary_ssgi_S_r_more_better.txt")
#write.table(summary_ssgi_S_r_more_better, file=summary_ssgi_S_r_more_better_file, sep = "\t", fileEncoding = "EUC-KR")

# 040502. summary_ssgi_S_r_more_better_best 저장
summary_ssgi_S_r_more_better_best_file <- paste0(work_path, output_file_path, summary_ssgi_S_r_more_better_best_output)
write.table(summary_ssgi_S_r_more_better_best, file=summary_ssgi_S_r_more_better_best_file, sep = "\t", fileEncoding = "EUC-KR")

# 05. 소상공인 대중소 LMS별 결합 및 저장

# 0501. LMS 전체 결합 및 저장 
# names 통일
names(summary_ssgi_L_r) <- c("CD_SSGI", "NM_SSGI", "cnt_grid", "sum_ssgi", "cor_ssgi")
names(summary_ssgi_M_r) <- c("CD_SSGI", "NM_SSGI", "cnt_grid", "sum_ssgi", "cor_ssgi")
names(summary_ssgi_S_r) <- c("CD_SSGI", "NM_SSGI", "cnt_grid", "sum_ssgi", "cor_ssgi")

# rbind 
summary_ssgi_r <- rbind(summary_ssgi_L_r, summary_ssgi_M_r, summary_ssgi_S_r)

#save RData, txt 
ssgi_r <- summary_ssgi_r

# 전체 분류별 상관계수 (저장 생략) 
#ssgi_r_RData_file <- paste0(work_path, output_file_path, "ssgi_r.RData")
#save(ssgi_r ,file = ssgi_r_RData_file)
#ssgi_r_file <- paste0(work_path, save_file_path, "ssgi_r.txt")
#write.table(ssgi_r, file=ssgi_r_file, sep = "\t", fileEncoding = "EUC-KR", row.names = F)


# 0502. LMS별 best 전체 결합 및 저장 
# names 통일 
names(summary_ssgi_L_r_more_better_best) <- c("CD_SSGI", "NM_SSGI", "cnt_grid", "sum_ssgi", "cor_ssgi")
names(summary_ssgi_M_r_more_better_best) <- c("CD_SSGI", "NM_SSGI", "cnt_grid", "sum_ssgi", "cor_ssgi")
names(summary_ssgi_S_r_more_better_best) <- c("CD_SSGI", "NM_SSGI", "cnt_grid", "sum_ssgi", "cor_ssgi")

# rbind 
summary_ssgi_r_best <- rbind(summary_ssgi_L_r_more_better_best, 
                             summary_ssgi_M_r_more_better_best, 
                             summary_ssgi_S_r_more_better_best)

#save RData, txt 
ssgi_r_best <- summary_ssgi_r_best

# best 변수들 대상 상관계수 파일 결합 저장 (생략) 
# ssgi_r_best_RData_file <- paste0(work_path, output_file_path, "ssgi_r_best.RData")
# save(ssgi_r_best ,file = ssgi_r_best_RData_file)
# ssgi_r_best_file <- paste0(work_path, output_file_path, "ssgi_r_best.txt")
# write.table(ssgi_r_best, file=ssgi_r_best_file, sep = "\t", fileEncoding = "EUC-KR", row.names = F)


# 0503. LMS별 more, more_better, best 변수 저장 

#more_L
more_L_RData_file <- paste0(work_path, ssgi_dp_path, more_L_RData)
save(more_L, file = more_L_RData_file)

#more_better_L
more_better_L_RData_file <- paste0(work_path, ssgi_dp_path, more_better_L_RData)
save(more_better_L, file = more_better_L_RData_file)

#best_L
best_L_RData_file <- paste0(work_path, ssgi_dp_path, best_L_RData)
save(best_L, file = best_L_RData_file)

#more_M
more_M_RData_file <- paste0(work_path, ssgi_dp_path, more_M_RData)
save(more_M, file = more_M_RData_file)

#more_better_M
more_better_M_RData_file <- paste0(work_path, ssgi_dp_path, more_better_M_RData)
save(more_better_M, file = more_better_M_RData_file)

#best_M
best_M_RData_file <- paste0(work_path, ssgi_dp_path, best_M_RData)
save(best_M, file = best_M_RData_file)

#more_S
more_S_RData_file <- paste0(work_path, ssgi_dp_path, more_S_RData)
save(more_S, file = more_S_RData_file)

#more_better_S
more_better_S_RData_file <- paste0(work_path, ssgi_dp_path, more_better_S_RData)
save(more_better_S, file = more_better_S_RData_file)

#best_S
best_S_RData_file <- paste0(work_path, ssgi_dp_path, best_S_RData)
save(best_S, file = best_S_RData_file)