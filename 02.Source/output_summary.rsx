
# Date_cr: 2019-12-01
# Date_up: 2019-12-01
# Project: 2019 UCK
# Code Nm: output_summary.R
# Coder N: pkim 
# REMARK : QGIS 조회 및 탐색을 위한 격자 및 탐색용 데이터 
# output : 

# 1. Packages Loading ----------
library(data.table)
library(dplyr)
library(plyr) #ddplyr
library(stringr)
library(nnet)
library(MASS)
library(randomForest)
library(reshape2)
library(fBasics)
library(klaR)
library(caret)
library(xgboost)
library(glmnet)
library(e1071)
library(randomForest)
library(ROCR)
library(pROC)

options(scipen=10000)
#theme_update(plot.title = element_text(hjust = 0.5))

mkn <- read.csv('C:\\19project\\07.UCK\\01.Data\\04.Setting_data\\06.Modeling\\modeling_kr_nm.csv', header = FALSE, stringsAsFactors = FALSE)

# Extra) Review용 (QGIS) Data Set 작성 ----
# 1) Validity INPUT - "버스정류소", "서비스인구 3종" 
load("C:\\19project\\07.UCK\\01.Data\\05.Modeling\\kiosk_validity.RData")  
kiosk_validity2 <- kiosk_validity[,c("gid", "YN_kiosk", "BS", "H", "W", "V", "n_biz", "n_biz_sum")]

# 2) Kiosk table - 전체 발급기(기타 제외) 이름 격자
kiosk <- fread('C:\\19project\\07.UCK\\01.Data\\03.Data_Preprocessing\\01.Kiosk\\Reference_Data\\kiosk_list_gid.csv', encoding = "unknown", sep = ",", header = T) 
kiosk <- kiosk[kiosk$k_char != mkn[, 1][1],]  #기타를 빼면 48개에서 3개 빠져서 45개 남음 
kiosk <- kiosk[,c("k_gid", "nm_k_cf", "k_char")]
head(kiosk)
#           k_gid      nm_k_cf k_char
# 1: 다바39aa99aa 우정읍사무소 지자체
# 2: 다바39ab98ab 우정조암농협   은행
# 3: 다바40ba98aa 장안면사무소 지자체

# 격자에 kiosk 이름 필요 (이름이 여러개 있을 수 있으므로 합쳐주는 루틴) 
# kiosk 중 발급기 있는 격자 
k_gid_list <- unique(kiosk$k_gid)

# kiosk 중 k_gid_list 에 있는 것만 추출 
#kiosk <- kiosk[kiosk$k_gid %in% k_gid_list , ]

#length(unique(kiosk$k_gid)) #40 #38 hs 36 

kiosk_s <- kiosk[,c("k_gid", "nm_k_cf")]
kiosk_s <- unique(kiosk_s) #41 #38 hs 

kiosk_a <- aggregate(nm_k_cf ~ k_gid , kiosk_s, length)
names(kiosk_a) <- c("k_gid", "k_cnt")
#kiosk_a

kiosk_s <- left_join(kiosk_s, kiosk_a, by="k_gid")
#kiosk_s
kiosk_s1k <- kiosk_s[kiosk_s$k_cnt == 1,]
kiosk_s2k <- kiosk_s[kiosk_s$k_cnt != 1,]

tt <- data.frame() 
for ( k in unique(kiosk_s2k$k_gid)) {
  t_k_cnt <- kiosk_s2k[kiosk_s2k$k_gid == k, c("k_cnt")][1]
  t_d <- dcast(kiosk_s2k[kiosk_s2k$k_gid == k,], k_gid ~ nm_k_cf, value.var = "k_cnt")
  t_nm <- paste0(setdiff(names(t_d), "k_gid"), collapse = "-")
  t <- data.frame(k_gid = k, nm_k_cf = t_nm, k_cnt = t_k_cnt)
  tt <- rbind(tt,t)
}

kiosk_s12 <- rbind(kiosk_s1k, tt)
kiosk_new <- kiosk_s12[,c("nm_k_cf", "k_gid")]

# 3) 발급량 예측 모델에 들어가는  발급량  3종 / 해당 년 - 발급량 모델의 발급량
load("C:\\19project\\07.UCK\\01.Data\\05.Modeling\\f_mw_WD_DT.RData") # kiosk modeling 에 들어가는 데이터
#names(f_mw_WD_DT)
f_mw_WD_DT_s <- f_mw_WD_DT[,c("k_gid", "cnt_mw3")]
#names(f_mw_WD_DT_s)
a_f_mw_WD_DT <- aggregate(cnt_mw3 ~ k_gid, f_mw_WD_DT_s, sum)
head(a_f_mw_WD_DT) #36
#          k_gid cnt_mw3
# 1 다바39aa99aa    6567
# 2 다바39ab98ab    1036
# 3 다바40ba98aa    2668


# 4) 인구정보 - 총인구, 증가율, 증감
jyd_pop_cen_servi_bus <- fread('C:\\19project\\07.UCK\\01.Data\\03.Data_Preprocessing\\07.NGII_Pop\\NGII_Tot_Eld_WAG.csv', encoding = 'unknown', sep = ',')
jyd_pop_cen_servi_bus[is.na(jyd_pop_cen_servi_bus)] <- 0
pop <- jyd_pop_cen_servi_bus[,c("gid", 
                                "Tot_04", "Tot_10")]
pop[is.na(pop)] <- 0
pop$p_grow <- ((pop$Tot_10 + 0.00001) - (pop$Tot_04 + 0.00001)) / ( pop$Tot_04 + 0.00001 ) 
pop$p_diff <- pop$Tot_10 - pop$Tot_04
pop <- pop[,c("gid", "Tot_10", "p_grow", "p_diff")]
head(pop) 
#             gid tot_1810_v    p_grow p_diff
# 1: 다바39ba97bb          0 0.0000000      0
# 2: 다바36aa94ab          0 0.0000000      0
# 3: 다사38aa12bb         30 0.1333333      4


# 5) 전체 격자 발급량 예측 <--- 발급량 모델링 
#pred_gid_k <- fread("pred_gid_k.txt", encoding = "UTF-8")

pred_gid_k <- fread( file = 'C:\\19project\\07.UCK\\03.Output\\07.Modeling\\kiosk_modeling_gid_pred.txt')
names(pred_gid_k) <- c("gid", "pred", "km_grade")
#table(pred_gid_k$km_grade)
#    1    2    3    4    5    6    7    8    9   10 
# 1230 1149 1190 1189 1190 1189 1189 1190 1189 1190 
#head(pred_gid_k)
#             gid    pred km_grade
# 1: 다바14ba99bb 4354.42        5
# 2: 다바15ab95ab 4354.60        5


# 6) 설치타당성 예측
validity_output <- fread("C:\\19project\\07.UCK\\03.Output\\07.Modeling\\vm_gbm_output_pred.csv")
validity_output <- validity_output[,c("gid", "model_class", "model_class_prob", "fit_class")]
colnames(validity_output) <- c("gid", "vm_class", "vm_prob", "vm_grade")
#summary(validity_output)
#boxplot(validity_output[,3:5])

#
# 1) Validity INPUT - "버스정류소", "서비스인구 3종": kiosk_validity2 : "gid", "YN_kiosk", "c_bus_stat", "H", "W", "V", "n_biz", "n_biz_sum"
# 2) Kiosk table - 전체 발급기(기타 제외) 이름 격자: kiosk_new : "nm_k_cf" "k_gid"
# 3) 발급량 예측: a_f_mw_WD_DT :           k_gid cnt_mw3
# 4) 인구정보 - 총인구, 증가율, 증감: pop : gid tot_1810_v    p_grow p_diff
# 5) 전체격자 발급량 예측: pred_gid_k :   gid    pred km_grade
# 6) 설치타당성 예측: validity_output : gid vm_class vm_prob vm_grade

# 1) +2) 
kv_nm <- left_join(kiosk_validity2, kiosk_new, by=c("gid" = "k_gid"))
kv_nm$nm_k_cf[is.na(kv_nm$nm_k_cf)] <- ""

# + 3) 
kv_nm_mw <- left_join(kv_nm, a_f_mw_WD_DT, by = c("gid" = "k_gid"))
kv_nm_mw[is.na(kv_nm_mw)] <- 0

# + 4) 
kv_nm_mw_p <- left_join(kv_nm_mw, pop, by=c("gid"))
kv_nm_mw_p[is.na(kv_nm_mw_p)] <- 0

# + 5)
kv_nm_mw_p_km <- left_join(kv_nm_mw_p, pred_gid_k, by=c("gid"))
kv_nm_mw_p_km[is.na(kv_nm_mw_p_km)] <- 0

# + 6) 
kv_nm_mw_p_km_vm <- left_join(kv_nm_mw_p_km, validity_output, by=c("gid"))
kv_nm_mw_p_km_vm[is.na(kv_nm_mw_p_km_vm)] <- 0

# save 
output_summary  <- kv_nm_mw_p_km_vm
output_summary <- output_summary[, c("gid", "YN_kiosk", "nm_k_cf", "cnt_mw3", 
                           "pred", "km_grade", "vm_class", "vm_prob", "vm_grade",
                           "H", "W", "V", "Tot_10", "p_grow", "p_diff", 
                           "BS", "n_biz", "n_biz_sum" )]

write.table(output_summary, file = "C:\\19project\\07.UCK\\03.Output\\07.Modeling\\output_summary.txt", sep = "\t", fileEncoding = "CP949", row.names = F)
              