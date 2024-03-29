
# Date_cr: 2019-12-01
# Date_up: 2019-12-01
# Project: 2019 UCK
# Code Nm: kiosk_modeling.R
# Coder N: thkim, pkim 
# Remark : 모델링 
#          시간대별 발급량 데이터 이용 
#          training 과 test 를 k_gid 갯수로 나눔. 

# I. Modeling 
# 01. File Loading 
# 02. Data Preprocessing
# 03. Modeling
# 04. Model Performance 확인
# 05. 모델 이용하여 기존 발급기 격자별 값과 예측값 확인하기

# II. Full Data Prediction 
# 06. 전체 격자 데이터 로딩
# 07. 전체 데이터에 대해서 모델 이용 예측
# 08. 예측치를 격자별로 aggregate 
# 09. 예측 결과를 이용한 평가 기준을 이용하여 기존 격자별 평가(분위수) 계산

# 00. Packs and Setting ----

# Settings 
# yr_anal = 2018

# Packages Loading
library(data.table)
library(dplyr)
library(stringr)
library(nnet)
library(MASS)
library(gbm) # for varImp 
library(caret) # for varImp 
library(e1071)
library(kernlab)

#숫자 자릿수 옵션(지수형 표시 없앰)
options(scipen=10000)
# system messages in English 
Sys.setenv(LANG = "en_US.UTF-8")

#I. Modeling ---- 

# 01. File Loading ----------

mkn <- read.csv('C:\\19project\\07.UCK\\01.Data\\04.Setting_data\\06.Modeling\\modeling_kr_nm.csv', header = FALSE, stringsAsFactors = FALSE)

# 0101. load f_mw_WD_DT 
load('C:\\19project\\07.UCK\\01.Data\\05.Modeling\\f_mw_WD_DT.RData')

df <- f_mw_WD_DT

# 02. Data Preprocessing ----------
# 0201. 변수 부분만 추출 ----
origin_df <- df
df <- subset(df, select = -c(k_gid, STD_YM, WK, TIME))
df[is.na(df)] <- 0

# 0202. Data Split ---------
set.seed(12345)
training_samples <- df$cnt_mw3 %>%
  createDataPartition(p = 0.7, list = FALSE)

train.data <- df[training_samples, ]
test.data <- df[-training_samples, ]

# 03. Modeling -----
# 0301. Support Vector Machine ---------

# Build model (scale 옵션을 이용하여 진행) 

# 030101. SVM Train 
fit_svm <- train(cnt_mw3 ~., data = train.data, method = "svmRadial",
                 trControl= trainControl(method = "repeatedcv", number = 10, repeats = 3, classProbs = F, allowParallel = TRUE, savePredictions = TRUE),
                 preProcess = c("center", "scale"),
                 tuneLength = 10)

# 030102. Prediction with test.data  
pred_svm <- predict(fit_svm, test.data)
df_fit_svm <- data.frame(obs = test.data$cnt_mw3, pred = pred_svm)

# 04. Model Performance 확인 ----

# 0401. RMSE : defaultSummary(df_fit_svm)
t1 <- capture.output(defaultSummary(df_fit_svm))
#t1 output to kiosk_modeling_output
cat(mkn[, 1][8], " ", "**1. Kiosk modeling - RMSE", " ", t1, file = 'C:\\19project\\07.UCK\\03.Output\\07.Modeling\\kiosk_modeling_output.txt', sep="\n",  append = F)

# SVM 결과 확인 ----
# 관측값의 sd 를 구해 봄. 
#sd(test.data$cnt_mw3)
# 예측값의 sd 를 구해 봄. 
#sd(df_fit_svm$pred)

# 관찰값과 예측값의 summary 
#summary(df_fit_svm)

# 0402. 시간대별 테스트 데이터이용 발급량 관측값과 예측값의 산점도(scatter_plot)
png(filename = 'C:\\19project\\07.UCK\\03.Output\\07.Modeling\\kiosk_modeling_scatter_plot_test.png')
plot(df_fit_svm$obs, df_fit_svm$pred, col="darkgrey", obs=0.7 , 
     main = mkn[, 1][9],
     xlab = mkn[, 1][10] ,ylab = mkn[, 1][11])
abline(1,1, col="blue")
dev.off()

#time (fit_svm 학습 소요시간)
#fit_svm$times[1]

# 0403. final model 결과 
#final Model (fit_svm 파라미터 및 정리 )
#fit_svm$finalModel
t2 <- capture.output(fit_svm$finalModel)
#t2 output to kiosk_modeling_output
cat(" "," ","**2. Kiosk modeling - final model ", " ", t2, file = 'C:\\19project\\07.UCK\\03.Output\\07.Modeling\\kiosk_modeling_output.txt', sep="\n",  append = T)

# 0404. 변수 중요도 파악 
# 040401. 변수 중요도 상위 15개 추출 
# variable importance 
varImp_svm <- varImp(fit_svm, scale = T)
df_varImp_svm <- as.data.frame(varImp_svm[1]$importance)
df_varImp_svm$vars <- row.names(df_varImp_svm)
df_varImp_svm <- df_varImp_svm[order(df_varImp_svm$Overall, decreasing = T), c("vars", "Overall")]
df_varImp_svm2 <- df_varImp_svm[1:15,]
# 변수 중요도 상위 15개 골라 냈음. 

# 040402. 변수 이름 테이블(var_names) 이용하여, 변수 설명(한글) 붙이기 
load('C:\\19project\\07.UCK\\01.Data\\05.Modeling\\var_names.RData')
df_varImp_svm2_nm <- dplyr::left_join(df_varImp_svm2, varnames, by=c("vars"))
df_varImp_svm2_nm$no <- 1:nrow(df_varImp_svm2_nm)
df_varImp_svm2_nm <- df_varImp_svm2_nm[,c("no", "vars", "nm_vars", "Overall")]

# 변수 중요도 15개 저장. 
write.table(df_varImp_svm2_nm, file = 'C:\\19project\\07.UCK\\03.Output\\07.Modeling\\kiosk_modeling_var_imp.txt', row.names = F, fileEncoding = "CP949", sep = "\t")

# 040403. 변수 중요도 플롯 
png(filename = 'C:\\19project\\07.UCK\\03.Output\\07.Modeling\\kiosk_modeling_var_imp.png')
plot(varImp_svm, top = 15, main = mkn[, 1][12],
     xlab = mkn[, 1][13],
     ylab = mkn[, 1][14])
dev.off()

#0# save(fit_svm, file=paste0(work_path, "/", "tt_kiosk_modeling_fit_svm.RData"))

# 05. 모델 이용하여 기존 발급기 격자별 값과 예측값 확인하기 ---- 

#0# load(file=paste0(work_path, "/", "tt_kiosk_modeling_fit_svm.RData"))

# 0501. 기존 발급 데이터를 이용하여 예측(발급기있는 격자만) 
pred_kiosk <- fit_svm %>% predict(f_mw_WD_DT)

f_mw_WD_DT_with_pred <- cbind(pred_kiosk, f_mw_WD_DT)
f_mw_WD_DT_with_pred_s <- f_mw_WD_DT_with_pred[,c("k_gid", "STD_YM", "WK", "TIME", "cnt_mw3", "pred_kiosk")]
names(f_mw_WD_DT_with_pred_s) <- c("k_gid", "STD_YM", "WK", "TIME", "obs", "pred")
#head(f_mw_WD_DT_with_pred_s)

# 0502. 기존 격자별 예측결과를 이용하여 RMSE 계산과 scatter plot 
# 예측 결과를 이용한 RMSE 등 defaultSummary 계산 (이것은 격자별 합계 전 이므로 넘어 감. )
#sd(f_mw_WD_DT_with_pred_s$obs)
#defaultSummary(f_mw_WD_DT_with_pred_s)

# 0503. 관측값과 예측값을 격자별로 aggregate 
f_mw_with_pred <- aggregate( cbind(obs, pred) ~ k_gid, f_mw_WD_DT_with_pred_s, sum)

# 050301. 격자별로 집계한 예측 결과를 이용한 RMSE 등 dafaultSummary 계산 
#defaultSummary(f_mw_with_pred)

t3 <- capture.output(defaultSummary(f_mw_with_pred))
cat(" ", mkn[, 1][15], " ", t3
    , file = 'C:\\19project\\07.UCK\\03.Output\\07.Modeling\\kiosk_modeling_output.txt', sep="\n",  append = T)

# 050302. 격자별로 집계한 예측 결과의 관측값과 예측값 summary 
t4 <- capture.output(summary(f_mw_with_pred[,2:3]))
cat(" ", mkn[, 1][16], " ", t4
    , file = 'C:\\19project\\07.UCK\\03.Output\\07.Modeling\\kiosk_modeling_output.txt', sep="\n",  append = T)

# 050303. 격자별로 집계한 예측 결과의 관측값과 예측값의 산점도(scatter plot) 
png(filename = 'C:\\19project\\07.UCK\\03.Output\\07.Modeling\\kiosk_modeling_scatter_plot_gid.png')
plot(f_mw_with_pred$obs, f_mw_with_pred$pred, col="darkblue", obs=0.7, 
     main=mkn[, 1][17],
     xlab = mkn[, 1][18], ylab = mkn[, 1][19])
abline(1, 1, col = "lightblue")
dev.off()

# II. Full Data Prediction ----

# 06. 전체 격자 데이터 로딩 ----
load('C:\\19project\\07.UCK\\01.Data\\05.Modeling\\f_WD_DT.RData')

# 07. 전체 데이터에 대해서 모델 이용 예측 ----
pred_svm_full <- fit_svm %>% predict(f_WD_DT)

# 08. 예측치를 격자별로 aggregate ----
f_WD_DT_p <- cbind(pred_svm_full, f_WD_DT)
f_WD_DT_p <- f_WD_DT_p[,c("k_gid", "STD_YM", "TIME", "pred_svm_full")]

# 0801. aggregate by GID
pred_gid <- aggregate(pred_svm_full ~ k_gid, f_WD_DT_p, sum)
# 0801_추가1. aggregate by GID, STD_YM 
#pred_gid_STDYM <- aggregate(pred_svm_full ~ k_gid + STD_YM, f_WD_DT_p, sum)
# 0801_추가2. aggregate by GID, TIME 
#pred_gid_TIME <- aggregate(pred_svm_full ~ k_gid + TIME, f_WD_DT_p, sum)

# 0802. 예측 결과에 대한 EDA ----
#nrow(pred_gid) #전체 격자
#summary(pred_gid) 

# 기본 예측값으로 히스토그램 (발급량 예측치에 음수가 있음) 
# 격자별 발급량 예측치 히스토그램 1
#hist(pred_gid$pred_svm_full, 35, main = "격자별 발급량 예측치 히스토그램", xlab = "격자별 발급량 예측치", ylab = "격자 수")
# 격자별 발급량 예측치 히스토그램 2 (밀도추정) 
#hist(pred_gid$pred_svm_full, 35, freq = F, main = "격자별 발급량 예측치 히스토그램/밀도추정")
#lines(density(pred_gid$pred_svm_full), col = "blue")

# 발급량 예측치에 음수가 있음. 
#nrow(pred_gid[pred_gid$pred_svm_full <0,])

# 발급량 예측은 음수가 될 수 없으므로, 음수 예측량은 0 으로 치환
pred_gid$pred_svm_full[pred_gid$pred_svm_full <0] <- 0

# 080201. 전체 격자에 대한 발급예측량의 summary 
# summary 
t5 <- capture.output(summary(pred_gid[,2]))
cat(" ", mkn[, 1][20],
    mkn[, 1][21]," ", t5
    , file = 'C:\\19project\\07.UCK\\03.Output\\07.Modeling\\kiosk_modeling_output.txt', sep="\n",  append = T)

# 080202. 전체 격자에 대한 발급예측량의 histogram 
# 격자별 발급량 예측치 히스토그램 3 (음수 발급량 예측치를 0 으로 처리) 
png(filename = 'C:\\19project\\07.UCK\\03.Output\\07.Modeling\\kiosk_modeling_histogram_prediction_gid.png',
    width = 640, height = 480, units = "px", pointsize = 12,
    bg = "white",  res = NA)
hist(pred_gid$pred_svm_full, 35, main = mkn[, 1][22],
     xlab = mkn[, 1][23], ylab = mkn[, 1][24])
dev.off()

# 격자별 발급량 예측치 히스토그램 4 (음수 발급량 예측치를 0 으로 처리 / 밀도추정)
#hist(pred_gid$pred_svm_full, 35, freq = F, main = "격자별 발급량 예측치 히스토그램/밀도추정(2)")
#lines(density(pred_gid$pred_svm_full), col = "blue")

# 09. 예측 결과를 이용한 평가 기준을 이용하여 기존 격자별 평가(분위수) 계산 ---- 

# 0901. 분위수(quantile)로 10단계 만들고, 해당 breaks 로 cut 하기 ----
# 분위수 (quantile) 
breaks_q <- quantile(pred_gid$pred_svm_full, p = c(0, .1, .2, .3, .4, .5, .6, .7, .8, .9, 1 ))
# 분위수가 겹치는 것을 막기 위하여 작은 값을 더함. 
breaks_q <- breaks_q +seq_along(breaks_q) * .Machine$double.eps
# 분위수 맨 처음 값음 0 이어야 함. 
breaks_q[1] <- 0
#round(breaks_q,3)  # 표시만 
pred_gid_cut_q <- cut(pred_gid$pred_svm_full, breaks = breaks_q,
                      right = T, include.lowest = T)
#pred_gid_cut_q
#as.numeric(pred_gid_cut_q)

# 090101. 전체 격자와 발급량 예측치와 등급 붙이기 
pred_gid$grade <- as.numeric(pred_gid_cut_q)
#head(pred_gid)
names(pred_gid) <- c("k_gid", "pred", "grade")
pred_gid$pred <- round(pred_gid$pred,2)

# 090102. 전체 격자 발급량 예측치 등급 저장 
kiosk_modeling_gid_pred <- pred_gid
write.table(kiosk_modeling_gid_pred, file = 'C:\\19project\\07.UCK\\03.Output\\07.Modeling\\kiosk_modeling_gid_pred.txt', row.names = F, fileEncoding = "cP949", sep = "\t")

# 0902. 전체 격자별 발급량 예측 분위수 분포표 (출력용) ----
pred_cut <- as.data.frame(table(pred_gid_cut_q))
#breaks_q[1:10]
#breaks_q[2:11]
pred_cut$from <- breaks_q[1:10]
pred_cut$to <- breaks_q[2:11]
pred_cut$no <- 1:10
pred_cut <- pred_cut[,c("no", "from", "to", "Freq")]

kiosk_modeling_pred_cut <- round(pred_cut,1)

write.table(kiosk_modeling_pred_cut, file = 'C:\\19project\\07.UCK\\03.Output\\07.Modeling\\kiosk_modeling_pred_cut_file.txt', row.names = F, fileEncoding = "CP949", sep = "\t")

# 0903. 기존 발급기 격자에 대한 분위수(quantile) 등급 ---- 
# 발급기있는 격자 알아와서 붙여보기 (40 격자) - 등간격 분위수 
# 090301. 발급기있는 격자의 민원건수(관측치) aggregate
kiosk_mw <- aggregate(cnt_mw3 ~ k_gid, f_mw_WD_DT, sum)

kiosk_mw$grade <- cut(kiosk_mw$cnt_mw3, breaks = breaks_q, 
                      right = T, include.lowest = T)
#table(kiosk_mw$grade)
kiosk_mw$grade <- as.numeric(kiosk_mw$grade)
#table(kiosk_mw$grade )

kiosk_mw <- kiosk_mw[,c("k_gid", "cnt_mw3", "grade")]

# 090302. 발급기 있는 격자의 이름 만들기 kiosk_new ----
# 한 격자에 두개 이상의 발급기 사이트가 있는 경우, 이 이름을 합쳐 준다. 
# 발급기 테이블 가져 오고, 
# 한 격자에 하나의 사이트만 있는 것들은 그대로 두고, 
# 한 격자에 둘 이상의 사이트가 있는 것들을 찾고, 이들의 이름을 합쳐 준다. 
# 그리고 다시 결합하여 kiosk_new 를 만들고, 
# 발급기별 평가 테이블에 이름을 붙힌다. 

kiosk <- read.table(file = 'C:\\19project\\07.UCK\\01.Data\\03.Data_Preprocessing\\01.Kiosk\\Reference_Data\\kiosk_list_gid.csv', sep = ",", header = T, fileEncoding = "CP949")

# 분석용 대상 데이터 중 발급기 있는 격자 
k_gid_list <- unique(kiosk_mw$k_gid)

# kiosk 중 k_gid_list 에 있는 것만 추출 
kiosk <- kiosk[kiosk$k_gid %in% k_gid_list , ]

#head(kiosk)

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

# end of 090201. 

# 090303. 발급기 평가에 발급기 이름 붙이고 저장 

kiosk_mw_nm <- left_join(kiosk_mw, kiosk_new, by = c("k_gid"))
#head(kiosk_mw_nm)

kiosk_mw_nm <- kiosk_mw_nm[,c("k_gid", "nm_k_cf", "cnt_mw3", "grade")]

kiosk_modeling_kiosk_grade <- kiosk_mw_nm

write.table(kiosk_modeling_kiosk_grade, file = 'C:\\19project\\07.UCK\\03.Output\\07.Modeling\\kiosk_modeling_kiosk_grade_file.txt', row.names = F, fileEncoding = "CP949", sep = "\t")