
# 00. Packs and Setting ----
Sys.setenv(JAVA_HOME  =  'C:\\Program Files\\Java\\jre1.8.0_241')
# 1. Packages Loading -----
library(data.table)
library(dplyr)
library(plyr) #ddplyr
library(stringr)
library(nnet)
library(MASS)
library(randomForest)
library(fBasics)
library(klaR)
library(caret)
library(xgboost)
library(gbm)
library(glmnet)
library(e1071)
library(randomForest)
library(ROCR)
library(pROC)
library(reshape2)


#숫자 자릿수 옵션(지수형 표시 없앰)
options(scipen=10000)
#ggplot2 option
theme_update(plot.title = element_text(hjust = 0.5))
# system messages in English 
Sys.setenv(LANG = "en_US.UTF-8")

# I. Modeling 

# 1. File Loading
load('C:\\19project\\07.UCK\\01.Data\\05.Modeling\\kiosk_validity.RData')
# load("kiosk_validity.Rdata")
df <- kiosk_validity

# 2. Data Preprocessing
origin_df <- df
df <- subset(df, select = -c(gid))  # gid 삭제 
df[is.na(df)] <- 0

# 2-1) EDA 
# 2-1-1) 상관분석
# all_corr <- round(cor(df), 2)
# write.xlsx(data.frame(all_corr), 
#            file = paste0('validity_corr', as.numeric(format(Sys.time(), '%Y%m%d')), '.xlsx'), 
#            sheetName = "corr", 
#            append = TRUE, 
#            row.names = TRUE)
#all_corr <- melt(all_corr)

# 상관분석 챠트 (생략) 
# png(filename = paste0('validity_corr_chart', as.numeric(format(Sys.time(), '%Y%m%d')), '.png'), width = 1200, height = 1200)
# ggplot(data = all_corr, aes(x=Var1, y=Var2, fill=value)) + 
#	geom_tile()
# dev.off()

# 2-1-2) Summary
#summary_df <- basicStats(df)
#summary_df <- t(summary_df)
# write.xlsx(summary_df, 
#            file = paste0('validity_vars_summary', as.numeric(format(Sys.time(), '%Y%m%d')), '.xlsx'), 
#            sheetName = "기초통계량", 
#            append = TRUE, 
#            row.names =TRUE)


# 2-2) 분석데이터셋 생성

# 분석데이터셋을 만들기 위하여, 
# YN_kiosk 1 과 0 인 두 개 그룹 중에서 샘플링을 한다. 
# 비교를 잘 하기 위하여, 1 과 반대되는 특성을 가진 데이터를 뽑아서 그룹에 넣는다. 
# 그러기 위해서 변수를 normalize 하고, 
# 변수들 값을 총 합(tt) 하고, 이를 이용하여, 두 그룹간의 차이를 확인하고, 
# 차이가 많이 나는 부분에서 0 의 그룹을 뽑느다. 
# 1 과 0 그룹의 비율은 1:2 한다. 
# 향후 샘플링을 고려하여, 2배수로 데이터 세트 크기를 만든다. 
# ex) 1 그룹 (설치 격자) 45 개이면, 이를 두 배인 90으로 만들고, 
#     0 그룹에서 180 개 를 뽑아서, 
#     270개 사이트 그룹을 만들고 여기에서 학습 데이터 셋을 뽑는다. 

# min-max scaler 기능의 normalize 함수 생성 
normalize <- function(x) {
  return((x-min(x))/(max(x)-min(x)))
}

# normalizer 적용 
df_n <- as.data.frame(lapply(df[-1], normalize))
df_n <- cbind(df[1], df_n)
# tt 변수에 요인 변수들 값을 모두 더함. 
df_n$tt <- apply(df_n[,-1], 1, sum, na.rm = T)
df <- df_n 

#summary(df[df$YN_kiosk == 1, c('tt')])
#summary(df[df$YN_kiosk == 0, c('tt')])


mu <- ddply(df, "YN_kiosk", summarize, grp.mean = mean(tt))
#mu
#hist(df[df$YN_kiosk > 0.,c('tt')], xlim = c(0, 180))
#hist(df[df$YN_kiosk <= 0,c('tt')], xlim = c(0, 180))
#boxplot(data=df, tt ~ YN_kiosk)


# ggplot(df, aes(x=tt, fill=factor(YN_kiosk))) +
#   geom_density(alpha = 0.3) +
#   geom_vline(data = mu, aes(xintercept = grp.mean, color = factor(YN_kiosk)))


df_1 <- df[df$YN_kiosk == 1,]  #45 
df_0 <- df[df$YN_kiosk == 0,]


q1_df_1 <- summary(df[df$YN_kiosk == 1, c('tt')])[2]

df_0_low <- df_0[df_0$tt <= q1_df_1  ,] #tt<=q1 of YN_kiosk == 1 

sample_df_0_low <- sample_n(df_0_low, nrow(df_1) * 4 )

df_model_r <- rbind(df_1, df_1, sample_df_0_low) #df_1 두 세트와 df_1 사이즈의 4배 세트
df_model <- df_model_r[,-ncol(df_model_r)] # tt 없애기 


# Y 값을 factor 로 
df_model$YN_kiosk <- as.factor(df_model$YN_kiosk)
colnames(df_model)[1] <- "y"
target_df_opt <- df_model
target_df_opt[is.na(target_df_opt)] <- 0

target_df_opt$y <- as.factor(target_df_opt$y)


# 변수 변화 없는 것 확인 
a <- data.frame(max =apply(Filter(is.numeric, target_df_opt), 2, max), min=apply(Filter(is.numeric, target_df_opt), 2, min))
b <- a[(a$max - a$min) == 0,]
exclude_vars <- row.names(b)
#exclude_vars

#f_mw_WD_DT <- select(f_mw_WD_DT, -exclude_vars)
target_df_opt <- dplyr::select(target_df_opt , - exclude_vars)



# 2-4) Data Split ==========
set.seed(12345)
training_samples <- target_df_opt$y %>%
  createDataPartition(p = 0.7, list = FALSE)

train.data <- target_df_opt[training_samples, ]
test.data <- target_df_opt[-training_samples, ]


# 3. Classification ----------
set.seed(1234567)

# 3-6) Gradient Boosting ==========
# 3-6-1) Build model 
fit_gbm <- train(y ~.,
				 data = train.data,
				 method = 'gbm',
				 metric = 'Accuracy',
				 tuneGrid = expand.grid(n.trees = seq(10, 100, 10), 
										interaction.depth = 1:5, 
										shrinkage = 0.1, 
										n.minobsinnode = 1:10),
  			 trControl = trainControl(method = 'boot',
										 search = 'grid'))

#summary(fit_gbm)


# 3-6-2) prediction 
pred_gbm <- fit_gbm %>% predict(test.data)
confusionMatrix(pred_gbm, test.data$y)

# 4. Compare Models ----------
# 4-1) CounfusionMatrix ==========
confusionmatrix_result <- 		c(
                    round(confusionMatrix(pred_gbm, test.data$y, mode = "prec_recall")$overall[1], 4),
									  round(confusionMatrix(pred_gbm, test.data$y, mode = "prec_recall")$byClass[5], 4),
									  round(confusionMatrix(pred_gbm, test.data$y, mode = "prec_recall")$byClass[6], 4),
									  round(confusionMatrix(pred_gbm, test.data$y, mode = "prec_recall")$byClass[7], 4),
									  format(round(auc(test.data$y, predict(fit_gbm, test.data, type = "prob")[, 2]), 4), nsmall = 4))
names(confusionmatrix_result) <- c('Accuracy', 'Precision', 'Recall', 'F1 Score', 'AUC')

# Waiting Place ---

confusionmatrix_result_df <- as.data.frame(confusionmatrix_result)
colnames(confusionmatrix_result_df) <- c("GBM")
#confusionmatrix_result_df
write.table(confusionmatrix_result_df, file="C:\\19project\\07.UCK\\03.Output\\07.Modeling\\VM_GBM_result.txt", sep = "\t", row.names = T)

# 5. Save Final Model ----------
# 5-1) Predict Full DataSet ==========
idx <- names(train.data)
pred_df <- subset(df, select = idx[-1])

#ridge_p <- table(predict(fit_ridge, pred_df))
#lasso_p <- table(predict(fit_lasso, pred_df))
#elast_p <- table(predict(fit_elastic, pred_df))
#rf____p <- table(predict(fit_rf, pred_df))
#svm___p <- table(predict(fit_svm, pred_df))
gbm___r <- table(predict(fit_gbm, pred_df))
# write.table(pp, file="VM_GBM_PRED.txt", sep = "\t", row.names = T)

# 5-2_GBM) ---- 
# GBM 가보자. 
pred_full_gbm <- predict(fit_gbm, pred_df)
pred_full_gbm_prob <- predict(fit_gbm, pred_df, type="prob")
#head(pred_full_gbm_prob)
#histogram(pred_full_gbm_prob$`0`)
#histogram(pred_full_gbm_prob$`1`)
#summary(pred_full_gbm_prob)

varImp_gbm <- varImp(fit_gbm, scale = T)
df_varImp_gbm <- as.data.frame(varImp_gbm[1]$importance)
df_varImp_gbm$vars <- row.names(df_varImp_gbm)
df_varImp_gbm <- df_varImp_gbm[order(df_varImp_gbm$Overall, decreasing = T), c("vars", "Overall")]
df_varImp_gbm2 <- df_varImp_gbm[1:15,]
# 변수 중요도 상위 15개 골라 냈음. 

#  변수 이름 테이블(var_names) 이용하여, 변수 설명(한글) 붙이기 
load('C:\\19project\\07.UCK\\01.Data\\05.Modeling\\var_names.RData')
df_varImp_gbm2_nm <- dplyr::left_join(df_varImp_gbm2, varnames, by=c("vars"))
df_varImp_gbm2_nm$no <- 1:nrow(df_varImp_gbm2_nm)
df_varImp_gbm2_nm <- df_varImp_gbm2_nm[,c("no", "vars", "nm_vars", "Overall")]

write.table(df_varImp_gbm2_nm, file="C:\\19project\\07.UCK\\03.Output\\07.Modeling\\VM_GBM_varImp.txt", fileEncoding = "CP949", sep = "\t", row.names = F)


# 5-3) Make Result ==========
# 5-3_svm) ----
# (y) 붙이기  with svm  

real <- df$YN_kiosk
idx <- which(real != 0)
real[idx] <- 1
# fit_df_svm <- as.data.frame(cbind(as.character(origin_df$gid), df$YN_kiosk, as.character(real), as.character(pred_full_svm), round(pred_full_svm_prob[, 2], 2)))
# head(fit_df_svm,5)
# names(fit_df_svm) <- c('gid', 'YN_kiosk', 'YN_class', 'model_class', 'model_class_prob')
# fit_df_svm$fit_class <- 0
# str(fit_df_svm)
# 
# hist(as.numeric(as.character(fit_df_svm$model_class_prob)), 21)
#histogram(fit_df$model_class_prob, breaks = 41)
# 
# idx <- which(as.numeric(as.character(fit_df_svm$model_class_prob)) > 0.5)
# fit_df_svm[idx, 6] <- 1
# #index <- as.vector(summary(as.numeric(as.character(fit_df[idx, 5])))[5])
# idx <- which(as.numeric(as.character(fit_df_svm$model_class_prob)) > 0.6)
# fit_df_svm[idx, 6] <- 2
# idx <- which(as.numeric(as.character(fit_df_svm$model_class_prob)) > 0.7)
# fit_df-svm[idx, 6] <- 3
# idx <- which(as.numeric(as.character(fit_df_svm$model_class_prob)) > 0.8)
# fit_df_svm[idx, 6] <- 4
# idx <- which(as.numeric(as.character(fit_df_svm$model_class_prob)) > 0.9)
# fit_df_svm[idx, 6] <- 5


#fit_df <- fit_df[, -5]
# table(fit_df_svm$model_class)
# table(fit_df_svm$fit_class)
# aa_svm <- aggregate(gid ~ fit_class, fit_df_svm, length)
# 
# write.table(aa, file="aa.txt", row.names = F, sep = "\t")


# 5-3_GBM) ----
fit_df_gbm <- as.data.frame(cbind(as.character(origin_df$gid), df$YN_kiosk, as.character(real), as.character(pred_full_gbm), round(pred_full_gbm_prob[, 2], 2)))
head(fit_df_gbm)
names(fit_df_gbm) <- c('gid', 'YN_kiosk', 'YN_class', 'model_class', 'model_class_prob')
fit_df_gbm$fit_class <- 0
str(fit_df_gbm)

#hist(as.numeric(as.character(fit_df_gbm$model_class_prob)), 21)

idx <- which(as.numeric(as.character(fit_df_gbm$model_class_prob)) > 0.5)
fit_df_gbm[idx, 6] <- 1
#index <- as.vector(summary(as.numeric(as.character(fit_df[idx, 5])))[5])
idx <- which(as.numeric(as.character(fit_df_gbm$model_class_prob)) > 0.6)
fit_df_gbm[idx, 6] <- 2
idx <- which(as.numeric(as.character(fit_df_gbm$model_class_prob)) > 0.7)
fit_df_gbm[idx, 6] <- 3
idx <- which(as.numeric(as.character(fit_df_gbm$model_class_prob)) > 0.8)
fit_df_gbm[idx, 6] <- 4
idx <- which(as.numeric(as.character(fit_df_gbm$model_class_prob)) > 0.9)
fit_df_gbm[idx, 6] <- 5

#table(fit_df_gbm$model_class)
#table(fit_df_gbm$fit_class)

#aa_gbm <- aggregate(gid ~ fit_class, fit_df_gbm, length)
#write.table(aa, file="aa.txt", row.names = F, sep = "\t")


# 5-4) Save Result & Model ==========
# 5-4_svm)----

#write.csv(fit_df_svm, file = paste0('validity_svm_output_', as.numeric(format(Sys.time(), '%Y%m%d')), '.csv'))
#save(fit_svm, file = paste0('validity_svm_model_', as.numeric(format(Sys.time(), '%Y%m%d')), '.RData'))


# 5-4_gbm)----

write.csv(fit_df_gbm, file = 'C:\\19project\\07.UCK\\03.Output\\07.Modeling\\vm_gbm_output_pred.CSV', row.names = FALSE)
