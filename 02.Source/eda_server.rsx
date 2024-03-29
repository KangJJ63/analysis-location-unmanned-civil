
# Date_cr: 2019-12-01
# Date_up: 2019-12-01
# Project: 2019 UCK
# Code Nm: eda_server.R 
# Coder N: pkim 
# REMARK : 
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

# 1) load
bc_server <- fread('C:\\19project\\07.UCK\\01.Data\\03.Data_Preprocessing\\02.DB_Pub_Counter\\DB_Pub_Counter_Daily_Issue.csv') #75244 

length(unique(bc_server$nm_d_c))  #40 $41 
length(unique(bc_server$nm_svr_id)) #472 $502 


# 가장 많은 서류를 발급한 사람----

count_per_server <- aggregate(count ~ nm_svr_id, bc_server, sum)
names(count_per_server) <- c("nm_svr_id", "total_cnt")

days_per_server <- aggregate(count ~ nm_svr_id, bc_server, length)
names(days_per_server) <- c("nm_svr_id", "days")

stat_per_server <- inner_join(days_per_server, count_per_server, by="nm_svr_id")

stat_per_server$cnt_per_day <- stat_per_server$total_cnt / stat_per_server$days

write.table(stat_per_server, file="C:\\19project\\07.UCK\\03.Output\\02.DB_Pub_Counter\\stat_per_server.csv", row.names = F, fileEncoding = "CP949", sep = ",")

png(filename = "C:\\19project\\07.UCK\\03.Output\\02.DB_Pub_Counter\\hist_server_days.png", 
    width = 480, height = 360, units = "px", pointsize = 12)
hist(stat_per_server$days, main= mkn[, 1][25])
dev.off()

png(filename = "C:\\19project\\07.UCK\\03.Output\\02.DB_Pub_Counter\\hist_server_total_issue.png", 
    width = 480, height = 360, units = "px", pointsize = 12)
hist(stat_per_server$total_cnt, main = mkn[, 1][26])
dev.off()

png(filename = "C:\\19project\\07.UCK\\03.Output\\02.DB_Pub_Counter\\hist_server_avg_issue.png", 
    width = 480, height = 360, units = "px", pointsize = 12)
hist(stat_per_server$cnt_per_day, main = mkn[, 1][27], breaks = 21)
dev.off()

summary_stat_per_server <- summary(stat_per_server)
write.table(summary_stat_per_server, file="C:\\19project\\07.UCK\\03.Output\\02.DB_Pub_Counter\\summary_stat_per_server.csv", row.names = F, fileEncoding = "CP949", sep = ",")

# 발급기관별 통계 ----
count_per_dept_server <- aggregate(count ~ nm_d_c + dt_mw, bc_server, sum)
names(count_per_dept_server) #[1] "nm_d_c" "dt_mw"  "count" 

ids_per_dept_server <- aggregate(nm_svr_id ~ nm_d_c + dt_mw , bc_server, length)
names(ids_per_dept_server) <- c("nm_d_c", "dt_mw", "cnt_ids")

stat_per_dept_server <- inner_join(count_per_dept_server, ids_per_dept_server, by=c("nm_d_c", "dt_mw"))
names(stat_per_dept_server)
stat_per_dept_server$cnt_per_svr <- stat_per_dept_server$count / stat_per_dept_server$cnt_ids

write.table(stat_per_dept_server, file="C:\\19project\\07.UCK\\03.Output\\02.DB_Pub_Counter\\stat_per_dept_server.csv", row.names = F, fileEncoding = "CP949", sep = ",")

summary_stat_per_dept_server <- summary(stat_per_dept_server)
write.table(summary_stat_per_dept_server, file="C:\\19project\\07.UCK\\03.Output\\02.DB_Pub_Counter\\summary_stat_per_dept_server.csv", row.names = F, fileEncoding = "CP949", sep = ",")

png(filename = "C:\\19project\\07.UCK\\03.Output\\02.DB_Pub_Counter\\hist_dept_avg_issue.png", 
    width = 480, height = 360, units = "px", pointsize = 12)
hist(stat_per_dept_server$count, main = mkn[, 1][28], breaks = 21)
dev.off()

png(filename = "C:\\19project\\07.UCK\\03.Output\\02.DB_Pub_Counter\\hist_dept_avg_issue_per_srv.png", 
    width = 480, height = 360, units = "px", pointsize = 12)
hist(stat_per_dept_server$cnt_per_svr, main = mkn[, 1][28], breaks=21)
dev.off()

