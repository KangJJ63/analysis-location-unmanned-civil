
# 00. Packs and Setting ----

# Packages Loading 
library(corrplot)
library(data.table)
library(dplyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(imputeTS)

#숫자 자릿수 옵션(지수형 표시 없앰)
options(scipen=10000)
#ggplot2 option
#theme_update(plot.title = element_text(hjust = 0.5))
# system messages in English 
Sys.setenv(LANG = "en_US.UTF-8")

# Working Directory
work_path <- "C:\\19project\\07.UCK"
setwd(work_path)


# jyd_file_path
jyd_file_path <- "\\01.Data\\03.Data_Preprocessing\\08.JYD\\"
# load file path 
#load_file_path <- "/data_hs/"
# save file path 
save_file_path <- "\\01.Data\\05.Modeling\\"
# output file path
output_file_path <- "\\03.Output\\05.JYD\\"

jyd_kr <- read.csv('C:\\19project\\07.UCK\\01.Data\\04.Setting_data\\05.JYD\\jyd_kr_nm.csv', header = FALSE, stringsAsFactors = FALSE)

# 한글Korean
# 0102  "없음" 
nothing <- jyd_kr[, 1][1] 

# inputs 

# 0102.
jyd_cd_summary_800m_table <- "JYD_CD_800.csv"
#jyd_div_summary_800m_table <- "주용도코드_격자버퍼800m당_대분류코드값_합계.csv"

# 010201.주용도코드용 영어 변수를 즉성에서 만듬. 
temp_jyd_cd_varname_table <- "temp_jyd_cd_varname.txt"
temp_jyd_cd_varname_file <- paste0(work_path, save_file_path, temp_jyd_cd_varname_table)

# 010301
#jyd_name_RData <- "jyd_name.RData"

# outputs 

# status output (output file path)
jyd_status_output <- "jyd_status_output.txt"

# 010202 
jyd_cd_800m_top_table <- "jyd_cd_800m_top.txt"

# 010203 (output file path) 
jyd_cd_a_table <- "jyd_cd_a.txt"

# 010302 (output file path)
#jyd_div_800m_top_table <- "jyd_div_800m_top.txt"

# 010303. (output file path)
#jyd_div_a_table <- "jyd_div_a.txt"

# 0301. (save_file_path)
jyd_cd_800m_d_RData <- "jyd_cd_800m_d.RData"

# 0302. (save_file_path )
#jyd_div_800m_d_RData <- "jyd_div_800m_d.RData"

# ++++++++++++++++++++++++++++++++++++++++++++++
# 01. Load Files ----
# 0101. base_grid (전체 격자가 있는 테이블을 가져온다)----
# 지도 그릴 때만 필요 하므로, 표준화 에서는 뺀다. 
#base_grid_file <- paste0(work_path, jyd_file_path, base_grid_table)
#base_grid <- fread(file = base_grid_file, encoding = "unknown", sep = "," ,stringsAsFactors = F) #11895

#base_grid_c <- base_grid %>% select(gid, jydval, JYD_NM, JYD_DIV, x_gid_cen, y_gid_cen)
#head(base_grid_c)

# 0102. 주용도 800m 개별코드  ----
jyd_cd_800m_file <- paste0(work_path, jyd_file_path, jyd_cd_summary_800m_table)
jyd_cd_800m <- fread(file = jyd_cd_800m_file, sep = "," ,header = T, stringsAsFactors = F)

# sum_val 가 NA 인 칸 에는 0 을, JYD_NM 이 비어 있으면 없음 으로 바꿈.
# sum_val 가 0 인 것 제거함. 
jyd_cd_800m <- jyd_cd_800m[jyd_cd_800m$JYD_NM != "",]
# gid 순, 그리고 sum_val 역순으로 정렬 
jyd_cd_800m[, c("val")] <- na_replace(jyd_cd_800m[,c("val")], 0)
jyd_cd_800m$JYD_NM <- ifelse(jyd_cd_800m$JYD_NM =="", nothing, jyd_cd_800m$JYD_NM)

# 010201. 주용도코드 영문코드 ----
                                                            
# 영문코드는 바꾼다. 
# 있는 코드를 정리하는 방식으로 바꾼다. 

jyd_cd_list <- unique(jyd_cd_800m$JYD_NM)[order(unique(jyd_cd_800m$JYD_NM))]
length(jyd_cd_list)

jyd_cd_var <- data.frame(jyd_cd = jyd_cd_list, varname = 1:length(jyd_cd_list))
jyd_cd_var$varname <- paste0("JCD_",str_pad(as.character(jyd_cd_var$varname), 2, side="left", "0"))

write.table(jyd_cd_var, file = temp_jyd_cd_varname_file, fileEncoding = "CP949", row.names = F, sep="\t")

jyd_cd_800m <- left_join(jyd_cd_800m, jyd_cd_var, by=c("JYD_NM" = "jyd_cd"))

jyd_cd_800m <- jyd_cd_800m %>% arrange(gid, -val)
jyd_cd_800m <- jyd_cd_800m[,c("gid", "JYD_NM", "varname", "val")]
#head(jyd_cd_800m)
#length(unique(jyd_cd_800m$gid)) #bc 959  # hs 11895 

# 010202. 주용도 코드 800m 범위 기준- 주 코드(top1) 추출 ---- 
# sum_val 이 동률일 경우, 둘이 모두 올라오므로, 랜덤하게 한개만 올림. 
# top_n 을 이용해서 뽑고, 여기에 runif 한 값을 더해서 이를 이용해서 한개 선택
set.seed(12345)
jyd_cd_800m_top <- jyd_cd_800m %>% group_by(gid) %>% top_n(1, val)  
jyd_cd_800m_top$val_r <- jyd_cd_800m_top$val + 
  runif(nrow(jyd_cd_800m_top), min=0, max=.0001)
jyd_cd_800m_top <- jyd_cd_800m_top %>% group_by(gid) %>% top_n(1, val_r) 
jyd_cd_800m_top <- jyd_cd_800m_top[,c("gid", "JYD_NM", "varname", "val")]

# console output file 준비 
jyd_status_output_file <- paste0(work_path, output_file_path, jyd_status_output)

t1 <- capture.output(length(unique(jyd_cd_800m_top$gid)))

#t1 output to jyd_status_output
cat(jyd_kr[, 1][2], " ", jyd_kr[, 1][3], t1, 
    file = jyd_status_output_file, sep="\n",  append = F)
t2 <- capture.output(length(unique(jyd_cd_800m_top$JYD_NM)))
cat(" ",jyd_kr[, 1][4], t2,
    file = jyd_status_output_file, sep="\n",  append = T)

#save 800m개별top ----
jyd_cd_800m_top_file <- paste0(work_path, output_file_path, jyd_cd_800m_top_table)
write.table(jyd_cd_800m_top, file = jyd_cd_800m_top_file, 
            sep = "\t", fileEncoding = "CP949", row.names = F)
cat(" ", jyd_kr[, 1][5],
    file = jyd_status_output_file, sep="\n",  append = T)

# 010203. 주용도코드 분포 집계 (800m 인근격자내 제일 많은 코드 기준) ----
# 주용도별 격자 수 
jyd_a <- aggregate(gid ~ JYD_NM, jyd_cd_800m_top, length)
#head(jyd_a)
jyd_a <- jyd_a[order(jyd_a$gid, decreasing = T),]
jyd_a$no <- 1:nrow(jyd_a)
jyd_a <- jyd_a[,c("no", "JYD_NM", "gid")]
names(jyd_a) <- c("no", "jyd_nm", "cnt_gid")
t3 <- capture.output(print(jyd_a, row.names = F))
cat(" ", jyd_kr[, 1][6],
    jyd_kr[, 1][7], t3,
    file = jyd_status_output_file, sep="\n",  append = T)

jyd_cd_a_file <- paste0(work_path, output_file_path, jyd_cd_a_table)
write.table(jyd_a, file = jyd_cd_a_file, sep="\t", fileEncoding = "CP949", row.names = F)

# 03. dcast ---- 
# 0301. jyd_cd_800m ----
#colnames(jyd_cd_800m) #        "gid""sum_val" "JYD_NM"
#head(jyd_cd_800m,5)   # 다바15ba95ba     100  단독주택
#nrow(jyd_cd_800m[jyd_cd_800m$JYD_NM =="없음",]) #11,776 건물이 없는 셀이 800m 내에 포함된 셀이 11,776 개 

jyd_cd_800m_d <- dcast(jyd_cd_800m, gid ~ varname, value.var = "val")
#colnames(jyd_cd_800m_d); head(jyd_cd_800m_d)
#head(jyd_cd_800m_d)
# na 
jyd_cd_800m_d <- na_replace(jyd_cd_800m_d, 0)

#save jyd_cd_800m_d (jyd_cd_800m decasted) ----
jyd_cd_800m_d_file <- paste0(work_path, save_file_path, jyd_cd_800m_d_RData)
save(jyd_cd_800m_d, file = jyd_cd_800m_d_file)

# full varnames "vars", "nm_vars" without JYD
load(file = "C:\\19project\\07.UCK\\01.Data\\04.Setting_data\\06.Modeling\\pre_var_names.RData")

# JYD varname
jyd_varname <- data.table::fread(file="C:\\19project\\07.UCK\\01.Data\\05.Modeling\\temp_jyd_cd_varname.txt" ,
                                 sep="\t", encoding = "unknown")

colnames(jyd_varname) <- c("nm_vars", "vars")

varnames <- rbind(varnames, jyd_varname)

varnames <- unique(varnames)

save(varnames, file = "C:\\19project\\07.UCK\\01.Data\\05.Modeling\\var_names.RData")