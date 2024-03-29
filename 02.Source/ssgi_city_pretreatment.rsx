#package loading
library(dplyr)
library(readxl)
library(stringr)
library(data.table)
library(filesstrings)

#SSGI 폴더 지정
ssgi_ori_path = 'C:/19project/07.UCK/01.Data/01.Raw_data/06.SSGI/'
setwd(ssgi_ori_path)

# settings 폴더파일 불러오기
select_yr <- read.csv('C:\\19project\\07.UCK\\01.Data\\04.Setting_data\\04.SSGI\\ssgi_select_year.csv', header = FALSE, stringsAsFactors = FALSE)

#미리 지정한 시군구 가져오기
target_city <- select_yr[, 2][2]

#전처리한 파일 저장
filenames <- list.files(ssgi_ori_path, pattern=glob2rx("*.csv"), full.names=TRUE)
df <- rbindlist(lapply(filenames, fread, encoding = 'UTF-8',sep=','))

#qgis스크립트는 한글 에러나므로 영어로 바꿔줘야함

#기본컬럼 미리 저장
ori_col_nm  <- colnames(df)

#임시로 쓸 컬럼명 저장
temp_col_nm <- ori_col_nm

#임시 컬럼 시군구명 컬럼명 변경
temp_col_nm[15] <- 'sigungu'

#df 변경된 컬럼 적용
colnames(df) <- temp_col_nm

result <- df %>% filter(str_detect(sigungu,target_city))

#다시 기본컬럼으로 저장
colnames(result) <- ori_col_nm
write.table(result,file="ssgi_bc.csv",sep=",",fileEncoding = 'UTF-8',row.names=F)
file.move(filenames,paste0(ssgi_ori_path,'bak'))