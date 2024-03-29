#인구데이터 경로
pop_path <- 'C:/19Project/07.UCK/01.Data/01.Raw_data/08.NGII_Pop/'
#주용도코드 경로
jyd_path <- 'C:/19Project/07.UCK/01.Data/01.Raw_data/10.JYD/'
#버스정류소 현황 경로
bus_path <- 'C:/19Project/07.UCK/01.Data/01.Raw_data/09.Bus_Stop/'
#건축물용도 코드 경로
jyd_nm_path <- 'C:/19Project/07.UCK/01.Data/01.Raw_data/11.JYD_NM/'
#읍면동경계 경로
emd_path <- 'C:/19Project/07.UCK/01.Data/01.Raw_data/12.EMD/'

#옮길 경로 ( Use_Data 폴더)
move_path <- 'C:/19Project/07.UCK/01.Data/02.Use_Data/'


#총인구(이전) 데이터 경로
tot_pre_path <- paste0(pop_path,'Tot_pop/pre/')
#총인구(현재) 데이터 경로
tot_next_path <- paste0(pop_path,'Tot_pop/next/')
#고령인구(이전) 데이터 경로
eld_pre_path <- paste0(pop_path,'Eld_pop/pre/')
#고령인구(이전) 데이터 경로
eld_next_path <- paste0(pop_path,'Eld_pop/next/')
#생산가능인구(이전) 데이터 경로
wag_pre_path <- paste0(pop_path,'Wag_pop/pre/')
#생산가능인구(이전) 데이터 경로
wag_next_path <- paste0(pop_path,'Wag_pop/next/')

#shp및 여러파일 move 함수
copy_to_use <- function(path,new_filenm){
  
  temp <- 'C:/19Project/07.UCK/01.Data/02.Use_Data/test'
  file_list <- list.files(path = path)
  for(i in file_list){
    file_path <- paste0(path,i)
    file_split <- strsplit(i,".", fixed=TRUE)
    new_file_path <- paste0(move_path,new_filenm,'.',file_split[[1]][2])
    file.copy(file_path,new_file_path)
  }
}

#총인구 데이터 move
copy_to_use(tot_pre_path,'Tot_pop_pre')
copy_to_use(tot_next_path,'Tot_pop_cur')


#고령인구 데이터 move
copy_to_use(eld_pre_path,'Eld_pop_pre')
copy_to_use(eld_next_path,'Eld_pop_cur')

#생산가능인구 데이터 move
copy_to_use(wag_pre_path,'Wag_pop_pre')
copy_to_use(wag_next_path,'Wag_pop_cur')

#주용도코드 move
copy_to_use(jyd_path,'jyd')

#읍면동경계 move
copy_to_use(emd_path,'EMD')

#버스정류소 데이터 파일명
bus_file <- list.files(path = bus_path)
#데이터 read
bus_df <- read.csv(paste0(bus_path,bus_file))
#컬럼명 변경
names(bus_df)[9:10] <- c('LATITUDE','LOGITUDE')
#데이터 저장
write.csv(bus_df,paste0(move_path,bus_file),row.names = FALSE)

#건축물용도 코드 데이터 파일명
jyd_nm_file <- list.files(path = jyd_nm_path)
#데이터 read
jyd_nm_df <- readxl::read_xls(paste0(jyd_nm_path,jyd_nm_file))
#컬럼명 변경
names(jyd_nm_df) <- c('lbl','JYD_NM','ETC')
#데이터 저장
jyd_split <- strsplit(jyd_nm_file,".", fixed=TRUE)

#파일확장자명 변경
jyd_nm_file <- paste(jyd_split[[1]][1],'csv',sep = '.')

#Use폴더에 저장
write.csv(jyd_nm_df,paste0(move_path,jyd_nm_file),row.names = FALSE)

