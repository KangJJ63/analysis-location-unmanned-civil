#install.packages('reshape2')

# Packages Loading ----------
library(data.table)
library(readxl)
library(dplyr)
library(lubridate)
library(stringr)
library(WriteXLS)
library(hms)
library(reshape2)

df <- read.csv("C:\\19project\\07.UCK\\03.Output\\04.SSGI\\SSGI_Select_Area.csv", stringsAsFactor = FALSE)

df1 <-df[,c(3:8)]
df1 <- unique(df1)
write.csv(df1, "C:\\19project\\07.UCK\\03.Output\\04.SSGI\\SSGI_Select_Area_List.csv", row.names = FALSE, fileEncoding = 'CP949')

df$count <- 1

df_ssgi_l <- aggregate(count ~ s_gid + CD_BZ_L, data = df, sum)
df_ssgi_l <- dcast(df_ssgi_l, s_gid ~ CD_BZ_L, value.var = 'count')
df_ssgi_l[is.na(df_ssgi_l)] <- 0
write.csv(df_ssgi_l, 'C:\\19project\\07.UCK\\01.Data\\03.Data_Preprocessing\\04.SSGI\\Reference_Data\\SSGI_Select_Area_L.csv', row.names = FALSE, fileEncoding = 'CP949')

df_ssgi_m <- aggregate(count ~ s_gid + CD_BZ_M, data = df, sum)
df_ssgi_m <- dcast(df_ssgi_m, s_gid ~ CD_BZ_M, value.var = 'count')
df_ssgi_m[is.na(df_ssgi_m)] <- 0
write.csv(df_ssgi_m, 'C:\\19project\\07.UCK\\01.Data\\03.Data_Preprocessing\\04.SSGI\\Reference_Data\\SSGI_Select_Area_M.csv', row.names = FALSE, fileEncoding = 'CP949')

df_ssgi_s <- aggregate(count ~ s_gid + CD_BZ_S, data = df, sum)
df_ssgi_s <- dcast(df_ssgi_s, s_gid ~ CD_BZ_S, value.var = 'count')
df_ssgi_s[is.na(df_ssgi_s)] <- 0
write.csv(df_ssgi_s, 'C:\\19project\\07.UCK\\01.Data\\03.Data_Preprocessing\\04.SSGI\\Reference_Data\\SSGI_Select_Area_S.csv', row.names = FALSE, fileEncoding = 'CP949')
