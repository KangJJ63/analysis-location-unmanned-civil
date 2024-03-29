Sys.setenv(JAVA_HOME  =  'C:\\Program Files\\Java\\jre1.8.0_241')

library(data.table)
library(stringr)


filenames <- list.files('C:\\19project\\07.UCK\\01.Data\\01.Raw_data\\06.SSGI', pattern=glob2rx("*.csv"), full.names=TRUE)
df <- rbindlist(lapply(filenames, fread, encoding = 'UTF-8'))

df <- df[,c(1:2, 4:9, 15, 25, 27, 38:39)]
colnames(df) <- c("BZ_NUM",
                  "BZ_NM",
                  "CD_BZ_L",
                  "NM_BZ_L",
                  "CD_BZ_M",
                  "NM_BZ_M",
                  "CD_BZ_S",
                  "NM_BZ_S",
                  "SIG_NM",
                  "BZ_ADDR",
                  "BZ_RD_ADDR",
                  "X_WG",
                  "Y_WG")

df$BZ_NUM <- str_trim(df$BZ_NUM, side = c("both"))
df$BZ_NM <- str_trim(df$BZ_NM, side = c("both"))
df$CD_BZ_L <- str_trim(df$CD_BZ_L, side = c("both"))
df$NM_BZ_L <- str_trim(df$NM_BZ_L, side = c("both"))
df$CD_BZ_M <- str_trim(df$CD_BZ_M, side = c("both"))
df$NM_BZ_M <- str_trim(df$NM_BZ_M, side = c("both"))
df$CD_BZ_S <- str_trim(df$CD_BZ_S, side = c("both"))
df$NM_BZ_S <- str_trim(df$NM_BZ_S, side = c("both"))
df$SIG_NM <- str_trim(df$SIG_NM, side = c("both"))
df$BZ_ADDR <- str_trim(df$BZ_ADDR, side = c("both"))
df$BZ_RD_ADDR <- str_trim(df$BZ_RD_ADDR, side = c("both"))
df$X_WG <- str_trim(df$X_WG, side = c("both"))
df$Y_WG <- str_trim(df$Y_WG, side = c("both"))

df1 <- df[,c(3:8)]
df1 <- unique(df1)
write.csv(df1, "C:\\19project\\07.UCK\\01.Data\\02.Use_Data\\SSGI_All_List.csv", row.names = FALSE, fileEncoding = 'CP949')

write.csv(df, "C:\\19project\\07.UCK\\01.Data\\02.Use_Data\\SSGI_Raw.csv", row.names = FALSE, fileEncoding = 'CP949')