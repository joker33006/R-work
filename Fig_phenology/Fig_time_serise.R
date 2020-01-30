path <- "E:/忍者/發表中/博論發表_FigModel/"
library(magrittr)
library(data.table)
library(imputeTS)
library(lubridate)
db <- fread(paste0(path,"Yac_sum_figandLeaf.csv"))
db[,date:=as.Date(date)]
db[,week:=week(date)][,year:=year(date)][,yyww:=paste0(year,"-",week)]
db2 <- as.data.table(cbind(sort(rep(c(2009:2016),52)),rep(c(1:52),8)))
setnames(db2,c("V1","V2"),c("year","week"))
db2 <- db2[,yyww:=paste0(year,"-",week)]
db3 <- merge(db2,db,by=("yyww"),all=TRUE)
db3 <- db3[order(year.x,week.x)]
db3 <- db3[-c(1:35),]
db3[,5:22]=na_kalman(db3[,5:22])
db3[,`1_E`:=NULL]
final_r <- NULL
for (i in c(5:21)){
  t1 <- ts(db3[,..i], 
        freq=365.25/7, 
        start=decimal_date(ymd("2009-09-12")))
  ts_r <- decompose(t1,type = "multiplicative")
  colname <- paste0("ts_", colnames(db3[,..i]))
  ts_rc <- as.data.table(cbind(ts_r$trend,ts_r$seasonal,ts_r$random))
  colnames(ts_rc) <- c(paste0(colname,"_t"),paste0(colname,"_s"),paste0(colname,"_r"))
  final_r <- cbind(final_r,ts_rc)
}
write.csv(db3,paste0(path,"result_fig.csv"))
######################weather data

weat_d <- fread(paste0(path,"發表用氣候資料/weather_hourly.csv"))
wave_d <- fread(paste0(path,"發表用氣候資料/sea_wave_2010-2016_daily.csv"))
weat_d[,Date:=as.Date(Date)][
  ,yyww:=paste0(year(Date),"-",week(Date))]
wave_d <- wave_d[,date:=as.Date(date)][
  ,yyww:=paste0(year(date),"-",week(date))]

weat_d[, names(weat_d) := lapply(.SD, function(x) gsub("X", NA, x))]
weat_d <- na.omit(weat_d)
wea_w <- weat_d[,.(avg_t=mean(as.numeric(Temp)),
                   max_t=max(as.numeric(Temp)),
                   avg_ws=mean(as.numeric(`wind speed`)),
                   max_ws=max(as.numeric(`wind speed`)),
                   pre=sum(as.numeric(precipitation))),
                by=yyww]
wav_w<- wave_d[,.(avg_wh=mean(as.numeric(avg_wh)),
                  max_wh=max(as.numeric(max_wh))),
               by=yyww]
env_w <- wav_w[wea_w,on=.(yyww=yyww)]
######## combind env and ficus phen table
final_r <- cbind(db3[,1:3],final_r)
data <- final_r[env_w,on=.(yyww=yyww)]
data <- na.omit(data,cols = 2)
write.csv(data,paste0(path,"result.csv"))
########### plot and analysis
