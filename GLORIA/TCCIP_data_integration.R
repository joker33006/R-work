#################### function
get_wdate <- function(d_path,x,y){ #wheater data path, X=longitude, Y=latitude
  library(data.table)
  library(lubridate)
  fr <- NULL
  dp <- list.files(d_path)
  for (i in c(1:length(dp))){
    dt <- as.data.table(read.table(paste0(d_path,"/",dp[i]),sep=",",row.names = NULL))
    dt[,row.names:=as.numeric(row.names)]
    temp <- as.numeric(dt[row.names==x&LON==y,][,3:(length(dt)-1)])
    date <- colnames(dt)[4:length(dt)]
    result <- as.data.table(cbind(date,temp))
    result[,date:=gsub("X","",date)][,date:=ymd(result$date)]
    fr <- rbind(fr,result)
    }
  fr[,temp:=as.numeric(temp)]
  return(fr)
  }
path <- "E:/Climdata/TCCIP/GLORIA_temp_east"
rpath <- "E:/Climdata/TCCIP/GLORIA_rain_east"

x = 121.00
y=23.45
sen_temp <- get_wdate(path,x,y)
yatsun_temp <- get_wdate(path,121.05,23.45)
sen_rain <- get_wdate(rpath,x,y)
setnames(sen_rain,"temp","rain")
yatsun_rain <- get_wdate(rpath,121.05,23.45)
setnames(yatsun_rain,"temp","rain")

######## loading GLORIA datalogger data

gpath <- "C:/Users/user/Desktop/GLORIA_EVI/gloria_soil_temp_2008-2019.csv"
Td <- fread(gpath)
dt_das <- Td[region=="DAS",]
dt_mean <- dt_das[,lapply(.SD,mean),
                  by=c("summit","year","month","day"),
                  .SDcols="temperature"][,date:=as.Date(paste(year,month,day,sep="-"))]
######### conbind data
r_yat <-dt_mean[summit=="YAT",][yatsun_temp,on=.(date=date)][temperature!="NA",] 
r_sun <- dt_mean[summit=="SUN",][yatsun_temp, on=.(date=date)][temperature!="NA",]
r_sen <- dt_mean[summit=="SEN",][sen_temp, on=.(date=date)][temperature!="NA",] 
plot(r_sun$temperature,r_sun$temp)
m_yat <- lm(temperature~temp,data=r_yat)
m_sun <- lm(temperature~temp,data=r_sun)
m_sen <- lm(temperature~temp,data=r_sen)
sen_result <- sen_temp[,temp_sen:=predict(m_sen,sen_temp)]
yatsun_result <-yatsun_temp[,temp_sun_p:=predict(m_sun,yatsun_temp)][
    ,temp_yat_p:=predict(m_yat,yatsun_temp)]
result <- merge(sen_result,yatsun_result,by="date",fill=T)

svpath <- "C:/Users/user/Desktop/GLORIA_EVI/temp_lm_result"
sink(file=paste0(svpath,"/m_yat.txt"))
print(summary(m_yat))
sink()
sink(file=paste0(svpath,"/m_sun.txt"))
print(summary(m_sun))
sink()
sink(file=paste0(svpath,"/m_sen.txt"))
print(summary(m_sun))
sink()
write.csv(result,file=paste0(svpath,"/temp_lm.csv"))
