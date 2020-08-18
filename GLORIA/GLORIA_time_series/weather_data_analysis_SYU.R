library(data.table)
library(ggplot2)
library(imputeTS)
path <- 'C:/Users/user/Desktop/Temp/ERA5/SYU/'
name_list <- c('DSH','JNJ','TSW')
weather_ts <- function(path,name){
  Sys.setlocale("LC_TIME", "English") 
  library(data.table)
  library(imputeTS)
  library(zoo) #read the ts type data
  wd <- fread(paste0(path,name,
                     "/1979_2020_monthly_weather.csv"))
  colnames(wd) <- c('date','temp_avg','rain')
  wd[,time:=as.Date(date,format="%b %d, %Y")]
  ts_temp<- ts(wd[,temp_avg]-273.15,frequency =12,start = c(1979,1))
  ts_rain <-ts(wd[,rain]*1000,frequency =12,start = c(1979,1)) 
  ts_temp_r <-  decompose(ts_temp, "additive")
  ts_rain_r <-  decompose(ts_rain, "additive")
  result <- cbind(wd[,time],ts_temp_r$x,ts_temp_r$seasonal,ts_temp_r$trend,ts_temp_r$random,
                  ts_rain_r$x,ts_rain_r$seasonal,ts_rain_r$trend,ts_rain_r$random)
  colnames(result) <- c('date','temp','t_season','t_trend','t_random',
                        'rain','r_season','r_trend','r_random')
  result

  }
 
for (i in 1:3){
  Sys.setlocale("LC_TIME", "English")
  name <- name_list[i]
  r <- weather_ts(path,name)
  write.csv(r,paste0(path,'Ts_result/','wt_r_',name,'.csv'))
}
########read the table 
for (i in 1:3){
  name <- name_list[i]
  r <- fread(paste0(path,'Ts_result/','wt_r_',name,'.csv'))
  r[,date:=as.Date(date,format='%b %d, %Y')]
  assign(paste0(name,'_wt_r'),r)
}

ggplot()+
  geom_line(data=DSH_wt_r,aes(x=date,y=temp))+
  scale_x_date(breaks="year",date_labels="%y")+
  xlab("Year")+
  ylab("Temperature")+
  labs(color = "Summit")
ggsave(filename = paste0(path,'/plot/SYU_terra_EVI.jpeg'),width = 7,height = 3,dpi = 600)

