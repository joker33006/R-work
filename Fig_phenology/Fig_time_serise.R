path <- "E:/忍者/發表中/博論發表_FigModel/"
library(magrittr)
library(data.table)
library(imputeTS)
library(lubridate)
library(ggpubr) #combind figure
db <- fread(paste0(path,"Yac_sum_figandLeaf.csv"))
db[,date:=as.Date(date)]
db[,week:=week(date)][,year:=year(date)][,yyww:=paste0(year,"-",week)]
db2 <- as.data.table(cbind(sort(rep(c(2009:2016),52)),rep(c(1:52),8)))
setnames(db2,c("V1","V2"),c("year","week"))
db2 <- db2[,yyww:=paste0(year,"-",week)]
db3 <- merge(db2,db,by=("yyww"),all=TRUE)
db3 <- db3[order(year.x,week.x)]
db3 <- db3[-c(1:35),]
db3[,5:22]=na_interpolation(db3[,5:22],option = "linear")
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
data[,yyww:=as.Date(paste(year.x,week.x,1),"%Y%U%u")]
data[,31:33:=NULL]
write.csv(data,paste0(path,"result.csv"))
########### plot and analysisc
colnames(data)
library(ggplot2)
library(rccdates) #change the date format
data[,yyww:=as.Date(paste(year.x,week.x,1),"%Y%U%u")]

name.list <- c("total","A","B","C","D&E")
get_plot_s <- function(data,i){
  name.a <- colnames(data[,2+i*3,with=FALSE])
  name.b <- colnames(data[,17+i*3,with=FALSE])
  r_plot <- ggplot(data=data[year.x==2012,],aes(x=yyww))+
    geom_line(aes(y=!!ensym(name.a),color="male"))+
    geom_line(aes(y=!!ensym(name.b),color="female"))+
    scale_x_date(date_labels = "%m")+
    xlab("Month")+
    ylab("Seasonality")+
    theme(legend.position = "none")
  return(r_plot)
  }
get_plot_t <- function(data,i){
  name.a <- colnames(data[,1+i*3,with=FALSE])
  name.b <- colnames(data[,16+i*3,with=FALSE])
  r_plot <- ggplot(data=data,aes(x=yyww))+
    geom_line(aes(y=!!ensym(name.a),color="male"))+
    geom_line(aes(y=!!ensym(name.b),color="female"))+
    scale_x_date(date_labels = "%Y")+
    xlab("Year")+
    ylab("Trend")+
    theme(legend.position = "none")
  return(r_plot)
}
get_plot_t(data,2)
for (i in c(1:5)){
  p <- get_plot_s(data,i)
    assign(paste0("p_",name.list[i]),p)
    p <- get_plot_t(data,i)
    assign(paste0("pt_",name.list[i]),p)
  }
pc_s <- ggarrange(p_total+ rremove("xlab")+ rremove("ylab"),ggarrange(
                 p_A+ rremove("x.text")+ rremove("xlab")+ rremove("ylab"), 
                     p_B+ rremove("x.text")+ rremove("xlab")+ rremove("ylab"), 
                     p_C+ rremove("ylab")+ rremove("xlab"),
                     `p_D&E`+ rremove("ylab")+ rremove("xlab")
                     ,align="v",heights=c(1,1,1),
                     ncol = 2,nrow=2),ncol=1)
pc_t <- ggarrange(pt_total+ rremove("xlab")+ rremove("ylab"),ggarrange(
  pt_A+ rremove("x.text")+ rremove("xlab")+ rremove("ylab"), 
  pt_B+ rremove("x.text")+ rremove("xlab")+ rremove("ylab"), 
  pt_C+ rremove("ylab")+ rremove("xlab"),
  `pt_D&E`+ rremove("ylab")+ rremove("xlab")
  ,align="v",heights=c(1,1,1),
  ncol = 2,nrow=2),ncol=1)
annotate_figure(pc_s,bottom="month",left=text_grob("Seasonal coefficient",rot = 90))
annotate_figure(pc_t,bottom="Year",left=text_grob("Trend coefficient",rot = 90))
ggsave(paste0(path,"plot/","fig_trend.jpeg"),width=4,height=5,dpi=600)
#######leaf time serise 
###lt start at col=34 and 43
get_plot_lt <- function(data,i){
  name.a <- colnames(data[,31+i*3,with=FALSE])
  name.b <- colnames(data[,40+i*3,with=FALSE])
  r_plot <- ggplot(data=data,aes(x=yyww))+
    geom_line(aes(y=!!ensym(name.a),color="male"))+
    geom_line(aes(y=!!ensym(name.b),color="female"))+
    scale_x_date(date_labels = "%Y")+
    xlab("Year")+
    ylab("Trend")+
    theme(legend.position = "none")
  return(r_plot)
}
get_plot_ls <- function(data,i){
  name.a <- colnames(data[,32+i*3,with=FALSE])
  name.b <- colnames(data[,41+i*3,with=FALSE])
  r_plot <- ggplot(data=data[year.x==2012,],aes(x=yyww))+
    geom_line(aes(y=!!ensym(name.a),color="male"))+
    geom_line(aes(y=!!ensym(name.b),color="female"))+
    scale_x_date(date_labels = "%m")+
    xlab("Month")+
    ylab("Seasonality")+
    theme(legend.position = "none")
  return(r_plot)
}
L_name.list <- c("mL","tL","sL")
for (i in c(1:3)){
  p <- get_plot_ls(data,i)
  assign(paste0("p_",L_name.list[i]),p)
  p <- get_plot_lt(data,i)
  assign(paste0("pt_",L_name.list[i]),p)
}

pc_L_t <- ggarrange(pt_mL+ rremove("x.text")+ rremove("xlab")+ rremove("ylab"), 
            pt_tL+ rremove("x.text")+ rremove("xlab")+ rremove("ylab"), 
            pt_sL+ rremove("ylab")+ rremove("xlab"),
            align="v",heights=c(1,1,1),ncol=1,nrow=3)
annotate_figure(pc_L_t,bottom="Year",left=text_grob("Trend coefficient",rot = 90))
ggsave(paste0(path,"plot/","leaf_trend.jpeg"),width=3,height=5,dpi=600)
pc_L_s <- ggarrange(p_mL+ rremove("x.text")+ rremove("xlab")+ rremove("ylab"), 
                    p_tL+ rremove("x.text")+ rremove("xlab")+ rremove("ylab"), 
                    p_sL+ rremove("ylab")+ rremove("xlab"),
                    align="v",heights=c(1,1,1),ncol=1,nrow=3)
annotate_figure(pc_L_s,bottom="Month",left=text_grob("Seasonal coefficient",rot = 90))
ggsave(paste0(path,"plot/","leaf_season.jpeg"),width=3,height=5,dpi=600)
