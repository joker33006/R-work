Sys.setlocale("LC_TIME", "English") 
#setting the Default language to eng, 
#making the abbreviation of month can be read.

comb_evi <- function(name,path){
  require(data.table)
  require(imputeTS)
  require(readr) 
  evi_t <- fread(paste0(path,name,"/Terra_16D.csv"))
  evi <- evi_t
  colnames(evi)<-c('time','EVI','AQ')
  evi_sel <- evi[,date:=as.Date(time,format="%b %d, %Y")][
    order(date)][AQ<=2,][
      ,EVI:=parse_number(EVI)/10000][
        EVI!=""]
  evi_sel[,EVI.sm:=loess(EVI~as.numeric(date),data=evi_sel,span=0.05)$fit]
  assign(paste0(name,"_EVI"),evi_sel)
}
path <- "E:/忍者/GLORIA_個人處理/EVI/DAS/"
######deal the EVI of SYU
name_list <- c('SEN','SUN','YAT')
for (i in 1:3){
  name <- name_list[i]
  a <- comb_evi(name,path) 
  write.csv(a,paste0(path,'result/',name,'_EVI.csv'))
  assign(paste0(name,'_EVI'),a)
}

######### time series
library(imputeTS)
W <- as.data.table(seq(as.Date('2003-01-01'),as.Date('2020-06-30'),by='weeks'))
W[,yyww:=paste0(year(V1),'-',week(V1))] 
setkey(W,yyww)
for(i in 1:3){
  #### Increasing data resolution to weekly
  rd <- fread(paste0(path,'/result/',name_list[i],'_EVI.csv')) 
  rd[,yyww:=paste0(year(date),'-',week(date))]
  setkey(rd,yyww)
  rd_w <- rd[W][order(i.V1)]
  rd_w[,EVI_rNA:=na_interpolation(EVI)]
  rd_w[,EVI_wsm:=na_interpolation(EVI.sm)]
  rd_ts <- ts(rd_w[,EVI_wsm],frequency =52,start = c(2003,1))
  ts_r = decompose(rd_ts, "additive")
  
  r <- cbind(rd_w[,.(date,yyww,i.V1,EVI_rNA)],ts_r$x,ts_r$seasonal,ts_r$trend,ts_r$random)
  setnames(r,c('i.V1','V2','V3','V4','V5'),c('date_w','EVI','Season','trend','random'))
  assign(paste0(name_list[i],'_ts_r'),r)
  write.csv(r,paste0(path,'ts_result/',name_list[i],'_ts_r.csv'))
}
##### plot the ts_result
name_list <- list.files()
