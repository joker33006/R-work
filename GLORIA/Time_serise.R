####FUNCTION#####################################
ts_Sm <- function(loca,i,fpath,svpath){
  library(data.table)
  library(imputeTS)
  #loca=local code,i=file order, 
  #fpath = rawdata path,svpath = result saving path
  table_name <- list.files(fpath)
  dat <- fread(paste0(fpath,"/",table_name[i]))
  colnames(dat)[c(2,3)] <- c("time","var")
  dat[,date:=as.Date(time)]
    result <- dat[area==loca,][,var_f:=na_kalman(var)]
  smoo1 <- smooth.spline(as.numeric(result$date),result$var_f)
  result[,smo_sp:=smoo1$y]
  colnames(result)[3] <- substr(table_name[i],7,9)
  return(result)
}
#######################################
library(data.table)
library(ggplot2)
library(imputeTS) #DEAL with THE TIME SERISE NA problem
fpath <-"C:/Users/user/Desktop/GLORIA_EVI/result"
svpath <- "C:/Users/user/Desktop/GLORIA_EVI/plot/"
i=4
for (j in c(1:3)){
  loca <- c("Sen","Sun","Yat")
  r1 <- ts_Sm(loca[j],i,fpath,svpath)
  assign(paste0(loca[j],"_",colnames(r1)[3]),r1)
  write.csv(r1,paste0(svpath,loca[j],"_",colnames(r1)[3],".csv"))
  r1 <- NULL
}
ggplot(r1,aes(x=date))+
  geom_line(aes(y=var_f),color="grey")+
  geom_line(aes(y=smo_sp),color="darkred")+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")
ggsave(paste0(svpath,loca,"_",substr(table_name[i],7,9),
              ".jpeg"),width=8,height=3,dpi=600)
#####################################################
ts_nd <- ts(Sen_rai$smo_sp,frequency =365,start = c(2003,1))
deco_nd = decompose(ts_nd, "additive")
plot(deco_nd)
r1 <- as.data.table(cbind(Sen_Tem$time,deco_nd$x,
                          deco_nd$seasonal,
                          deco_nd$trend,
                          deco_nd$random))
colnames(r1) <- c("date","org","seasonal","trend","reandom")
write.csv(r1,paste0(svpath,"ts_Sen_temp",".csv"))
library(ggplot2)
ggplot(r1,aes(x=as.Date(date)))+
  geom_line(aes(y=seasonal),color="grey")+
  geom_line(aes(y=trend),color="darkred")



