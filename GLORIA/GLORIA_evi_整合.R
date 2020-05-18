library(data.table)
library(imputeTS) #fix th na of EVI
library(readr) #change chr "1,000" to num "1000"
library(ggplot2)
library(magrittr)
l_path <- "C:/Users/user/Desktop/GLORIA_EVI/16d_EVI/"
s_path <- "C:/Users/user/Desktop/GLORIA_EVI/"
name <- list.files(l_path)
rog_n <- c("sen","sun","yat")

for (i in c(1,5,9)){
  r_aq <- fread(paste0(l_path,name[i]))
  colnames(r_aq) <- c("date","EVI")
  r_aq_aq <- fread(paste0(l_path,name[i+1]))
  colnames(r_aq_aq) <- c("date","AQ")
  r1 <- r_aq[r_aq_aq,on=.(date=date)][,date:=as.Date(date)]
  r_tr <- fread(paste0(l_path,name[i+2]))
  colnames(r_tr) <- c("date","EVI")
  r_tr_aq <- fread(paste0(l_path,name[i+3]))
  colnames(r_tr_aq) <- c("date","AQ")
  r2 <- r_tr[r_tr_aq,on=.(date=date)][,date:=as.Date(date)]
  r3 <- rbind(r1,r2)
  setorder(r3,"date")
  r3 <- r3[AQ<=2,][,EVI:=parse_number(EVI)/10000][,EVI:=na_kalman(EVI)]
  r3[,EVI.sm:=smooth.spline(r3$EVI)$y][,week :=week(date)]
  assign(paste0(i,"_EVI"),r3)
  write.csv(r3,paste(s_path,i,"_EVI.csv"))
}
cb1 <- merge(`1_EVI`,`5_EVI`,by="date",all=T)
result <- merge(cb1,`9_EVI`,by="date",all=T)
setnames(result,c("EVI.x","EVI.sm.x","EVI.y","EVI.sm.y","EVI","EVI.sm"),c("sen","sen.sm","sun","sun.sm","yat","yat.sm"))

result[,sen.sm:=na_kalman(sen.sm)][,sun.sm:=na_kalman(sun.sm)][,yat.sm:=na_kalman(yat.sm)]
ggplot(result,aes(x=date))+
  geom_line(aes(y=sen.sm,color="sen"))+
  geom_line(aes(y=sun.sm,color="sun"))+
  geom_line(aes(y=yat.sm,color="yat"))+
  scale_x_date(breaks="year",date_labels="%y")+
  xlab("Year")+
  ylab("EVI")
