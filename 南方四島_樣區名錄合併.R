library(data.table)
path <- "C:/Users/user/Desktop/海洋國家公園管理處107403_01(澎湖南方四島國家公園陸域資源調查評析案)/期中報告/"
p_name <- list.files(paste0(path,"export_data/rawdata"))
ts_2015 <- NULL
for (i in 1:length(p_name)){
  p_n <- p_name[i]
  tb <- fread(paste0(path,"export_data/rawdata/",p_n),sep ="|",encoding = 'UTF-8')
  tb[,plot_name:=gsub(".csv","",p_n)]
  ts_2015 <- rbind(ts_2015,tb,fill=T)
}                 
write.csv(ts_2015,paste0(path,"2019野外調查/2015_survey.csv"))

s_2019 <- fread(paste0(path,"2019野外調查/2019_survey.csv"),head=T)
s_2015 <- fread(paste0(path,"2019野外調查/2015_survey.csv"),head=T)
c_1519 <- merge(s_2015,s_2019,by=c("plot_name","學名","層次"),all=T)
write.csv(c_1519,paste0(path,"2019野外調查/1519_survey.csv"))
