library(data.table)
path <- "C:/Users/user/Desktop/海洋國家公園管理處107403_01(澎湖南方四島國家公園陸域資源調查評析案)/期中報告/過往名錄/"
n_2006 <- fread(paste0(path,"2006/2006_各島嶼名錄.csv"),encoding="UTF-8",head=T)
n_2008 <- fread(paste0(path,"2008/2008_各島嶼名錄.csv"),encoding="UTF-8",head=T)
n_2010 <- fread(paste0(path,"2010/2010_各島嶼名錄.csv"),encoding="UTF-8",head=T)
n_2015 <- fread(paste0(path,"2015/2015_各島嶼名錄.csv"),encoding="UTF-8",head=T)
n_2019 <- fread(paste0(path,"2019/2019_各島嶼名錄.csv"),encoding="UTF-8",head=T)

d_0608 <- merge(n_2006,n_2008,by='學名',all=T)
d_0610 <- merge(d_0608,n_2010,by='學名',all=T)

d_0615 <- merge(d_0610,n_2015,by='學名',all=T)
d_06_19 <- merge(d_0615,n_2019,by='學名',all=T)
write.csv(d_06_19,paste0(path,"2006-2019名錄統整.csv"))
