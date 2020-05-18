library(data.table)
fpath <-"C:/Users/user/Desktop/GLORIA_EVI"
file_name <- list.files(paste0(fpath,"/rawdata"))
for (i in c(1:length(file_name))){
    result <- NULL 
    rdata_path <- paste0(fpath,"/rawdata/",file_name[i])
    data_name <- list.files(rdata_path)
    for (j in c(1:length(data_name))){
       rd <- fread(paste0(rdata_path,"/",data_name[j]))
       rd[,area:=substr(data_name[j],start=1,stop=3)]
       result <- rbind(result,rd)
    }
  write.csv(result,paste0(fpath,"/result/total_",file_name[i],".csv")) 
}
