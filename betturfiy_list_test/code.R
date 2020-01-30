library(data.table)
path <- "E:/忍者/R-work/Git/R-work/蝴蝶名錄產生試作/"
namelist <- fread(paste0(path,"database/台灣產蝴蝶名錄_蝴蝶保育協會2014.csv"),encoding = "UTF-8")
surv_list <- fread(paste0(path,"Survey_list/sp_list.csv"),header = FALSE)
namelist[,order:=seq(1:nrow(namelist))]
miss_value <- NULL
bs_list <- NULL
for (i in c(1:nrow(surv_list))){
    name <- surv_list[i]
    nr_1 <- namelist[CN_1==name]
    nr_2 <- namelist[CN_2==name]
    nr <- rbind(nr_1,nr_2)
        if (nrow(nr)==0){
          miss_value <- rbind(miss_value,name)      
          }else{
          bs_list <- rbind(bs_list,nr[1])  
          }
    }
tb <- unique(bs_list[order(order)])
tb[,cbname:=paste0("*",Sci_name_simp,"*"," ",author," ",CN_1," ",CN_2)]
list <- NULL
fam_oName <- 0
j=1
for (i in c(1:nrow(tb))){
  fam_name <- tb[i,family]
  sci_name <- tb[i,cbname]
      if(fam_oName==fam_name){
        list <- rbind(list,paste0("\t",i,". ",sci_name))
      }else{
        fam_oName <- fam_name
        list <- rbind(list,paste0(j,". ",fam_name))
        list <- rbind(list,paste0("\t",i,". ",sci_name))
        j=j+1
        }
    }
write.table(list, file = paste0(path,"/result/list.txt"), 
                                quote = FALSE,col.names = F,row.names = F)    
            
