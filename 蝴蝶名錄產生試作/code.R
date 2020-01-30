require(data.table)
path <- "C:/Users/user/Desktop/betturfiy_list_test/"
namelist <- fread(paste0(path,"database/butterflylistinTW_2014.csv"),encoding = "UTF-8")
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
fam_number <- length(unique(tb[,family]))
genus_number <- length(unique(tb[,Genus]))
sp_number <- nrow(tb)
endemic_number <- nrow(tb[SP=="特有種",])
fam_oName <- 0 #family name in previous record
j=1
for (i in c(1:nrow(tb))){
  fam_name <- tb[i,family]
  sci_name <- tb[i,cbname]
  if(tb[i,SP]=="特有種"){sci_name <- paste(sci_name,"@")}
      if(fam_oName==fam_name){
        list <- paste0(list,"  \n","\t",i,". ",sci_name)
      }else{
        fam_oName <- fam_name
        list <- paste0(list,"  \n",j,". ",fam_name,"")
        list <- paste0(list,"  \n","\t",i,". ",sci_name)
        j=j+1
        }
    }
cat(list)
```