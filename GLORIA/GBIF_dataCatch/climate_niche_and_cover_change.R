library(data.table)
library(ggplot2)
path <- "E:/忍者/GLORIA_個人處理/2019/2019資料輸入/資料分析/"
rd_set <- fread(paste0(path,'Cover_integrate_sect.csv'))
#rd_set <- fread(paste0(path,'Cover_integrate_1X1.csv'))
bio_1 <- fread(paste0(path,'bio_1_qun.csv'))
bio_6<- fread(paste0(path,'bio_6_qun.csv'))
base <- rd_set[bio_1,on=.(Code=code)]

base <- base[dir!="NA"]
base[,d0813:=`2013`-`2008`][,d1319:=`2019`-`2013`][,d0819:=`2019`-`2008`]
base[,rd0813:=(`2013`-`2008`)/(`2008`+1)][,rd1319:=(`2019`-`2013`)/(`2013`+1)][,rd0819:=(`2019`-`2008`)/(`2013`+1)]
write.csv(base,paste0(path,"cover_change_sect.csv"))
base <- base[order(Summit,dir)]
base <- base[!("木"%in%life_form)]
ggplot(data=base[Summit=='SEN',],aes(bio1_3/10,`2019`,color=dir))+
  geom_point()+ 
  scale_color_brewer(palette="Set2")+
  theme_classic()+
  labs(x="AAT(°C)",y="Abundence")

  
