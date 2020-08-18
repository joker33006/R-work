library(data.table)
library(ggplot2)
library(ggpubr)
path <- "E:/忍者/GLORIA_個人處理/2019/2019資料輸入/資料分析/"

rd_set <- fread(paste0(path,'Cover_integrate_sect.csv'))
rd_list <- fread(paste0(path,'name_database.csv'))
#rd_set <- fread(paste0(path,'Cover_integrate_1X1.csv'))
bio_1 <- fread(paste0(path,'bio_1_qun.csv'))
bio_6<- fread(paste0(path,'bio_6_qun.csv'))
bio_12 <- fread(paste0(path,'bio_12_qun.csv'))
bio_wi <- (paste0(path,'WI_qun.csv'))
base <- rd_set[bio_1,on=.(Code=code)]
base <- base[rd_list,on=.(Code=code)]
base <- base[dir!="NA"]

base[,d0813:=`2013`-`2008`][,d1319:=`2019`-`2013`][,d0819:=`2019`-`2008`]
base[,rd0813:=(`2013`-`2008`)/(`2008`+1)][,rd1319:=(`2019`-`2013`)/(`2013`+1)][,rd0819:=(`2019`-`2008`)/(`2013`+1)]
setnames(base,c('2008','2013','2019'),c('c2008','c2013','c2019'))
write.csv(base,paste0(path,"cover_change_sect.csv"))
#write.csv(base,paste0(path,"cover_change_1X1.csv"))
base <- base[order(Summit,dir)]
base_shurb <- base[!("木"%in%life_form)]
plot_d <- function(summit,year){
  data <- base[Summit==summit][,bio1:=bio1_3/10]
p <- ggplot(data=data,aes_string('bio1',year))+
  scale_color_brewer(palette="Set2")+
  xlim(0, 18)+
  labs(x="AAT(°C)",y=year)+
  geom_density_2d_filled()
return(p)
}
y <- c('2008','2013')
sun_2008 <- plot_d('SEN','c2008')
sun_2013 <- plot_d('SEN','c2013')
sun_2019 <- plot_d('SEN','c2019')

ggarrange(sun_2008,sun_2013,sun_2019, nrow = 1)
ggsave(paste0(path,'/plot/sen.jpeg'),dpi=600,width =14,height = 5)
