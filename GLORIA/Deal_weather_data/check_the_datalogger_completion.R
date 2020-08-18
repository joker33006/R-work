library(data.table)
library(ggplot2)
td <- fread('C:/Users/user/Desktop/temp_data.csv')
ggplot(data = td,aes(date,y=dir,group=group))+
  geom_line(size=2)+
  scale_y_discrete(limits=c("W","S","E",'N'))+
  facet_grid(Summit ~ .)+
  #scale_color_brewer(palette="")+
  theme_bw()+
  labs(x="Year",y='Direction')+
  theme(legend.position="none")+
  scale_x_date(breaks="2 year",date_labels="%Y")
  

ggsave(paste0('E:/忍者/GLORIA_個人處理/2020/Temp_20200809/SYU/plot/datalogger_data_completion_boring.jpeg'),
       width=4,height=3,dpi=300)
  