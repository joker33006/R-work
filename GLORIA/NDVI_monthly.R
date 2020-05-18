library(data.table)
library(magrittr)
path <- "C:/Users/user/Desktop/GLORIA_EVI/plot/"
fi_n <- list.files(paste0(path,"/Ts_NDV/"))
i=1
d1 <- fread(paste0(path,"/Ts_NDV/",fi_n[1]))
d1[,date2:=as.Date(date)]
d1 %>% mutate(y_m=format(date2,"%Y%m"),
              year=format(date2,"%Y")) %>%
  group_by(y_m) %>%
  summarise(NDV_avg=mean(org)) %>% as.data.table() -> Sen_m_ndv
Sen_rai %>%
  mutate(y_m2 = format(date, "%Y%m"), year = format(date, "%Y")) %>%
  group_by(y_m2) %>%
  summarise(rain_sum = sum(rai))%>% as.data.table() ->Sen_m_rai
class(Sen_m_rai)
r1 <- Sen_m_ndv[Sen_m_rai,on=.(y_m=y_m2)]
plot(r1$rain_sum,r1$NDV_avg)
