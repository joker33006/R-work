library(data.table)

d1 <- fread("E:/忍者/發表中/博論發表_climate factor/YAC_UselessFig1.csv")
d_fig <- fread("E:/忍者/發表中/博論發表_climate factor/YAC_Ttol_fig_avg.csv")
d_leaf <- fread("E:/忍者/發表中/博論發表_climate factor/YAC_Ttol_Leaf_avg.csv")
climate <- fread("E:/忍者/發表中/博論發表_climate factor/YAC_climate_data.csv")
S_Basic <- fread("E:/忍者/發表中/博論發表_climate factor/YAC_envir_base.csv")
d2 <- d1[,lapply(.SD,sum), by=TN][,BN:=NULL]
df <- melt(d_fig,id.vars="TN",measure.vars=c(2:178),
           variable.name = "date",value.name="avgf")
dL <- melt(d_leaf,id.var="TN",measure.vars=c(2:178),
           variable.name = "date",value.name="avgL")
d2.fig_ab <- melt(d2,id.vars="TN",measure.vars=c(2:178),
                  variable.name = "date",value.name="avg_nAB")
climate[,Date_2:=as.factor(Date)]
df_2 <- df[climate,on=.(date=Date_2)][S_Basic,on=.(TN=sn)]


write.csv(d2,"E:/忍者/發表中/博論發表_climate factor/YAC_無效果1.csv")
