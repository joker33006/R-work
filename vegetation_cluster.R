library(data.table)
require(vegan)

veg_b <- read.csv("E:/忍者/R-work/Git/R-work/test.csv",
                  encoding="unknown",header=T,row.names=1) 

v.dist <- (vegdist(veg_b,method="jaccard"))*100
v_clas <- hclust(v.dist, method="average")
si=70
cutree(v_clas, h = si)
rn <- row.names(veg_b)
veg_bc <- as.data.table(veg_b)
veg_bc[,cls:=cutree(v_clas, h = si)][,pn:=rn]

head(veg_bc)
plot(as.dendrogram(v_clas), edgePar=list(col=12, lwd=0.5),
     xlab="Similarity(%)", horiz=T,xlim=c(100,0), 
     axes=FALSE) 
axis(1, seq(0, 100, by=20), seq(100, 0, by=-20))
abline(v=si, col="red")
