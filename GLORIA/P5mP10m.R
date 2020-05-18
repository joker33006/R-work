require(xlsx)
library(readxl)
library(data.table)
read_excel_allsheets <- function(filename) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X,col_names = FALSE))
  names(x) <- sheets
  x
}

path.name <- "E:/忍者/GLORIA/2019/2019資料輸入/p5m10m/" #需要合併工作表的資料夾
AA=list.files(path =path.name) 

 for (j in c(1:3)){
 
 AA_N <- paste0(path.name,AA[j]) 
  ts <- read_excel_allsheets(AA_N)  
  
  for (i in c(1:8)){
   s1 <- as.data.table(ts[i])
   s2 <- s1[8:max(row(s1[,1])),1:2]
  colnames(s2) <- c("species_name","abundance")
   s2[,area:=names(ts[i])][,mon:=substring(AA[j],1,3)]
    sr <- rbind(sr,s2)
  }
}   
class(sr)
write.csv(sr,paste0(path.name,"/p5mp10m統整.csv")) 

################ 屬性表合成
tr <- NULL 
for (j in c(1:3)){ #deal with plant abundance
  
  AA_N <- paste0(path.name,AA[j]) 
  ts <- read_excel_allsheets(AA_N)  
  
  for (i in c(1:8)){
    
    s1 <- as.data.table(ts[i])
    s2 <- s1[1:7,4:5]
    colnames(s2) <- c("attribute","cover")
    s2[,area:=names(ts[i])][,mon:=substring(AA[j],1,3)]
    tr <- rbind(tr,s2)
  }
}
write.csv(tr,paste0(path.name,"/p5mp10m_屬性統整.csv")) 
