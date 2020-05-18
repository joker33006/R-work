require(xlsx)
library(readxl)
library(data.table)
read_excel_allsheets <- function(filename) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  names(x) <- sheets
  x
}
path.name <- "E:/忍者/GLORIA/2019/2019資料輸入/10X10/" #需要合併工作表的資料夾
AA=list.files(path =path.name) 
sr <- NULL
for (j in c(1:3)){
  AA_N <- paste0(path.name,AA[j]) 
  ts <- read_excel_allsheets(AA_N)  
  
  for (i in c(1:4)){
     s1 <- as.data.table(ts[i])
   s2 <- s1[3:nrow(s1[,1])][-31,]
   s2 <- na.omit(s2, cols=1)
   s2[,area:=names(ts[i])][,mon:=substring(AA[j],1,3)]

    sr <- rbind(sr,s2,use.names=FALSE,fill=FALSE)
  }
}
    
class(sr)
write.csv(sr,paste0(path.name,"/10X10統整.csv")) 
