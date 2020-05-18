require(xlsx)
library(readxl)
library(data.table)
read_excel_allsheets <- function(filename) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  names(x) <- sheets
  x
}
path.name <- "E:/忍者/GLORIA/2019/2019資料輸入/1X1統整/" #需要合併工作表的資料夾
AA=list.files(path =path.name) 
sr <- NULL
for (j in c(1:3)){
  AA_N <- paste0(path.name,AA[j]) 
  ts <- read_excel_allsheets(AA_N)  

    for (i in c(1:16)){
    s1 <- as.data.table(ts[i])
    names(s1)[2] <-"spname"
    names(s1)[5] <-"cover"
    s2 <- s1[grepl('[0-9]',cover)][grepl('\\w',spname)][,name:=names(ts[i])][,area:=gsub(".xlsx","",AA[j])]
      if(!is.null(sr)){  #To turn off the first loop
      s2 <- setNames(s2, names(sr))  #rename the s2 by sr, because different colname can't be combined.
      }
    sr <- rbind(sr,s2)
    }
}
class(sr)
write.csv(sr,paste0(path.name,"/",AA[j],"統整.csv")) 
area_name
